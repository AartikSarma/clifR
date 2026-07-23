#' Self-contained SOFA score computation from raw CLIF tables
#'
#' Port of `clifpy/utils/sofa_polars.py` (`compute_sofa_polars`). Unlike
#' [compute_sofa()], this implementation does not need a wide dataset: it loads
#' the five source tables itself, does its own medication unit conversion and
#' respiratory-support waterfall, and reduces everything to one worst-value row
#' per ID.
#'
#' clifpy's version is a polars pipeline of elementwise expressions rather than a
#' SQL pipeline, so this port follows it with dplyr, preserving the order of
#' operations and polars' three-valued (null-propagating) comparison semantics.
#'
#' @name clif-sofa-direct
NULL

#' Lab categories required by [compute_sofa_direct()]
#' @keywords internal
REQUIRED_LABS <- c("creatinine", "platelet_count", "po2_arterial", "bilirubin_total")

#' Vital categories required by [compute_sofa_direct()]
#' @keywords internal
REQUIRED_VITALS <- c("map", "spo2", "weight_kg")

#' Assessment categories required by [compute_sofa_direct()]
#' @keywords internal
REQUIRED_ASSESSMENTS <- c("gcs_total")

#' Medication categories required by [compute_sofa_direct()]
#' @keywords internal
REQUIRED_MEDS <- c("norepinephrine", "epinephrine", "dopamine", "dobutamine")

#' Respiratory support columns required by [compute_sofa_direct()]
#' @keywords internal
REQUIRED_RESP_SUPPORT_COLS <- c("device_category", "mode_category", "fio2_set")

#' Dose-unit spelling variants collapsed before unit conversion
#'
#' Named character vector of `replacement = pattern`, applied in order. clifpy
#' keeps a second copy of `UNIT_NAMING_VARIANTS` inside `sofa_polars.py`; the
#' name is qualified here so it cannot shadow the unit converter's copy.
#' @keywords internal
SOFA_DIRECT_UNIT_VARIANTS <- c(
  "/hr" = "/h(r|our)?$",
  "/min" = "/m(in|inute)?$",
  "u" = "u(nits|nit)?",
  "m" = "milli-?",
  "l" = "l(iters|itres|itre|iter)?",
  "mcg" = "^(u|µ|μ)g",
  "g" = "^g(rams|ram)?"
)

#' Test a condition with polars/SQL null semantics
#'
#' `NA` conditions are false, so a `when(...)` branch with a missing input falls
#' through to the next branch rather than being taken.
#'
#' @param condition Logical vector, possibly containing `NA`.
#' @return Logical vector with `NA` replaced by `FALSE`.
#' @keywords internal
is_true <- function(condition) {
  !is.na(condition) & condition
}

#' Minimum ignoring missing values, returning `NA` for an all-missing input
#' @param values Numeric vector.
#' @return Scalar minimum, or `NA_real_`.
#' @keywords internal
min_or_na <- function(values) {
  if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
}

#' Maximum ignoring missing values, returning `NA` for an all-missing input
#' @param values Numeric vector.
#' @return Scalar maximum, or `NA_real_`.
#' @keywords internal
max_or_na <- function(values) {
  if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
}

#' Relabel datetime columns into the requested timezone
#'
#' Naive timestamps are read as UTC instants (as pandas does with
#' `tz_localize('UTC')`), then relabelled to `timezone`; the underlying instants
#' never move.
#'
#' @param data A data frame.
#' @param timezone Olson timezone name, or `NULL` to leave the data alone.
#' @param datetime_columns Columns to relabel.
#' @return The data frame with relabelled datetime columns.
#' @keywords internal
standardize_datetime_columns <- function(data, timezone, datetime_columns) {
  if (is.null(timezone)) {
    return(data)
  }
  for (column_name in intersect(datetime_columns, names(data))) {
    column_values <- data[[column_name]]
    if (inherits(column_values, "POSIXct")) {
      attr(column_values, "tzone") <- timezone
      data[[column_name]] <- column_values
    }
  }
  data
}

#' Read a CLIF table from a data directory
#'
#' @param data_directory Directory holding `clif_<table_name>.<filetype>`.
#' @param filetype `"parquet"` or `"csv"`.
#' @param table_name snake_case table name.
#' @param columns Columns to read.
#' @return A tibble, or `NULL` when the file does not exist.
#' @keywords internal
read_clif_table <- function(data_directory, filetype, table_name, columns = NULL) {
  file_path <- file.path(data_directory, paste0("clif_", table_name, ".", filetype))
  if (!file.exists(file_path)) {
    cli::cli_alert_warning("{.file {file_path}} not found")
    return(NULL)
  }
  table_data <- if (identical(filetype, "parquet")) {
    if (is.null(columns)) {
      arrow::read_parquet(file_path)
    } else {
      arrow::read_parquet(file_path, col_select = dplyr::all_of(columns))
    }
  } else {
    if (is.null(columns)) {
      readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE)
    } else {
      readr::read_csv(
        file_path,
        col_select = dplyr::all_of(columns), show_col_types = FALSE, progress = FALSE
      )
    }
  }
  dplyr::as_tibble(table_data)
}

#' Backward as-of join
#'
#' For every left row, take the most recent right row in the same `by` group at
#' or before the left timestamp. Equivalent to polars'
#' `join_asof(strategy = "backward")`.
#'
#' @param left_data,right_data Data frames.
#' @param left_time,right_time Timestamp column names.
#' @param by Character vector of grouping columns.
#' @param value_columns Columns of `right_data` to attach.
#' @param tolerance_minutes Optional maximum gap; matches further back than this
#'   are dropped.
#' @return `left_data` with `value_columns` attached, `NA` where unmatched.
#' @keywords internal
asof_backward_join <- function(left_data, right_data, left_time, right_time, by,
                               value_columns, tolerance_minutes = NULL) {
  matched_indices <- rep(NA_integer_, nrow(left_data))

  if (nrow(right_data) > 0 && nrow(left_data) > 0) {
    right_data <- right_data[
      do.call(order, c(unname(as.list(right_data[by])), list(right_data[[right_time]]))),
      ,
      drop = FALSE
    ]
    left_key <- do.call(paste, c(unname(as.list(left_data[by])), list(sep = "\r")))
    right_key <- do.call(paste, c(unname(as.list(right_data[by])), list(sep = "\r")))
    right_rows_by_key <- split(seq_len(nrow(right_data)), right_key)
    left_rows_by_key <- split(seq_len(nrow(left_data)), left_key)

    left_times <- as.numeric(left_data[[left_time]])
    right_times <- as.numeric(right_data[[right_time]])

    for (group_key in names(left_rows_by_key)) {
      right_rows <- right_rows_by_key[[group_key]]
      if (is.null(right_rows)) next
      left_rows <- left_rows_by_key[[group_key]]
      position <- findInterval(left_times[left_rows], right_times[right_rows])
      is_matched <- position > 0
      matched_indices[left_rows[is_matched]] <- right_rows[position[is_matched]]
    }

    if (!is.null(tolerance_minutes)) {
      time_gap <- left_times - right_times[matched_indices]
      matched_indices[!is.na(matched_indices) & time_gap > tolerance_minutes * 60] <- NA_integer_
    }
  }

  for (column_name in value_columns) {
    left_data[[column_name]] <- right_data[[column_name]][matched_indices]
  }
  left_data
}

#' Create respiratory support episode identifiers
#'
#' Applies the waterfall heuristics (IMV/NIPPV detection from `mode_category`,
#' room-air FiO2 default, optional nasal-cannula LPM to FiO2 mapping), forward
#' fills the device and mode categories, and numbers the resulting episodes.
#'
#' @param resp_data Respiratory support rows.
#' @param id_col Grouping column.
#' @return `resp_data` with `device_cat_id` and `mode_cat_id` added.
#' @keywords internal
create_resp_support_episodes <- function(resp_data, id_col = "hospitalization_id") {
  resp_data <- resp_data[order(resp_data[[id_col]], resp_data[["recorded_dttm"]]), , drop = FALSE]

  # Heuristic 1: IMV detection from mode_category
  is_imv_mode <- is.na(resp_data$device_category) &
    !is.na(resp_data$mode_category) &
    grepl(
      "(?:assist control-volume control|simv|pressure control)",
      tolower(resp_data$mode_category)
    )
  resp_data$device_category[is_imv_mode] <- "IMV"

  # Heuristic 2: NIPPV detection from mode_category
  is_nippv_mode <- is.na(resp_data$device_category) &
    !is.na(resp_data$mode_category) &
    grepl("pressure support", tolower(resp_data$mode_category)) &
    !grepl("cpap", tolower(resp_data$mode_category))
  resp_data$device_category[is_nippv_mode] <- "NIPPV"

  # Heuristic 3: room air FiO2 default
  is_room_air_without_fio2 <- is_true(tolower(resp_data$device_category) == "room air") &
    is.na(resp_data$fio2_set)
  resp_data$fio2_set[is_room_air_without_fio2] <- 0.21

  # Heuristic 4: FiO2 imputation from nasal cannula flow. clifpy drops lpm_set
  # from the frame before reaching this branch, so it is normally inactive.
  if ("lpm_set" %in% names(resp_data)) {
    lpm_rounded <- as.integer(round(resp_data$lpm_set, 0))
    fio2_from_lpm <- c(0.24, 0.28, 0.32, 0.36, 0.40, 0.44, 0.48, 0.52, 0.56, 0.60)
    is_nasal_cannula_without_fio2 <- is_true(tolower(resp_data$device_category) == "nasal cannula") &
      is.na(resp_data$fio2_set) &
      !is.na(resp_data$lpm_set) &
      is_true(lpm_rounded >= 1) &
      is_true(lpm_rounded <= 10)
    resp_data$fio2_set[is_nasal_cannula_without_fio2] <-
      fio2_from_lpm[lpm_rounded[is_nasal_cannula_without_fio2]]
  }

  # Forward fill device and mode categories within each ID
  resp_data <- resp_data |>
    dplyr::group_by(.data[[id_col]]) |>
    tidyr::fill(dplyr::all_of(c("device_category", "mode_category")), .direction = "down") |>
    dplyr::ungroup()

  identifier_values <- resp_data[[id_col]]
  previous_identifier <- dplyr::lag(identifier_values)

  device_changed <- (resp_data$device_category !=
    stats::ave(resp_data$device_category, identifier_values, FUN = dplyr::lag)) |
    (identifier_values != previous_identifier)
  resp_data$device_cat_id <- stats::ave(
    as.integer(is_true(device_changed)), identifier_values, FUN = cumsum
  )

  mode_changed <- (resp_data$mode_category !=
    stats::ave(resp_data$mode_category, identifier_values, FUN = dplyr::lag)) |
    (resp_data$device_cat_id !=
      stats::ave(resp_data$device_cat_id, identifier_values, FUN = dplyr::lag)) |
    (identifier_values != previous_identifier)
  resp_data$mode_cat_id <- stats::ave(
    as.integer(is_true(mode_changed)), identifier_values, FUN = cumsum
  )

  resp_data
}

#' Load labs for direct SOFA computation
#' @inheritParams compute_sofa_direct
#' @param hospitalization_ids Hospitalizations to keep.
#' @param cohort_local Cohort with standardized timestamps.
#' @return A tibble of labs restricted to the cohort windows.
#' @keywords internal
load_labs_direct <- function(data_directory, filetype, hospitalization_ids, cohort_local, timezone) {
  labs_data <- read_clif_table(
    data_directory, filetype, "labs",
    c("hospitalization_id", "lab_result_dttm", "lab_category", "lab_value", "lab_value_numeric")
  )
  identifier_columns <- setdiff(names(cohort_local), c("start_dttm", "end_dttm"))
  if (is.null(labs_data)) {
    return(NULL)
  }

  labs_data |>
    dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id)) |>
    dplyr::filter(
      .data$lab_category %in% REQUIRED_LABS,
      .data$hospitalization_id %in% hospitalization_ids
    ) |>
    standardize_datetime_columns(timezone, "lab_result_dttm") |>
    dplyr::inner_join(cohort_local, by = "hospitalization_id") |>
    dplyr::filter(
      .data$lab_result_dttm >= .data$start_dttm,
      .data$lab_result_dttm <= .data$end_dttm
    ) |>
    dplyr::select(dplyr::all_of(c(
      identifier_columns, "lab_result_dttm", "lab_category", "lab_value_numeric"
    )))
}

#' Load vitals for direct SOFA computation
#' @inheritParams load_labs_direct
#' @return A tibble of vitals restricted to the cohort windows.
#' @keywords internal
load_vitals_direct <- function(data_directory, filetype, hospitalization_ids, cohort_local, timezone) {
  vitals_data <- read_clif_table(
    data_directory, filetype, "vitals",
    c("hospitalization_id", "recorded_dttm", "vital_category", "vital_value")
  )
  identifier_columns <- setdiff(names(cohort_local), c("start_dttm", "end_dttm"))
  if (is.null(vitals_data)) {
    return(NULL)
  }

  vitals_data |>
    dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id)) |>
    dplyr::filter(
      .data$vital_category %in% REQUIRED_VITALS,
      .data$hospitalization_id %in% hospitalization_ids
    ) |>
    standardize_datetime_columns(timezone, "recorded_dttm") |>
    dplyr::inner_join(cohort_local, by = "hospitalization_id") |>
    dplyr::filter(
      .data$recorded_dttm >= .data$start_dttm,
      .data$recorded_dttm <= .data$end_dttm
    ) |>
    dplyr::select(dplyr::all_of(c(
      identifier_columns, "recorded_dttm", "vital_category", "vital_value"
    )))
}

#' Load patient assessments for direct SOFA computation
#'
#' Numeric and categorical values are coalesced into a single `assessment_value`.
#'
#' @inheritParams load_labs_direct
#' @return A tibble of assessments restricted to the cohort windows.
#' @keywords internal
load_patient_assessments_direct <- function(data_directory, filetype, hospitalization_ids,
                                            cohort_local, timezone) {
  assessments_data <- read_clif_table(
    data_directory, filetype, "patient_assessments",
    c(
      "hospitalization_id", "recorded_dttm", "assessment_category",
      "numerical_value", "categorical_value"
    )
  )
  identifier_columns <- setdiff(names(cohort_local), c("start_dttm", "end_dttm"))
  if (is.null(assessments_data)) {
    return(NULL)
  }

  assessments_data |>
    dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id)) |>
    dplyr::filter(
      .data$assessment_category %in% REQUIRED_ASSESSMENTS,
      .data$hospitalization_id %in% hospitalization_ids
    ) |>
    standardize_datetime_columns(timezone, "recorded_dttm") |>
    dplyr::inner_join(cohort_local, by = "hospitalization_id") |>
    dplyr::filter(
      .data$recorded_dttm >= .data$start_dttm,
      .data$recorded_dttm <= .data$end_dttm
    ) |>
    dplyr::mutate(
      assessment_value = dplyr::coalesce(
        as.numeric(.data$numerical_value),
        suppressWarnings(as.numeric(.data$categorical_value))
      )
    ) |>
    dplyr::select(dplyr::all_of(c(
      identifier_columns, "recorded_dttm", "assessment_category", "assessment_value"
    )))
}

#' Load respiratory support for direct SOFA computation
#'
#' Rows are pulled with a lookback window so that the device, mode and FiO2 in
#' effect at the start of the SOFA window can be carried forward, then filtered
#' back to the SOFA window itself.
#'
#' @inheritParams load_labs_direct
#' @param lookback_hours Hours of history to pull before each window start.
#' @return A tibble of respiratory support rows with `device_rank`.
#' @keywords internal
load_respiratory_support_direct <- function(data_directory, filetype, hospitalization_ids,
                                            cohort_local, lookback_hours = 24, timezone = NULL) {
  respiratory_data <- read_clif_table(
    data_directory, filetype, "respiratory_support",
    c(
      "hospitalization_id", "recorded_dttm", "device_category", "mode_category",
      "fio2_set", "lpm_set", "tidal_volume_set", "resp_rate_set"
    )
  )
  identifier_columns <- setdiff(names(cohort_local), c("start_dttm", "end_dttm"))
  if (is.null(respiratory_data)) {
    return(NULL)
  }

  cohort_with_lookback <- cohort_local |>
    dplyr::mutate(
      start_dttm_lookback = .data$start_dttm - lookback_hours * 3600,
      end_dttm_original = .data$end_dttm
    )

  respiratory_data <- respiratory_data |>
    dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id)) |>
    dplyr::filter(.data$hospitalization_id %in% hospitalization_ids) |>
    standardize_datetime_columns(timezone, "recorded_dttm") |>
    dplyr::inner_join(cohort_with_lookback, by = "hospitalization_id") |>
    dplyr::filter(
      .data$recorded_dttm >= .data$start_dttm_lookback,
      .data$recorded_dttm <= .data$end_dttm_original
    ) |>
    # clifpy narrows to these columns before its `lpm_set` check, so lpm_set is
    # dropped here too and the LPM heuristic stays inactive.
    dplyr::select(dplyr::all_of(c(
      identifier_columns, "recorded_dttm", "device_category", "mode_category",
      "fio2_set", "start_dttm", "end_dttm"
    )))

  respiratory_data <- create_resp_support_episodes(respiratory_data, id_col = "hospitalization_id")

  respiratory_data |>
    dplyr::arrange(.data$hospitalization_id, .data$recorded_dttm) |>
    dplyr::group_by(.data$hospitalization_id, .data$mode_cat_id) |>
    tidyr::fill("fio2_set", .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data$recorded_dttm >= .data$start_dttm,
      .data$recorded_dttm <= .data$end_dttm
    ) |>
    dplyr::select(-dplyr::all_of(c("start_dttm", "end_dttm"))) |>
    dplyr::mutate(
      device_rank = dplyr::coalesce(
        unname(DEVICE_RANK_DICT[.data$device_category]), 9L
      )
    )
}

#' Normalize a medication dose unit string
#'
#' @param dose_units Character vector of raw unit strings.
#' @return Cleaned, lower-case unit strings.
#' @keywords internal
clean_dose_unit <- function(dose_units) {
  cleaned_units <- tolower(gsub("\\s+", "", dose_units))
  for (replacement in names(SOFA_DIRECT_UNIT_VARIANTS)) {
    cleaned_units <- gsub(SOFA_DIRECT_UNIT_VARIANTS[[replacement]], replacement, cleaned_units)
  }
  cleaned_units
}

#' Load continuous medications and convert doses to mcg/kg/min
#'
#' Weights come from the vitals table via a backward as-of join on the
#' administration time, matching clifpy.
#'
#' @inheritParams load_labs_direct
#' @return A tibble with `dose_mcg_kg_min` per administration.
#' @keywords internal
load_and_convert_medications_direct <- function(data_directory, filetype, hospitalization_ids,
                                                cohort_local, timezone) {
  medications_data <- read_clif_table(
    data_directory, filetype, "medication_admin_continuous",
    c("hospitalization_id", "admin_dttm", "med_category", "med_dose", "med_dose_unit")
  )
  identifier_columns <- setdiff(names(cohort_local), c("start_dttm", "end_dttm"))
  if (is.null(medications_data)) {
    return(NULL)
  }

  medications_data <- medications_data |>
    dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id)) |>
    dplyr::filter(
      .data$med_category %in% REQUIRED_MEDS,
      .data$hospitalization_id %in% hospitalization_ids
    ) |>
    standardize_datetime_columns(timezone, "admin_dttm") |>
    dplyr::inner_join(cohort_local, by = "hospitalization_id") |>
    dplyr::filter(
      .data$admin_dttm >= .data$start_dttm,
      .data$admin_dttm <= .data$end_dttm
    ) |>
    dplyr::mutate(dose_unit_clean = clean_dose_unit(.data$med_dose_unit))

  weight_data <- read_clif_table(data_directory, filetype, "vitals")
  if (is.null(weight_data)) {
    weight_data <- tibble::tibble(
      hospitalization_id = character(0),
      recorded_dttm = as.POSIXct(character(0)),
      weight_kg = numeric(0)
    )
  } else {
    weight_data <- weight_data |>
      dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id)) |>
      dplyr::filter(
        .data$hospitalization_id %in% hospitalization_ids,
        .data$vital_category == "weight_kg"
      ) |>
      dplyr::select(dplyr::all_of(c("hospitalization_id", "recorded_dttm", "vital_value"))) |>
      dplyr::rename(weight_kg = "vital_value") |>
      standardize_datetime_columns(timezone, "recorded_dttm")
  }

  medications_data <- medications_data |>
    dplyr::arrange(.data$hospitalization_id, .data$admin_dttm)
  weight_data <- weight_data |>
    dplyr::arrange(.data$hospitalization_id, .data$recorded_dttm)

  medications_data <- asof_backward_join(
    medications_data, weight_data,
    left_time = "admin_dttm", right_time = "recorded_dttm",
    by = "hospitalization_id", value_columns = "weight_kg"
  )

  medications_data |>
    dplyr::mutate(
      dose_converted = dplyr::case_when(
        is_true(grepl("^mg", .data$dose_unit_clean)) ~ .data$med_dose * 1000,
        is_true(grepl("^g/", .data$dose_unit_clean)) ~ .data$med_dose * 1000000,
        is_true(grepl("^ng", .data$dose_unit_clean)) ~ .data$med_dose / 1000,
        .default = .data$med_dose
      ),
      dose_converted = dplyr::if_else(
        is_true(grepl("/hr$", .data$dose_unit_clean)),
        .data$dose_converted / 60,
        .data$dose_converted
      ),
      dose_mcg_kg_min = dplyr::case_when(
        is_true(grepl("/kg", .data$dose_unit_clean)) ~ .data$dose_converted,
        is_true(grepl("/lb", .data$dose_unit_clean)) ~ .data$dose_converted * 2.20462,
        .default = .data$dose_converted / .data$weight_kg
      )
    ) |>
    dplyr::select(dplyr::all_of(c(
      identifier_columns, "admin_dttm", "med_category", "dose_mcg_kg_min"
    )))
}

#' Impute PaO2 from SpO2 (elementwise Severinghaus form)
#'
#' Mirrors the arithmetic of clifpy's polars implementation, which differs
#' syntactically (but not algebraically) from the DuckDB form used by
#' [compute_sofa()].
#'
#' @param data Data frame with an `spo2` column.
#' @return The data frame with `pao2_imputed` added.
#' @keywords internal
impute_pao2_from_spo2_direct <- function(data) {
  severinghaus_a <- 11700.0 / ((100.0 / data$spo2) - 1)
  severinghaus_b <- (severinghaus_a^2 + 50^3)^0.5
  data$pao2_imputed <- dplyr::if_else(
    is_true(data$spo2 < 97),
    (severinghaus_b + severinghaus_a)^(1.0 / 3.0) - (severinghaus_b - severinghaus_a)^(1.0 / 3.0),
    NA_real_
  )
  data
}

#' Calculate P/F ratios from concurrent PO2 and FiO2 measurements
#'
#' The SOFA-97 specification requires the PO2 and the FiO2 to be measured at
#' about the same time, so each arterial PO2 is matched to the most recent
#' respiratory support row within `time_tolerance_minutes`.
#'
#' @param labs_with_po2 Rows with a `po2_arterial` value and `lab_result_dttm`.
#' @param respiratory_data Respiratory support rows with `fio2_set`.
#' @param time_tolerance_minutes Maximum lookback for the match.
#' @param identifier_columns Grouping columns for the match.
#' @return A tibble of matched rows with a non-missing `concurrent_pf`.
#' @keywords internal
calculate_concurrent_pf_ratios <- function(labs_with_po2, respiratory_data,
                                           time_tolerance_minutes = 240,
                                           identifier_columns = "hospitalization_id") {
  po2_data <- labs_with_po2 |>
    dplyr::filter(!is.na(.data$po2_arterial)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(identifier_columns, "lab_result_dttm"))))

  respiratory_for_join <- respiratory_data |>
    dplyr::select(dplyr::all_of(c(
      identifier_columns, "recorded_dttm", "fio2_set", "device_category"
    ))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(identifier_columns, "recorded_dttm"))))

  po2_with_fio2 <- asof_backward_join(
    po2_data, respiratory_for_join,
    left_time = "lab_result_dttm", right_time = "recorded_dttm",
    by = identifier_columns,
    value_columns = c("fio2_set", "device_category"),
    tolerance_minutes = time_tolerance_minutes
  )

  po2_with_fio2 |>
    dplyr::mutate(
      concurrent_pf = dplyr::if_else(
        is_true(!is.na(.data$po2_arterial) & !is.na(.data$fio2_set) & .data$fio2_set > 0),
        .data$po2_arterial / .data$fio2_set,
        NA_real_
      )
    ) |>
    dplyr::filter(!is.na(.data$concurrent_pf))
}

#' Compute the six SOFA component scores from aggregated extremal values
#'
#' @param extremal_data One row per ID of worst values.
#' @param id_name Grouping column name.
#' @return `extremal_data` with `p_f_imputed`, the six components and the total.
#' @keywords internal
compute_sofa_scores_direct <- function(extremal_data, id_name) {
  required_columns <- c(
    "norepinephrine_mcg_kg_min", "epinephrine_mcg_kg_min", "dopamine_mcg_kg_min",
    "dobutamine_mcg_kg_min", "platelet_count", "bilirubin_total", "creatinine",
    "po2_arterial", "pao2_imputed", "map", "spo2", "fio2_set", "gcs_total", "device_rank"
  )
  for (column_name in required_columns) {
    if (!column_name %in% names(extremal_data)) {
      extremal_data[[column_name]] <- NA_real_
    }
  }

  if (!"p_f" %in% names(extremal_data)) {
    extremal_data$p_f <- extremal_data$po2_arterial / extremal_data$fio2_set
  }
  extremal_data$p_f_imputed <- extremal_data$pao2_imputed / extremal_data$fio2_set

  if (!"device_category" %in% names(extremal_data)) {
    rank_to_device <- stats::setNames(names(DEVICE_RANK_DICT), as.character(DEVICE_RANK_DICT))
    mapped_device <- unname(rank_to_device[as.character(extremal_data$device_rank)])
    extremal_data$device_category <- dplyr::coalesce(mapped_device, "Other")
  }

  supported_devices <- c("IMV", "NIPPV", "CPAP")

  extremal_data$sofa_cv_97 <- dplyr::case_when(
    is_true(
      extremal_data$dopamine_mcg_kg_min > 15 |
        extremal_data$epinephrine_mcg_kg_min > 0.1 |
        extremal_data$norepinephrine_mcg_kg_min > 0.1
    ) ~ 4L,
    is_true(
      extremal_data$dopamine_mcg_kg_min > 5 |
        extremal_data$epinephrine_mcg_kg_min <= 0.1 |
        extremal_data$norepinephrine_mcg_kg_min <= 0.1
    ) ~ 3L,
    is_true(
      extremal_data$dopamine_mcg_kg_min <= 5 | extremal_data$dobutamine_mcg_kg_min > 0
    ) ~ 2L,
    is_true(extremal_data$map < 70) ~ 1L,
    is_true(extremal_data$map >= 70) ~ 0L,
    .default = NA_integer_
  )

  extremal_data$sofa_coag <- dplyr::case_when(
    is_true(extremal_data$platelet_count < 20) ~ 4L,
    is_true(extremal_data$platelet_count < 50) ~ 3L,
    is_true(extremal_data$platelet_count < 100) ~ 2L,
    is_true(extremal_data$platelet_count < 150) ~ 1L,
    is_true(extremal_data$platelet_count >= 150) ~ 0L,
    .default = NA_integer_
  )

  extremal_data$sofa_liver <- dplyr::case_when(
    is_true(extremal_data$bilirubin_total >= 12) ~ 4L,
    is_true(extremal_data$bilirubin_total >= 6) ~ 3L,
    is_true(extremal_data$bilirubin_total >= 2) ~ 2L,
    is_true(extremal_data$bilirubin_total >= 1.2) ~ 1L,
    is_true(extremal_data$bilirubin_total < 1.2) ~ 0L,
    .default = NA_integer_
  )

  extremal_data$sofa_resp <- dplyr::case_when(
    is_true(extremal_data$p_f < 100) & extremal_data$device_category %in% supported_devices ~ 4L,
    is_true(extremal_data$p_f >= 100 & extremal_data$p_f < 200) &
      extremal_data$device_category %in% supported_devices ~ 3L,
    is_true(extremal_data$p_f >= 200 & extremal_data$p_f < 300) ~ 2L,
    is_true(extremal_data$p_f >= 300 & extremal_data$p_f < 400) ~ 1L,
    is_true(extremal_data$p_f >= 400) ~ 0L,
    .default = NA_integer_
  )

  extremal_data$sofa_cns <- dplyr::case_when(
    is_true(extremal_data$gcs_total < 6) ~ 4L,
    is_true(extremal_data$gcs_total >= 6 & extremal_data$gcs_total <= 9) ~ 3L,
    is_true(extremal_data$gcs_total >= 10 & extremal_data$gcs_total <= 12) ~ 2L,
    is_true(extremal_data$gcs_total >= 13 & extremal_data$gcs_total <= 14) ~ 1L,
    is_true(extremal_data$gcs_total == 15) ~ 0L,
    .default = NA_integer_
  )

  extremal_data$sofa_renal <- dplyr::case_when(
    is_true(extremal_data$creatinine >= 5) ~ 4L,
    is_true(extremal_data$creatinine >= 3.5) ~ 3L,
    is_true(extremal_data$creatinine >= 2) ~ 2L,
    is_true(extremal_data$creatinine >= 1.2) ~ 1L,
    is_true(extremal_data$creatinine < 1.2) ~ 0L,
    .default = NA_integer_
  )

  subscore_columns <- c(
    "sofa_cv_97", "sofa_coag", "sofa_liver", "sofa_resp", "sofa_cns", "sofa_renal"
  )
  extremal_data$sofa_total <- as.integer(rowSums(
    as.matrix(extremal_data[, subscore_columns, drop = FALSE]),
    na.rm = TRUE
  ))

  extremal_data
}

#' Compute SOFA scores directly from raw CLIF tables
#'
#' Port of `clifpy.utils.sofa_polars.compute_sofa_polars`. Loads labs, vitals,
#' patient assessments, respiratory support and continuous medications from
#' `data_directory`, restricts them to each cohort member's observation window,
#' converts vasoactive doses to mcg/kg/min, and reduces everything to one
#' worst-value row per ID before scoring.
#'
#' The respiratory component uses *concurrent* P/F ratios: each arterial PO2 is
#' paired with the FiO2 in effect within the preceding four hours, and the lowest
#' resulting ratio (with its device) is used.
#'
#' @param data_directory Directory containing the CLIF data files.
#' @param cohort_df Data frame with `hospitalization_id`, `start_dttm` and
#'   `end_dttm`, plus any additional ID columns (e.g. `encounter_block`).
#' @param filetype `"parquet"` (default) or `"csv"`.
#' @param id_name Column to group scores by; defaults to `"hospitalization_id"`.
#' @param extremal_type `"worst"` (default); the only supported aggregation.
#' @param fill_na_scores_with_zero If `TRUE` (default), missing component scores
#'   become 0.
#' @param remove_outliers If `TRUE` (default), drop physiologically implausible
#'   `po2_arterial`, `fio2_set` and `spo2` values before aggregating.
#' @param timezone Optional Olson timezone for the loaded timestamps.
#'
#' @return A tibble with one row per ID: `sofa_cv_97`, `sofa_coag`, `sofa_liver`,
#'   `sofa_resp`, `sofa_cns`, `sofa_renal`, `sofa_total`, and the intermediate
#'   worst values used to derive them.
#' @export
#'
#' @examples
#' \dontrun{
#' cohort <- data.frame(
#'   hospitalization_id = c("H1", "H2"),
#'   start_dttm = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC"),
#'   end_dttm = as.POSIXct(c("2024-01-05", "2024-01-06"), tz = "UTC")
#' )
#' sofa_scores <- compute_sofa_direct("/path/to/clif", cohort, timezone = "UTC")
#' }
compute_sofa_direct <- function(data_directory,
                                cohort_df,
                                filetype = "parquet",
                                id_name = "hospitalization_id",
                                extremal_type = "worst",
                                fill_na_scores_with_zero = TRUE,
                                remove_outliers = TRUE,
                                timezone = NULL) {
  required_columns <- c("hospitalization_id", "start_dttm", "end_dttm")
  missing_columns <- setdiff(required_columns, names(cohort_df))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "cohort_df must contain columns {.val {required_columns}}. Missing: {.val {missing_columns}}"
    )
  }
  if (!id_name %in% names(cohort_df)) {
    cli::cli_abort("id_name {.val {id_name}} not found in cohort_df columns")
  }
  if (!identical(extremal_type, "worst")) {
    cli::cli_abort("extremal_type must be {.val worst}, got {.val {extremal_type}}")
  }

  cohort_local <- cohort_df |>
    dplyr::as_tibble() |>
    standardize_datetime_columns(timezone, c("start_dttm", "end_dttm")) |>
    dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id))

  identifier_columns <- setdiff(names(cohort_local), c("start_dttm", "end_dttm"))
  hospitalization_ids <- unique(cohort_local$hospitalization_id)

  labs_data <- load_labs_direct(
    data_directory, filetype, hospitalization_ids, cohort_local, timezone
  )
  vitals_data <- load_vitals_direct(
    data_directory, filetype, hospitalization_ids, cohort_local, timezone
  )
  assessments_data <- load_patient_assessments_direct(
    data_directory, filetype, hospitalization_ids, cohort_local, timezone
  )
  respiratory_data <- load_respiratory_support_direct(
    data_directory, filetype, hospitalization_ids, cohort_local, 24, timezone
  )
  medications_data <- load_and_convert_medications_direct(
    data_directory, filetype, hospitalization_ids, cohort_local, timezone
  )

  rename_event_time <- function(data, time_column) {
    if (is.null(data)) {
      return(NULL)
    }
    dplyr::rename(data, event_time = dplyr::all_of(time_column))
  }

  combined_data <- dplyr::bind_rows(
    rename_event_time(labs_data, "lab_result_dttm"),
    rename_event_time(vitals_data, "recorded_dttm"),
    rename_event_time(assessments_data, "recorded_dttm"),
    rename_event_time(respiratory_data, "recorded_dttm"),
    rename_event_time(medications_data, "admin_dttm")
  )

  if (remove_outliers && all(c("lab_value_numeric", "fio2_set", "vital_value") %in% names(combined_data))) {
    combined_data <- combined_data |>
      dplyr::mutate(
        lab_value_numeric = dplyr::if_else(
          is_true(!is.na(.data$lab_value_numeric) & .data$lab_category == "po2_arterial" &
            (.data$lab_value_numeric < 0 | .data$lab_value_numeric > 700)),
          NA_real_,
          .data$lab_value_numeric
        ),
        fio2_set = dplyr::if_else(
          is_true(!is.na(.data$fio2_set) & (.data$fio2_set < 0.21 | .data$fio2_set > 1)),
          NA_real_,
          .data$fio2_set
        ),
        vital_value = dplyr::if_else(
          is_true(!is.na(.data$vital_value) & .data$vital_category == "spo2" &
            (.data$vital_value < 50 | .data$vital_value > 100)),
          NA_real_,
          .data$vital_value
        )
      )
  }

  max_labs <- c("creatinine", "bilirubin_total")
  min_labs <- c("platelet_count", "po2_arterial")
  max_meds <- REQUIRED_MEDS

  aggregate_long <- function(data, category_column, value_column, aggregator, data_type) {
    if (!category_column %in% names(data)) {
      return(NULL)
    }
    data |>
      dplyr::filter(!is.na(.data[[category_column]])) |>
      dplyr::group_by(.data[[id_name]], category = .data[[category_column]]) |>
      dplyr::summarise(value = aggregator(.data[[value_column]]), .groups = "drop") |>
      dplyr::mutate(data_type = data_type)
  }

  filter_lab_categories <- function(data, categories) {
    if (!"lab_category" %in% names(data)) {
      return(data)
    }
    dplyr::filter(data, .data$lab_category %in% categories)
  }

  labs_max_aggregated <- aggregate_long(
    filter_lab_categories(combined_data, max_labs),
    "lab_category", "lab_value_numeric", max_or_na, "lab"
  )
  labs_min_aggregated <- aggregate_long(
    filter_lab_categories(combined_data, min_labs),
    "lab_category", "lab_value_numeric", min_or_na, "lab"
  )
  vitals_aggregated <- aggregate_long(
    combined_data, "vital_category", "vital_value", min_or_na, "vital"
  )
  medications_aggregated <- aggregate_long(
    combined_data, "med_category", "dose_mcg_kg_min", max_or_na, "med"
  )
  assessments_aggregated <- aggregate_long(
    combined_data, "assessment_category", "assessment_value", min_or_na, "assessment"
  )

  aggregated_data <- dplyr::bind_rows(
    labs_max_aggregated, labs_min_aggregated,
    vitals_aggregated, medications_aggregated, assessments_aggregated
  )

  # clifpy pivots the output of an unordered polars `group_by`, so its column
  # order varies between runs; here the categories are pivoted in a fixed order.
  canonical_category_order <- c(
    max_labs, min_labs, REQUIRED_VITALS, max_meds, REQUIRED_ASSESSMENTS
  )
  aggregated_data <- aggregated_data |>
    dplyr::mutate(category = factor(
      .data$category,
      levels = union(
        intersect(canonical_category_order, unique(.data$category)),
        unique(.data$category)
      )
    )) |>
    dplyr::arrange(.data$category)

  combined_wide <- aggregated_data |>
    dplyr::select(dplyr::all_of(c(id_name, "category", "value"))) |>
    dplyr::mutate(category = as.character(.data$category)) |>
    tidyr::pivot_wider(names_from = "category", values_from = "value")

  medication_columns_present <- intersect(names(combined_wide), max_meds)
  if (length(medication_columns_present) > 0) {
    combined_wide <- dplyr::rename_with(
      combined_wide,
      \(column_names) paste0(column_names, "_mcg_kg_min"),
      dplyr::all_of(medication_columns_present)
    )
  }

  combined_wide <- impute_pao2_from_spo2_direct(combined_wide)

  other_identifier_columns <- setdiff(intersect(identifier_columns, names(labs_data)), id_name)
  labs_with_po2 <- labs_data |>
    dplyr::filter(.data$lab_category == "po2_arterial", !is.na(.data$lab_value_numeric)) |>
    dplyr::select(
      dplyr::all_of(c(id_name, "lab_result_dttm")),
      po2_arterial = "lab_value_numeric",
      dplyr::all_of(other_identifier_columns)
    )

  concurrent_pf_data <- calculate_concurrent_pf_ratios(
    labs_with_po2, respiratory_data,
    time_tolerance_minutes = 240,
    identifier_columns = identifier_columns
  )

  pf_aggregated <- concurrent_pf_data |>
    dplyr::group_by(.data[[id_name]]) |>
    dplyr::summarise(
      p_f = min_or_na(.data$concurrent_pf),
      po2_arterial_right = min_or_na(.data$po2_arterial),
      fio2_set = max_or_na(.data$fio2_set),
      device_category = .data$device_category[order(.data$concurrent_pf)][1],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      device_rank = dplyr::coalesce(unname(DEVICE_RANK_DICT[.data$device_category]), 9L)
    )

  combined_wide <- dplyr::left_join(combined_wide, pf_aggregated, by = id_name)

  sofa_scores <- compute_sofa_scores_direct(combined_wide, id_name)

  if (fill_na_scores_with_zero) {
    subscore_columns <- c(
      "sofa_cv_97", "sofa_coag", "sofa_liver", "sofa_resp", "sofa_cns", "sofa_renal"
    )
    for (column_name in subscore_columns) {
      column_values <- sofa_scores[[column_name]]
      column_values[is.na(column_values)] <- 0L
      sofa_scores[[column_name]] <- as.integer(column_values)
    }
    sofa_scores$sofa_total <- as.integer(rowSums(
      as.matrix(sofa_scores[, subscore_columns, drop = FALSE]),
      na.rm = TRUE
    ))
  }

  sofa_scores |>
    dplyr::as_tibble() |>
    dplyr::arrange(.data[[id_name]])
}
