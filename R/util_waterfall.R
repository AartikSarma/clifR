#' Respiratory-support waterfall processing
#'
#' Port of `clifpy/utils/waterfall.py`. Cleans and waterfall-fills the CLIF
#' `respiratory_support` table: an hourly scaffold is inserted, device and mode
#' labels are inferred heuristically, hierarchical episode identifiers are built,
#' and the numeric setter columns are filled inside each mode block.
#'
#' The pipeline is strictly order-sensitive — every phase consumes the output of
#' the previous one, and several steps re-derive lagged values after earlier
#' assignments. The step order therefore follows the Python original exactly,
#' even where a different order would read more naturally.
#'
#' @name clif-waterfall
NULL

# Forward-fill a vector, carrying the last non-missing value forward.
forward_fill_vector <- function(values) {
  is_present <- !is.na(values)
  if (!any(is_present)) {
    return(values)
  }
  carry_index <- cumsum(is_present)
  carry_index[carry_index == 0] <- NA_integer_
  values[is_present][carry_index]
}

# Backward-fill a vector, carrying the next non-missing value backward.
backward_fill_vector <- function(values) {
  rev(forward_fill_vector(rev(values)))
}

# clifpy's nested `fb`: forward-fill only, or forward then backward when bfill.
fill_forward_or_both <- function(values, use_backward_fill) {
  filled_values <- forward_fill_vector(values)
  if (use_backward_fill) {
    filled_values <- backward_fill_vector(filled_values)
  }
  filled_values
}

# Row indices of each group, in the order the rows appear. NA keys are given
# their own key so grouping matches pandas' behaviour of skipping them only where
# the Python code explicitly does.
group_row_indices <- function(group_keys) {
  key_strings <- do.call(
    paste,
    c(lapply(group_keys, group_key_to_character), sep = "\r")
  )
  unname(split(seq_along(key_strings), factor(key_strings, levels = unique(key_strings))))
}

# Lossless character rendering of a grouping key, so distinct values never collide.
group_key_to_character <- function(key_values) {
  if (inherits(key_values, "POSIXct")) {
    key_values <- as.numeric(key_values)
  }
  rendered <- if (is.double(key_values)) {
    format(key_values, digits = 17, scientific = FALSE, trim = TRUE)
  } else {
    as.character(key_values)
  }
  rendered[is.na(key_values)] <- "NA"
  rendered
}

# Apply `fb` within each group, preserving row order.
fill_within_groups <- function(values, group_keys, use_backward_fill) {
  filled_values <- values
  for (row_indices in group_row_indices(group_keys)) {
    filled_values[row_indices] <- fill_forward_or_both(values[row_indices], use_backward_fill)
  }
  filled_values
}

# clifpy's nested `change_id`: a 1-based running counter that increments each time
# the value changes within a group, with missing values treated as "missing".
change_id <- function(column_values, group_keys) {
  filled_values <- ifelse(is.na(column_values), "missing", as.character(column_values))
  episode_ids <- integer(length(filled_values))
  for (row_indices in group_row_indices(group_keys)) {
    group_values <- filled_values[row_indices]
    is_change <- c(TRUE, group_values[-1] != group_values[-length(group_values)])
    episode_ids[row_indices] <- cumsum(is_change)
  }
  as.integer(episode_ids)
}

# Lagged / leading value within a group, matching pandas' groupby().shift().
shift_within_groups <- function(values, group_keys, offset) {
  shifted_values <- values
  shifted_values[] <- NA
  for (row_indices in group_row_indices(group_keys)) {
    group_values <- values[row_indices]
    group_length <- length(group_values)
    if (offset > 0) {
      shifted_values[row_indices] <- c(rep(NA, min(offset, group_length)),
                                       utils::head(group_values, group_length - offset))
    } else {
      lead_offset <- -offset
      shifted_values[row_indices] <- c(utils::tail(group_values, max(group_length - lead_offset, 0)),
                                       rep(NA, min(lead_offset, group_length)))
    }
  }
  shifted_values
}

# `x > threshold` with missing values treated as FALSE, matching pandas' .gt().
greater_than_ignoring_na <- function(values, threshold) {
  comparison <- values > threshold
  comparison & !is.na(comparison)
}

# Case-insensitive substring test with missing values treated as FALSE.
contains_ignoring_na <- function(values, pattern) {
  matches <- grepl(pattern, values, perl = TRUE)
  matches & !is.na(values)
}

# Build the hourly scaffold with DuckDB, mirroring clifpy's DuckDB branch: one row
# per hour between the floored first and last observation of each encounter,
# stamped at HH:59:59.
build_hourly_scaffold <- function(resp_support, id_col, verbose) {
  if (verbose) {
    cli::cli_alert_info("Building hourly scaffold via DuckDB")
  }

  scaffold_source <- resp_support[!is.na(resp_support[["recorded_dttm"]]), c(id_col, "recorded_dttm")]

  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(connection, "resp_support_bounds_source", as.data.frame(scaffold_source))

  scaffold_query <- sprintf(
    "
    WITH bounds AS (
      SELECT
        \"%1$s\" AS id,
        date_trunc('hour', MIN(recorded_dttm)) AS tmin_h,
        date_trunc('hour', MAX(recorded_dttm)) AS tmax_h
      FROM resp_support_bounds_source
      GROUP BY 1
    ),
    hour_sequence AS (
      SELECT
        b.id AS \"%1$s\",
        gs.ts + INTERVAL '59 minutes 59 seconds' AS recorded_dttm
      FROM bounds b,
           LATERAL generate_series(b.tmin_h, b.tmax_h, INTERVAL 1 HOUR) AS gs(ts)
    )
    SELECT \"%1$s\", recorded_dttm
    FROM hour_sequence
    ORDER BY \"%1$s\", recorded_dttm
    ",
    id_col
  )

  scaffold <- DBI::dbGetQuery(connection, scaffold_query)
  attr(scaffold$recorded_dttm, "tzone") <- "UTC"

  scaffold$recorded_date <- as.Date(scaffold$recorded_dttm, tz = "UTC")
  scaffold$recorded_hour <- as.integer(format(scaffold$recorded_dttm, "%H", tz = "UTC"))
  scaffold$is_scaffold <- TRUE
  dplyr::as_tibble(scaffold)
}

#' Clean and waterfall-fill the CLIF respiratory support table
#'
#' Port of `clifpy.utils.waterfall.process_resp_support_waterfall`. Timestamps are
#' expected to already be in UTC; no timezone conversion is performed.
#'
#' The pipeline runs in four phases:
#'
#' 1. **Setup** — lowercase the categorical labels, coerce the numeric setters,
#'    rescale FiO2 recorded as percentages, and build an hourly scaffold of
#'    `HH:59:59` rows spanning each encounter.
#' 2. **Heuristics** — infer `imv` from ventilator modes and from neighbouring
#'    rows with plausible PEEP / rate / tidal volume, infer `nippv` from
#'    neighbouring rows with plausible pressures, drop duplicate and empty rows,
#'    and clear `nasal cannula` labels that carry PEEP.
#' 3. **Episode identifiers** — forward-fill device and mode labels and derive the
#'    nested `device_cat_id`, `device_id`, `mode_cat_id` and `mode_name_id`
#'    counters.
#' 4. **Waterfall fill** — fill the numeric setters inside each
#'    `(id_col, mode_name_id)` block, restarting at each `trach collar` row, then
#'    forward-fill the tracheostomy flag and tidy up.
#'
#' @param resp_support A data frame of raw CLIF respiratory support data, already
#'   in UTC.
#' @param id_col Encounter-level identifier column. Defaults to
#'   `"hospitalization_id"`.
#' @param bfill When `TRUE`, numeric setters are back-filled after the
#'   forward-fill; when `FALSE` (the default) only forward-fill is applied.
#' @param verbose Whether to print progress messages.
#'
#' @return A tibble with the scaffold rows merged in, the hierarchical episode
#'   identifiers added (`device_cat_id`, `device_id`, `mode_cat_id`,
#'   `mode_name_id`, plus the `is_scaffold` flag) and the numeric setters filled,
#'   sorted by `id_col` and `recorded_dttm`.
#' @export
#'
#' @examples
#' \dontrun{
#' resp_support <- load_data("respiratory_support", "data/clif", "parquet", site_tz = "UTC")
#' waterfalled <- process_resp_support_waterfall(resp_support)
#' }
process_resp_support_waterfall <- function(resp_support,
                                           id_col = "hospitalization_id",
                                           bfill = FALSE,
                                           verbose = TRUE) {
  announce <- function(message_text) {
    if (verbose) {
      cli::cli_alert_info(message_text)
    }
  }

  # ---------------------------------------------------------------- #
  # Phase 0 - set-up & hourly scaffold                                #
  # ---------------------------------------------------------------- #
  announce("Phase 0: initialise & create hourly scaffold")
  resp_support_working <- dplyr::as_tibble(resp_support)

  categorical_columns <- c("device_category", "device_name", "mode_category", "mode_name")
  for (column_name in intersect(categorical_columns, names(resp_support_working))) {
    resp_support_working[[column_name]] <- tolower(resp_support_working[[column_name]])
  }

  numeric_columns <- c(
    "tracheostomy", "fio2_set", "lpm_set", "peep_set",
    "tidal_volume_set", "resp_rate_set", "resp_rate_obs",
    "pressure_support_set", "peak_inspiratory_pressure_set"
  )
  numeric_columns <- intersect(numeric_columns, names(resp_support_working))
  for (column_name in numeric_columns) {
    resp_support_working[[column_name]] <- suppressWarnings(
      as.numeric(resp_support_working[[column_name]])
    )
  }

  if ("fio2_set" %in% names(resp_support_working)) {
    mean_fio2 <- mean(resp_support_working$fio2_set, na.rm = TRUE)
    if (!is.na(mean_fio2) && mean_fio2 > 1) {
      needs_rescaling <- greater_than_ignoring_na(resp_support_working$fio2_set, 1)
      resp_support_working$fio2_set[needs_rescaling] <-
        resp_support_working$fio2_set[needs_rescaling] / 100
      announce("Scaled FiO2 values > 1 down by /100")
    }
  }

  scaffold <- build_hourly_scaffold(resp_support_working, id_col, verbose)
  if (verbose) {
    cli::cli_alert_info("Scaffold rows created: {nrow(scaffold)}")
  }

  resp_support_working$recorded_date <- as.Date(resp_support_working$recorded_dttm, tz = "UTC")
  resp_support_working$recorded_hour <- as.integer(
    format(resp_support_working$recorded_dttm, "%H", tz = "UTC")
  )

  # ---------------------------------------------------------------- #
  # Phase 1 - heuristic device / mode inference                       #
  # ---------------------------------------------------------------- #
  announce("Phase 1: heuristic inference of device & mode")

  most_common_imv_name <- most_common_label(
    resp_support_working, "device_name", "device_category", "imv", "ventilator"
  )
  most_common_nippv_name <- most_common_label(
    resp_support_working, "device_name", "device_category", "nippv", "bipap"
  )
  # `most_common_cmv_name` exists in clifpy but is never consumed; omitted here.

  # --- 1-a IMV inferred from the ventilator mode categories
  ventilator_mode_pattern <- "(?:assist control-volume control|simv|pressure control)"
  inferred_imv_rows <- is.na(resp_support_working$device_category) &
    is.na(resp_support_working$device_name) &
    contains_ignoring_na(resp_support_working$mode_category, ventilator_mode_pattern)
  resp_support_working$device_category[inferred_imv_rows] <- "imv"
  resp_support_working$device_name[inferred_imv_rows] <- most_common_imv_name

  # --- 1-b IMV inferred by looking behind / ahead
  resp_support_working <- sort_by_columns(resp_support_working, c(id_col, "recorded_dttm"))
  previous_device_category <- shift_within_groups(
    resp_support_working$device_category, list(resp_support_working[[id_col]]), 1
  )
  next_device_category <- shift_within_groups(
    resp_support_working$device_category, list(resp_support_working[[id_col]]), -1
  )
  imv_like_rows <- is.na(resp_support_working$device_category) &
    (identical_ignoring_na(previous_device_category, "imv") |
       identical_ignoring_na(next_device_category, "imv")) &
    greater_than_ignoring_na(resp_support_working$peep_set, 1) &
    greater_than_ignoring_na(resp_support_working$resp_rate_set, 1) &
    greater_than_ignoring_na(resp_support_working$tidal_volume_set, 1)
  resp_support_working$device_category[imv_like_rows] <- "imv"
  resp_support_working$device_name[imv_like_rows] <- most_common_imv_name

  # --- 1-c NIPPV heuristics (lagged values are re-derived after 1-b's writes)
  previous_device_category <- shift_within_groups(
    resp_support_working$device_category, list(resp_support_working[[id_col]]), 1
  )
  next_device_category <- shift_within_groups(
    resp_support_working$device_category, list(resp_support_working[[id_col]]), -1
  )
  nippv_like_rows <- is.na(resp_support_working$device_category) &
    (identical_ignoring_na(previous_device_category, "nippv") |
       identical_ignoring_na(next_device_category, "nippv")) &
    greater_than_ignoring_na(resp_support_working$peak_inspiratory_pressure_set, 1) &
    greater_than_ignoring_na(resp_support_working$pressure_support_set, 1)
  resp_support_working$device_category[nippv_like_rows] <- "nippv"
  resp_support_working$device_name[nippv_like_rows & is.na(resp_support_working$device_name)] <-
    most_common_nippv_name

  # --- 1-d Clean duplicates & empty rows
  resp_support_working <- sort_by_columns(resp_support_working, c(id_col, "recorded_dttm"))
  duplicate_counts <- count_within_groups(
    list(resp_support_working[[id_col]], resp_support_working$recorded_dttm)
  )
  resp_support_working <- resp_support_working[
    !(duplicate_counts > 1 & identical_ignoring_na(resp_support_working$device_category, "nippv")), ,
    drop = FALSE
  ]
  duplicate_counts <- count_within_groups(
    list(resp_support_working[[id_col]], resp_support_working$recorded_dttm)
  )
  resp_support_working <- resp_support_working[
    !(duplicate_counts > 1 & is.na(resp_support_working$device_category)), ,
    drop = FALSE
  ]

  # --- 1-e Guard: nasal-cannula rows must never carry PEEP
  if ("peep_set" %in% names(resp_support_working)) {
    bad_nasal_cannula_rows <-
      identical_ignoring_na(resp_support_working$device_category, "nasal cannula") &
      greater_than_ignoring_na(resp_support_working$peep_set, 0)
    if (any(bad_nasal_cannula_rows)) {
      resp_support_working$device_category[bad_nasal_cannula_rows] <- NA_character_
      announce(paste0(
        sum(bad_nasal_cannula_rows),
        " rows had PEEP>0 on nasal cannula device_category reset"
      ))
    }
  }

  # Drop rows carrying no respiratory information at all
  informative_columns <- intersect(
    c(
      "device_category", "device_name", "mode_category", "mode_name",
      "tracheostomy", "fio2_set", "lpm_set", "peep_set", "tidal_volume_set",
      "resp_rate_set", "resp_rate_obs", "pressure_support_set",
      "peak_inspiratory_pressure_set"
    ),
    names(resp_support_working)
  )
  has_any_information <- Reduce(
    `|`,
    lapply(informative_columns, function(column_name) !is.na(resp_support_working[[column_name]]))
  )
  resp_support_working <- resp_support_working[has_any_information, , drop = FALSE]

  # Keep the first row per encounter timestamp
  is_first_of_timestamp <- !duplicated(
    paste(
      resp_support_working[[id_col]],
      as.numeric(resp_support_working$recorded_dttm),
      sep = "\r"
    )
  )
  resp_support_working <- resp_support_working[is_first_of_timestamp, , drop = FALSE]

  # Merge in the scaffold rows, then sort exactly as clifpy does
  resp_support_working$is_scaffold <- FALSE
  resp_support_working <- dplyr::bind_rows(resp_support_working, scaffold)
  resp_support_working <- sort_by_columns(
    resp_support_working,
    c(id_col, "recorded_dttm", "recorded_date", "recorded_hour")
  )

  # ---------------------------------------------------------------- #
  # Phase 2 - hierarchical IDs                                        #
  # ---------------------------------------------------------------- #
  announce("Phase 2: build hierarchical IDs")

  encounter_keys <- list(resp_support_working[[id_col]])

  resp_support_working$device_category <- fill_within_groups(
    resp_support_working$device_category, encounter_keys, use_backward_fill = FALSE
  )
  resp_support_working$device_cat_id <- change_id(
    resp_support_working$device_category, encounter_keys
  )

  # clifpy re-sorts by recorded_dttm before this fill; the frame is already sorted
  # by (id, recorded_dttm) and pandas' multi-key sort is stable, so the within-group
  # row order is unchanged and the fill is equivalent.
  resp_support_working$device_name <- fill_within_groups(
    resp_support_working$device_name,
    list(resp_support_working[[id_col]], resp_support_working$device_cat_id),
    use_backward_fill = bfill
  )
  resp_support_working$device_id <- change_id(
    resp_support_working$device_name, encounter_keys
  )

  resp_support_working <- sort_by_columns(resp_support_working, c(id_col, "recorded_dttm"))
  encounter_keys <- list(resp_support_working[[id_col]])

  resp_support_working$mode_category <- fill_within_groups(
    resp_support_working$mode_category,
    list(resp_support_working[[id_col]], resp_support_working$device_id),
    use_backward_fill = bfill
  )
  resp_support_working$mode_cat_id <- change_id(
    resp_support_working$mode_category, encounter_keys
  )

  resp_support_working$mode_name <- fill_within_groups(
    resp_support_working$mode_name,
    list(resp_support_working[[id_col]], resp_support_working$mode_cat_id),
    use_backward_fill = bfill
  )
  resp_support_working$mode_name_id <- change_id(
    resp_support_working$mode_name, encounter_keys
  )

  # ---------------------------------------------------------------- #
  # Phase 3 - numeric waterfall                                       #
  # ---------------------------------------------------------------- #
  announce(sprintf(
    "Phase 3: %s numeric fill inside mode_name_id blocks",
    if (bfill) "bi-directional" else "forward-only"
  ))

  if ("fio2_set" %in% names(resp_support_working)) {
    room_air_rows <- identical_ignoring_na(resp_support_working$device_category, "room air") &
      is.na(resp_support_working$fio2_set)
    resp_support_working$fio2_set[room_air_rows] <- 0.21
  }

  if ("tidal_volume_set" %in% names(resp_support_working)) {
    is_pressure_support_mode <- identical_ignoring_na(
      resp_support_working$mode_category, "pressure support/cpap"
    )
    has_trach_device_name <- contains_ignoring_na(resp_support_working$device_name, "trach")
    implausible_tidal_volume_rows <-
      (is_pressure_support_mode & !is.na(resp_support_working$pressure_support_set)) |
      (is.na(resp_support_working$mode_category) & has_trach_device_name) |
      (is_pressure_support_mode & has_trach_device_name)
    resp_support_working$tidal_volume_set[implausible_tidal_volume_rows] <- NA_real_
  }

  waterfall_columns <- intersect(
    c(
      "fio2_set", "lpm_set", "peep_set", "tidal_volume_set",
      "pressure_support_set", "resp_rate_set", "resp_rate_obs",
      "peak_inspiratory_pressure_set"
    ),
    names(resp_support_working)
  )

  if (verbose) {
    cli::cli_alert_info(
      "applying waterfall fill to {length(unique(resp_support_working[[id_col]]))} encounters"
    )
  }

  is_trach_collar <- identical_ignoring_na(resp_support_working$device_category, "trach collar")
  mode_block_indices <- group_row_indices(
    list(resp_support_working[[id_col]], resp_support_working$mode_name_id)
  )
  for (block_indices in mode_block_indices) {
    block_trach_collar <- is_trach_collar[block_indices]
    if (any(block_trach_collar)) {
      # clifpy's `fill_block` intends a trach-collar row to break the block, so
      # fills restart at every such row:
      #
      #   breaker = (g["device_category"] == "trach collar").cumsum()
      #   return g.groupby(breaker)[num_cols_fill].apply(fb)
      #
      # In pandas that inner `groupby(...).apply()` returns a frame indexed by
      # (breaker, original_row) rather than by original_row alone. When the outer
      # `groupby(...).apply(fill_block)` result is written back with
      # `rs[num_cols_fill] = ...`, those tuple labels match no row, so every
      # filled column of a block containing a trach-collar row comes back as NaN
      # — the observed values in the block are erased along with the fill.
      #
      # Parity with clifpy is the requirement here, so the same erasure is
      # reproduced rather than the evidently intended per-breaker fill.
      for (column_name in waterfall_columns) {
        resp_support_working[[column_name]][block_indices] <- NA_real_
      }
    } else {
      for (column_name in waterfall_columns) {
        resp_support_working[[column_name]][block_indices] <- fill_forward_or_both(
          resp_support_working[[column_name]][block_indices], bfill
        )
      }
    }
  }

  # "T-piece" devices are recorded as blow-by
  t_piece_rows <- is.na(resp_support_working$mode_category) &
    contains_ignoring_na(resp_support_working$device_name, "t-piece")
  resp_support_working$mode_category[t_piece_rows] <- "blow by"

  if ("tracheostomy" %in% names(resp_support_working)) {
    resp_support_working$tracheostomy <- fill_within_groups(
      resp_support_working$tracheostomy,
      list(resp_support_working[[id_col]]),
      use_backward_fill = FALSE
    )
  }

  # ---------------------------------------------------------------- #
  # Phase 4 - final tidy-up                                           #
  # ---------------------------------------------------------------- #
  announce("Phase 4: final dedup & ordering")
  resp_support_working <- resp_support_working[
    !duplicated(resp_support_working), , drop = FALSE
  ]
  resp_support_working <- sort_by_columns(resp_support_working, c(id_col, "recorded_dttm"))

  helper_columns <- intersect(c("recorded_date", "recorded_hour"), names(resp_support_working))
  resp_support_working <- resp_support_working[
    , setdiff(names(resp_support_working), helper_columns), drop = FALSE
  ]

  announce("Respiratory-support waterfall complete.")
  dplyr::as_tibble(resp_support_working)
}

# Stable multi-key sort, matching pandas' lexsort-based sort_values().
sort_by_columns <- function(data, column_names) {
  sort_arguments <- c(
    lapply(column_names, function(column_name) data[[column_name]]),
    list(method = "radix", na.last = TRUE)
  )
  data[do.call(order, sort_arguments), , drop = FALSE]
}

# Equality against a scalar with missing values treated as FALSE.
identical_ignoring_na <- function(values, target_value) {
  comparison <- values == target_value
  comparison & !is.na(comparison)
}

# Group size broadcast back to each row, matching groupby().transform("size").
count_within_groups <- function(group_keys) {
  group_sizes <- integer(length(group_keys[[1]]))
  for (row_indices in group_row_indices(group_keys)) {
    group_sizes[row_indices] <- length(row_indices)
  }
  group_sizes
}

# Most frequent value of `label_column` among rows whose `category_column` equals
# `category_value`; falls back to `default_label` when there are none. Mirrors
# pandas' value_counts() ordering (descending count).
most_common_label <- function(data, label_column, category_column, category_value, default_label) {
  if (!all(c(label_column, category_column) %in% names(data))) {
    return(default_label)
  }
  candidate_rows <- !is.na(data[[label_column]]) &
    identical_ignoring_na(data[[category_column]], category_value)
  if (!any(candidate_rows)) {
    return(default_label)
  }
  label_counts <- sort(table(data[[label_column]][candidate_rows]), decreasing = TRUE)
  names(label_counts)[1]
}
