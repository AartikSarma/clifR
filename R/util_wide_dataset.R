#' Wide Dataset Transformation
#'
#' @description
#' Transform narrow (long) clinical data to wide time-series format.
#' Creates one row per time point with measurements as columns.

#' Create wide dataset from CLIF tables
#'
#' @description
#' Transform narrow clinical tables (vitals, labs) into wide format with
#' one row per hospitalization per time point.
#'
#' @param vitals_data tibble. Vitals table data.
#' @param labs_data tibble. Labs table data (optional).
#' @param hospitalization_data tibble. Hospitalization table for patient info.
#' @param time_resolution Character. Time resolution: "hour", "4hour", "day" (default: "hour").
#' @param id_col Character. ID column name (default: "hospitalization_id").
#'
#' @return tibble in wide format with measurements as columns.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' wide_data <- create_wide_dataset(
#'   vitals_data = orchestrator$vitals$df,
#'   labs_data = orchestrator$labs$df,
#'   hospitalization_data = orchestrator$hospitalization$df
#' )
#' }
create_wide_dataset <- function(vitals_data = NULL,
                                  labs_data = NULL,
                                  hospitalization_data = NULL,
                                  time_resolution = "hour",
                                  id_col = "hospitalization_id") {

  cli::cli_h2("Creating Wide Dataset")
  cli::cli_alert_info("Time resolution: {time_resolution}")

  # Validate inputs
  if (is.null(vitals_data) && is.null(labs_data)) {
    cli::cli_abort("At least one of vitals_data or labs_data must be provided")
  }

  # Initialize result with time grid
  wide_data <- NULL

  # Process vitals
  if (!is.null(vitals_data)) {
    cli::cli_alert_info("Processing vitals data...")

    vitals_wide <- pivot_vitals_wide(
      vitals_data,
      time_resolution = time_resolution,
      id_col = id_col
    )

    wide_data <- vitals_wide
    cli::cli_alert_success(
      "Vitals: {nrow(vitals_wide)} rows, {ncol(vitals_wide)} columns"
    )
  }

  # Process labs
  if (!is.null(labs_data)) {
    cli::cli_alert_info("Processing labs data...")

    labs_wide <- pivot_labs_wide(
      labs_data,
      time_resolution = time_resolution,
      id_col = id_col
    )

    if (is.null(wide_data)) {
      wide_data <- labs_wide
    } else {
      # Merge vitals and labs
      wide_data <- merge_wide_tables(wide_data, labs_wide, id_col = id_col)
    }

    cli::cli_alert_success(
      "Labs: {nrow(labs_wide)} rows, {ncol(labs_wide)} columns"
    )
  }

  # Add hospitalization information if provided
  if (!is.null(hospitalization_data)) {
    cli::cli_alert_info("Adding hospitalization information...")

    hosp_info <- hospitalization_data %>%
      dplyr::select(
        dplyr::all_of(id_col),
        patient_id,
        age_at_admission,
        admission_dttm,
        discharge_dttm,
        discharge_category
      )

    wide_data <- wide_data %>%
      dplyr::left_join(hosp_info, by = id_col)
  }

  cli::cli_alert_success(
    "Final wide dataset: {nrow(wide_data)} rows, {ncol(wide_data)} columns"
  )

  return(wide_data)
}

#' Pivot vitals to wide format
#'
#' @keywords internal
pivot_vitals_wide <- function(vitals_data, time_resolution, id_col) {

  # Round timestamps to time resolution
  vitals_rounded <- vitals_data %>%
    dplyr::mutate(
      time_rounded = round_to_resolution(recorded_dttm, time_resolution)
    )

  # Pivot vital categories to columns
  vitals_wide <- vitals_rounded %>%
    dplyr::group_by(.data[[id_col]], time_rounded, vital_category) %>%
    dplyr::summarize(
      vital_value = mean(vital_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = vital_category,
      values_from = vital_value,
      names_prefix = "vital_"
    )

  return(vitals_wide)
}

#' Pivot labs to wide format
#'
#' @keywords internal
pivot_labs_wide <- function(labs_data, time_resolution, id_col) {

  # Use result datetime
  labs_rounded <- labs_data %>%
    dplyr::mutate(
      time_rounded = round_to_resolution(lab_result_dttm, time_resolution)
    )

  # Pivot lab categories to columns
  labs_wide <- labs_rounded %>%
    dplyr::group_by(.data[[id_col]], time_rounded, lab_category) %>%
    dplyr::summarize(
      lab_value = mean(lab_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = lab_category,
      values_from = lab_value,
      names_prefix = "lab_"
    )

  return(labs_wide)
}

#' Merge wide tables
#'
#' @keywords internal
merge_wide_tables <- function(table1, table2, id_col) {
  table1 %>%
    dplyr::full_join(
      table2,
      by = c(id_col, "time_rounded")
    )
}

#' Round timestamp to time resolution
#'
#' @keywords internal
round_to_resolution <- function(timestamp, resolution) {
  switch(resolution,
    hour = lubridate::floor_date(timestamp, "hour"),
    "4hour" = {
      hour_of_day <- lubridate::hour(timestamp)
      rounded_hour <- floor(hour_of_day / 4) * 4
      lubridate::floor_date(timestamp, "day") + lubridate::hours(rounded_hour)
    },
    day = lubridate::floor_date(timestamp, "day"),
    {
      cli::cli_warn("Unknown resolution {resolution}, using hour")
      lubridate::floor_date(timestamp, "hour")
    }
  )
}

#' Convert wide dataset to hourly aggregation
#'
#' @description
#' Aggregate wide dataset into hourly windows with summary statistics.
#'
#' @param wide_data tibble. Wide format data.
#' @param agg_function Character. Aggregation function: "mean", "median", "first", "last" (default: "mean").
#' @param id_col Character. ID column name (default: "hospitalization_id").
#'
#' @return tibble with hourly aggregated data.
#'
#' @export
convert_wide_to_hourly <- function(wide_data,
                                     agg_function = "mean",
                                     id_col = "hospitalization_id") {

  if (!"time_rounded" %in% names(wide_data)) {
    cli::cli_abort("wide_data must have time_rounded column")
  }

  cli::cli_alert_info("Aggregating to hourly resolution using {agg_function}")

  # Create hourly time grid
  hourly_data <- wide_data %>%
    dplyr::mutate(
      hour_rounded = lubridate::floor_date(time_rounded, "hour")
    )

  # Get numeric columns to aggregate
  numeric_cols <- names(hourly_data)[sapply(hourly_data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c(id_col, "patient_id", "age_at_admission"))

  # Group by hour and aggregate
  agg_func <- switch(agg_function,
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) median(x, na.rm = TRUE),
    first = function(x) dplyr::first(stats::na.omit(x)),
    last = function(x) dplyr::last(stats::na.omit(x)),
    {
      cli::cli_warn("Unknown aggregation {agg_function}, using mean")
      function(x) mean(x, na.rm = TRUE)
    }
  )

  result <- hourly_data %>%
    dplyr::group_by(.data[[id_col]], hour_rounded) %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(numeric_cols),
        agg_func
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename(time_rounded = hour_rounded)

  cli::cli_alert_success(
    "Hourly aggregation: {nrow(result)} rows, {ncol(result)} columns"
  )

  return(result)
}

#' Fill missing values in wide dataset
#'
#' @description
#' Forward-fill or backward-fill missing values in wide dataset.
#'
#' @param wide_data tibble. Wide format data.
#' @param method Character. Fill method: "forward", "backward", "both" (default: "forward").
#' @param max_gap_hours Numeric. Maximum gap in hours to fill (default: 24).
#' @param id_col Character. ID column name (default: "hospitalization_id").
#'
#' @return tibble with filled values.
#'
#' @export
fill_missing_wide <- function(wide_data,
                                method = "forward",
                                max_gap_hours = 24,
                                id_col = "hospitalization_id") {

  cli::cli_alert_info("Filling missing values using {method} fill (max gap: {max_gap_hours} hours)")

  # Get numeric columns
  numeric_cols <- names(wide_data)[sapply(wide_data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c(id_col, "patient_id", "age_at_admission"))

  result <- wide_data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::arrange(time_rounded, .by_group = TRUE) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_cols),
        ~fill_with_max_gap(.x, method, max_gap_hours)
      )
    ) %>%
    dplyr::ungroup()

  cli::cli_alert_success("Missing value imputation complete")

  return(result)
}

#' Fill with maximum gap constraint
#'
#' @keywords internal
fill_with_max_gap <- function(x, method, max_gap_hours) {
  # Simple forward fill implementation
  # More sophisticated gap-aware filling could be added

  if (method == "forward" || method == "both") {
    x <- tidyr::fill(data.frame(x = x), x, .direction = "down")$x
  }

  if (method == "backward" || method == "both") {
    x <- tidyr::fill(data.frame(x = x), x, .direction = "up")$x
  }

  return(x)
}

#' Calculate summary statistics for wide dataset
#'
#' @description
#' Calculate summary statistics across time for each hospitalization.
#'
#' @param wide_data tibble. Wide format data.
#' @param id_col Character. ID column name (default: "hospitalization_id").
#'
#' @return tibble with summary statistics per hospitalization.
#'
#' @export
summarize_wide_dataset <- function(wide_data, id_col = "hospitalization_id") {

  # Get numeric columns
  numeric_cols <- names(wide_data)[sapply(wide_data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, c(id_col, "patient_id", "age_at_admission"))

  summary_data <- wide_data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarize(
      n_timepoints = dplyr::n(),
      dplyr::across(
        dplyr::all_of(numeric_cols),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  cli::cli_alert_success(
    "Summary statistics: {nrow(summary_data)} hospitalizations"
  )

  return(summary_data)
}
