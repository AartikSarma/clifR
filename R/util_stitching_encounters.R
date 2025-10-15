#' Stitch Related Hospital Encounters
#'
#' @description
#' Link related hospital encounters based on time proximity.
#' Encounters within a specified time window are grouped together.
#'
#' @param hospitalizations tibble or Hospitalization object. Hospital encounter data.
#' @param time_interval_hours Numeric. Maximum hours between discharge and
#'   next admission to consider encounters related (default: 24).
#' @param patient_id_col Character. Name of patient ID column (default: "patient_id").
#' @param hosp_id_col Character. Name of hospitalization ID column (default: "hospitalization_id").
#' @param admission_col Character. Name of admission datetime column (default: "admission_dttm").
#' @param discharge_col Character. Name of discharge datetime column (default: "discharge_dttm").
#'
#' @return tibble with original hospitalization_id and new stitched hospitalization_joined_id.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Stitch encounters within 24 hours
#' stitched <- stitch_encounters(hospitalizations, time_interval_hours = 24)
#'
#' # Stitch encounters within 48 hours
#' stitched <- stitch_encounters(hospitalizations, time_interval_hours = 48)
#' }
stitch_encounters <- function(hospitalizations,
                               time_interval_hours = 24,
                               patient_id_col = "patient_id",
                               hosp_id_col = "hospitalization_id",
                               admission_col = "admission_dttm",
                               discharge_col = "discharge_dttm") {

  # Extract data if Hospitalization object
  if (inherits(hospitalizations, "Hospitalization")) {
    hosp_df <- hospitalizations$df
  } else {
    hosp_df <- hospitalizations
  }

  # Validate required columns
  required_cols <- c(patient_id_col, hosp_id_col, admission_col, discharge_col)
  missing_cols <- setdiff(required_cols, names(hosp_df))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns for encounter stitching:",
      "x" = "Missing: {.field {missing_cols}}"
    ))
  }

  cli::cli_alert_info("Stitching encounters with {time_interval_hours} hour window")

  # Sort by patient and admission time
  hosp_sorted <- hosp_df %>%
    dplyr::arrange(
      .data[[patient_id_col]],
      .data[[admission_col]]
    )

  # Initialize stitched IDs
  hosp_sorted <- hosp_sorted %>%
    dplyr::mutate(
      hospitalization_joined_id = .data[[hosp_id_col]],
      .row_number = dplyr::row_number()
    )

  # Track groupings
  current_group_id <- NULL
  group_assignments <- rep(NA_character_, nrow(hosp_sorted))

  # Iterate through each patient's hospitalizations
  for (i in seq_len(nrow(hosp_sorted))) {
    current_patient <- hosp_sorted[[patient_id_col]][i]
    current_admission <- hosp_sorted[[admission_col]][i]
    current_hosp_id <- hosp_sorted[[hosp_id_col]][i]

    # Check if this is the first hospitalization or a new patient
    if (i == 1 || hosp_sorted[[patient_id_col]][i - 1] != current_patient) {
      # Start new group
      current_group_id <- current_hosp_id
      group_assignments[i] <- current_group_id
      next
    }

    # Check time gap from previous discharge
    prev_discharge <- hosp_sorted[[discharge_col]][i - 1]
    time_gap_hours <- as.numeric(
      difftime(current_admission, prev_discharge, units = "hours")
    )

    # If within time window, assign to same group
    if (!is.na(time_gap_hours) && time_gap_hours <= time_interval_hours) {
      group_assignments[i] <- current_group_id
    } else {
      # Start new group
      current_group_id <- current_hosp_id
      group_assignments[i] <- current_group_id
    }
  }

  # Create result
  result <- hosp_sorted %>%
    dplyr::mutate(hospitalization_joined_id = group_assignments) %>%
    dplyr::select(
      dplyr::all_of(c(hosp_id_col, "hospitalization_joined_id"))
    )

  # Count stitched groups
  n_original <- length(unique(result[[hosp_id_col]]))
  n_stitched <- length(unique(result$hospitalization_joined_id))
  n_combined <- n_original - n_stitched

  cli::cli_alert_success(
    "Stitched {n_original} encounters into {n_stitched} groups ({n_combined} combined)"
  )

  return(result)
}

#' Apply encounter stitching to hospitalization data
#'
#' @description
#' Update hospitalization data with stitched encounter IDs.
#'
#' @param hospitalizations tibble or Hospitalization object. Hospital encounter data.
#' @param time_interval_hours Numeric. Maximum hours between encounters (default: 24).
#'
#' @return Updated tibble with hospitalization_joined_id column.
#'
#' @export
apply_encounter_stitching <- function(hospitalizations,
                                       time_interval_hours = 24) {

  # Get stitching mapping
  stitching_map <- stitch_encounters(
    hospitalizations,
    time_interval_hours = time_interval_hours
  )

  # Extract data
  if (inherits(hospitalizations, "Hospitalization")) {
    hosp_df <- hospitalizations$df
  } else {
    hosp_df <- hospitalizations
  }

  # Apply stitching
  result <- hosp_df %>%
    dplyr::left_join(
      stitching_map,
      by = "hospitalization_id"
    ) %>%
    dplyr::mutate(
      hospitalization_joined_id = dplyr::coalesce(
        hospitalization_joined_id.y,
        hospitalization_joined_id.x,
        hospitalization_id
      )
    ) %>%
    dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y"))

  return(result)
}
