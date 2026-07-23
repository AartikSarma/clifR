#' Encounter stitching utilities
#'
#' Port of `clifpy/utils/stitching_encounters.py` (clifpy 0.5.0). Groups
#' hospitalizations that occur within a configurable number of hours of each
#' other into a single `encounter_block`, so that rapid discharge/readmission
#' sequences (e.g. ED to inpatient transfers) can be analyzed as one continuous
#' encounter.
#'
#' @name clif-stitching-encounters
NULL

# Extract a tibble from either a BaseTable-derived R6 object or a data frame.
resolve_stitching_input <- function(table_input, argument_name) {
  if (inherits(table_input, "R6") && !is.null(table_input$df)) {
    return(dplyr::as_tibble(table_input$df))
  }
  if (is.data.frame(table_input)) {
    return(dplyr::as_tibble(table_input))
  }
  cli::cli_abort(
    "{.arg {argument_name}} must be a CLIF table object or a data frame."
  )
}

#' Stitch together related hospital encounters
#'
#' Identifies and groups hospitalizations that occur within a specified time
#' window of each other (default 6 hours), treating them as a single continuous
#' encounter. If a patient is readmitted within `time_interval` hours of a
#' discharge, the two hospitalizations receive the same `encounter_block`.
#' Chains of linked hospitalizations share one block, and blocks are numbered
#' by the row index (within the patient/admission-time sort) of the last
#' hospitalization in each chain, matching clifpy exactly.
#'
#' @param hospitalization A `Hospitalization` table object or data frame with
#'   required columns `patient_id`, `hospitalization_id`, `admission_dttm`,
#'   `discharge_dttm`, `age_at_admission`, `admission_type_category` and
#'   `discharge_category`.
#' @param adt An `Adt` table object or data frame with required columns
#'   `hospitalization_id`, `in_dttm`, `out_dttm`, `location_category` and
#'   `hospital_id`.
#' @param time_interval Integer, default `6`. Number of hours between discharge
#'   and the next admission for the encounters to be considered linked.
#'
#' @return A named list with three tibbles (clifpy returns the same three
#'   objects as a tuple):
#'   \describe{
#'     \item{hospitalization}{The input hospitalization data with an added
#'       `encounter_block` column.}
#'     \item{adt}{The input ADT data with an added `encounter_block` column.}
#'     \item{encounter_mapping}{Mapping of `hospitalization_id` to
#'       `encounter_block`.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' stitched <- stitch_encounters(hospitalization_data, adt_data, time_interval = 12)
#' stitched$hospitalization
#' stitched$adt
#' stitched$encounter_mapping
#' }
stitch_encounters <- function(hospitalization, adt, time_interval = 6) {
  hospitalization_data <- resolve_stitching_input(hospitalization, "hospitalization")
  adt_data <- resolve_stitching_input(adt, "adt")

  # Validate required columns, mirroring clifpy's ValueError messages.
  hospitalization_required_columns <- c(
    "patient_id", "hospitalization_id", "admission_dttm",
    "discharge_dttm", "age_at_admission", "admission_type_category",
    "discharge_category"
  )
  adt_required_columns <- c(
    "hospitalization_id", "in_dttm", "out_dttm",
    "location_category", "hospital_id"
  )

  missing_hospitalization_columns <- setdiff(
    hospitalization_required_columns, names(hospitalization_data)
  )
  if (length(missing_hospitalization_columns) > 0) {
    cli::cli_abort(
      "Missing required columns in hospitalization data frame: {.field {missing_hospitalization_columns}}"
    )
  }

  missing_adt_columns <- setdiff(adt_required_columns, names(adt_data))
  if (length(missing_adt_columns) > 0) {
    cli::cli_abort(
      "Missing required columns in ADT data frame: {.field {missing_adt_columns}}"
    )
  }

  hospitalization_filtered <- hospitalization_data |>
    dplyr::select(dplyr::all_of(hospitalization_required_columns))

  # Join hospitalization stays to their ADT segments (left join, so
  # hospitalizations without ADT rows are retained).
  hospitalization_adt_join <- hospitalization_filtered |>
    dplyr::select(
      "patient_id", "hospitalization_id", "age_at_admission",
      "admission_type_category", "admission_dttm", "discharge_dttm",
      "discharge_category"
    ) |>
    dplyr::left_join(
      adt_data |>
        dplyr::select(
          "hospitalization_id", "in_dttm", "out_dttm",
          "location_category", "hospital_id"
        ),
      by = "hospitalization_id",
      relationship = "many-to-many"
    )

  adt_segment_lookup <- hospitalization_adt_join |>
    dplyr::select("hospitalization_id", "in_dttm", "out_dttm", "hospital_id")

  # Step 1: one row per hospitalization, sorted by patient and admission time.
  hospital_block <- hospitalization_adt_join |>
    dplyr::select(
      "patient_id", "hospitalization_id", "admission_dttm", "discharge_dttm",
      "age_at_admission", "discharge_category", "admission_type_category"
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$patient_id, .data$admission_dttm)

  # Step 2: hours from each discharge to the patient's next admission.
  hospital_block <- hospital_block |>
    dplyr::group_by(.data$patient_id) |>
    dplyr::mutate(next_admission_dttm = dplyr::lead(.data$admission_dttm)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      discharge_to_next_admission_hrs = as.numeric(
        difftime(.data$next_admission_dttm, .data$discharge_dttm, units = "secs")
      ) / 3600
    )

  # Step 3: flag linked encounters, with a tiny tolerance for float rounding
  # (same epsilon as clifpy).
  float_rounding_epsilon <- 1e-6
  hospital_block <- hospital_block |>
    dplyr::mutate(
      linked_hrs = dplyr::coalesce(
        .data$discharge_to_next_admission_hrs <= time_interval + float_rounding_epsilon,
        FALSE
      )
    ) |>
    dplyr::arrange(.data$patient_id, .data$admission_dttm)

  # Initialize encounter_block with row indices + 1, then iteratively propagate
  # the next row's block backwards along linked chains until a fixed point is
  # reached — the exact algorithm clifpy uses, so block numbering matches.
  encounter_block_values <- as.numeric(seq_len(nrow(hospital_block)))
  patient_identifiers <- hospital_block$patient_id
  next_patient_identifiers <- dplyr::lead(patient_identifiers)
  propagation_mask <- hospital_block$linked_hrs &
    !is.na(next_patient_identifiers) &
    patient_identifiers == next_patient_identifiers

  repeat {
    shifted_block_values <- dplyr::lead(encounter_block_values)
    previous_block_values <- encounter_block_values
    encounter_block_values[propagation_mask] <- shifted_block_values[propagation_mask]
    if (identical(encounter_block_values, previous_block_values)) {
      break
    }
  }

  hospital_block$encounter_block <- as.integer(encounter_block_values)

  # Attach ADT segments and de-duplicate, matching clifpy's ordering.
  hospital_block <- hospital_block |>
    dplyr::left_join(
      adt_segment_lookup,
      by = "hospitalization_id",
      relationship = "many-to-many"
    ) |>
    dplyr::arrange(
      .data$patient_id, .data$admission_dttm, .data$in_dttm, .data$out_dttm
    ) |>
    dplyr::distinct()

  # Mapping of hospitalization_id to encounter_block.
  encounter_mapping <- hospital_block |>
    dplyr::select("hospitalization_id", "encounter_block") |>
    dplyr::distinct()

  hospitalization_stitched <- hospitalization_data |>
    dplyr::left_join(encounter_mapping, by = "hospitalization_id")

  adt_stitched <- adt_data |>
    dplyr::left_join(encounter_mapping, by = "hospitalization_id")

  list(
    hospitalization = hospitalization_stitched,
    adt = adt_stitched,
    encounter_mapping = encounter_mapping
  )
}
