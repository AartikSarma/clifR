#' Code Status Table Class
#'
#' @description
#' R6 class for CLIF code_status table containing code status changes
#' (DNR, DNI, full code, etc.) over time.
#'
#' @export
#' @importFrom R6 R6Class
CodeStatus <- R6::R6Class(
  classname = "CodeStatus",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize CodeStatus table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new CodeStatus instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "code_status",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Get code status history for a patient
    #'
    #' @param patient_id Character patient ID.
    #'
    #' @return tibble of code status changes for the patient.
    get_patient_code_status = function(patient_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$patient_id == !!patient_id) %>%
        dplyr::arrange(start_dttm)
    },

    #' @description
    #' Get code status distribution
    #'
    #' @return tibble with counts by code status category.
    get_status_distribution = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(code_status_category) %>%
        dplyr::summarize(
          n_changes = dplyr::n(),
          n_patients = dplyr::n_distinct(patient_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_changes))
    },

    #' @description
    #' Identify DNR/DNI patients
    #'
    #' @return tibble with DNR/DNI patient list and first change time.
    get_dnr_patients = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(grepl("DNR|DNI|DNAR|AND", code_status_category)) %>%
        dplyr::group_by(patient_id) %>%
        dplyr::summarize(
          first_dnr_time = min(start_dttm, na.rm = TRUE),
          final_code_status = dplyr::last(code_status_category),
          n_changes = dplyr::n(),
          .groups = "drop"
        )
    },

    #' @description
    #' Calculate time to first code status change
    #'
    #' @param reference_time_col Character reference time column.
    #' @param reference_data tibble with reference times (e.g., hospitalization).
    #'
    #' @return tibble with time to first code status change.
    calculate_time_to_first_change = function(reference_time_col = "admission_dttm",
                                              reference_data) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      if (missing(reference_data)) {
        cli::cli_abort("reference_data required")
      }

      # First code status change per patient
      first_change <- self$df %>%
        dplyr::group_by(patient_id) %>%
        dplyr::summarize(
          first_change_time = min(start_dttm, na.rm = TRUE),
          first_status = dplyr::first(code_status_category[start_dttm == first_change_time]),
          .groups = "drop"
        )

      # Join with reference time
      reference_data %>%
        dplyr::select(
          patient_id,
          reference_time = !!rlang::sym(reference_time_col)
        ) %>%
        dplyr::inner_join(first_change, by = "patient_id") %>%
        dplyr::mutate(
          time_to_change_hours = as.numeric(
            difftime(first_change_time, reference_time, units = "hours")
          )
        )
    },

    #' @description
    #' Summarize code status data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Code Status Summary")

      n_total <- nrow(self$df)
      n_patients <- dplyr::n_distinct(self$df$patient_id)
      n_dnr <- sum(grepl("DNR|DNI|DNAR|AND", self$df$code_status_category))

      cli::cli_text("Total status changes: {.val {n_total}}")
      cli::cli_text("Unique patients: {.val {n_patients}}")
      cli::cli_text("DNR/DNI changes: {.val {n_dnr}} ({round(n_dnr/n_total*100, 1)}%)")

      cli::cli_h3("Code Status Distribution")
      status_dist <- self$get_status_distribution()
      print(status_dist)

      invisible(list(
        n_total = n_total,
        n_patients = n_patients,
        n_dnr = n_dnr,
        distribution = status_dist
      ))
    }
  )
)
