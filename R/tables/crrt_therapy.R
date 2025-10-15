#' CRRT Therapy Table Class
#'
#' @description
#' R6 class for CLIF crrt_therapy table containing continuous renal replacement
#' therapy (dialysis) settings and parameters.
#'
#' @export
#' @importFrom R6 R6Class
CrrtTherapy <- R6::R6Class(
  classname = "CrrtTherapy",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize CrrtTherapy table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new CrrtTherapy instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "crrt_therapy",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Get CRRT mode distribution
    #'
    #' @return tibble with counts by CRRT mode.
    get_mode_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(crrt_mode_category) %>%
        dplyr::summarize(
          n_sessions = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_sessions))
    },

    #' @description
    #' Calculate CRRT duration per hospitalization
    #'
    #' @return tibble with CRRT duration statistics.
    calculate_crrt_duration = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          first_crrt = min(recorded_dttm, na.rm = TRUE),
          last_crrt = max(recorded_dttm, na.rm = TRUE),
          n_observations = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          crrt_duration_hours = as.numeric(
            difftime(last_crrt, first_crrt, units = "hours")
          ),
          crrt_duration_days = crrt_duration_hours / 24
        )
    },

    #' @description
    #' Summarize CRRT therapy data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("CRRT Therapy Summary")

      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)

      cli::cli_text("Total observations: {.val {n_total}}")
      cli::cli_text("Hospitalizations with CRRT: {.val {n_hosp}}")

      cli::cli_h3("CRRT Mode Distribution")
      mode_summary <- self$get_mode_summary()
      print(mode_summary)

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        mode_summary = mode_summary
      ))
    }
  )
)
