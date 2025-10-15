#' Microbiology Nonculture Table Class
#'
#' @description
#' R6 class for CLIF microbiology_nonculture table containing non-culture-based
#' microbiology results (e.g., PCR tests for C. diff, COVID-19, RSV).
#'
#' @export
#' @importFrom R6 R6Class
MicrobiologyNonculture <- R6::R6Class(
  classname = "MicrobiologyNonculture",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize MicrobiologyNonculture table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new MicrobiologyNonculture instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "microbiology_nonculture",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter by organism
    #'
    #' @param organism_category Character organism to filter by.
    #'
    #' @return Filtered tibble.
    filter_by_organism = function(organism_category) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$organism_category == !!organism_category)
    },

    #' @description
    #' Get COVID-19 (SARS-CoV-2) test results
    #'
    #' @return tibble with COVID-19 test results.
    get_covid_tests = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$organism_category == "sars_cov2")
    },

    #' @description
    #' Get C. difficile test results
    #'
    #' @return tibble with C. diff test results.
    get_cdiff_tests = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$organism_category == "clostridium_difficile")
    },

    #' @description
    #' Get RSV test results
    #'
    #' @return tibble with RSV test results.
    get_rsv_tests = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$organism_category == "respiratory_syncytial_virus")
    },

    #' @description
    #' Get positive test results (detected)
    #'
    #' @return tibble with positive test results.
    get_positive_tests = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$result_category == "detected")
    },

    #' @description
    #' Get test distribution by organism
    #'
    #' @return tibble with test counts by organism.
    get_organism_distribution = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(organism_category) %>%
        dplyr::summarize(
          n_tests = dplyr::n(),
          n_detected = sum(.data$result_category == "detected", na.rm = TRUE),
          n_patients = dplyr::n_distinct(patient_id),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          positivity_rate = n_detected / n_tests
        ) %>%
        dplyr::arrange(dplyr::desc(n_tests))
    },

    #' @description
    #' Calculate test turnaround time
    #'
    #' @return tibble with turnaround time statistics.
    calculate_turnaround_time = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::mutate(
          collection_to_result_hours = as.numeric(
            difftime(result_dttm, collect_dttm, units = "hours")
          ),
          order_to_collection_hours = as.numeric(
            difftime(collect_dttm, order_dttm, units = "hours")
          ),
          order_to_result_hours = as.numeric(
            difftime(result_dttm, order_dttm, units = "hours")
          )
        ) %>%
        dplyr::select(
          patient_id, hospitalization_id,
          organism_category, result_category,
          order_dttm, collect_dttm, result_dttm,
          collection_to_result_hours, order_to_collection_hours, order_to_result_hours
        )
    },

    #' @description
    #' Get specimen source distribution
    #'
    #' @return tibble with fluid category counts.
    get_fluid_distribution = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(fluid_category) %>%
        dplyr::summarize(
          n_tests = dplyr::n(),
          n_detected = sum(.data$result_category == "detected", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          positivity_rate = n_detected / n_tests
        ) %>%
        dplyr::arrange(dplyr::desc(n_tests))
    },

    #' @description
    #' Get tests for specific patient
    #'
    #' @param patient_id Character patient ID.
    #'
    #' @return tibble with patient's test results.
    get_patient_tests = function(patient_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$patient_id == !!patient_id) %>%
        dplyr::arrange(collect_dttm)
    },

    #' @description
    #' Summarize microbiology nonculture data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Microbiology Nonculture Summary")

      n_total <- nrow(self$df)
      n_patients <- dplyr::n_distinct(self$df$patient_id)
      n_positive <- sum(self$df$result_category == "detected", na.rm = TRUE)
      positivity_rate <- n_positive / n_total

      cli::cli_text("Total tests: {.val {n_total}}")
      cli::cli_text("Unique patients: {.val {n_patients}}")
      cli::cli_text("Positive tests: {.val {n_positive}} ({round(positivity_rate*100, 1)}%)")

      cli::cli_h3("Organism Distribution")
      organism_dist <- self$get_organism_distribution()
      print(organism_dist)

      invisible(list(
        n_total = n_total,
        n_patients = n_patients,
        n_positive = n_positive,
        positivity_rate = positivity_rate,
        organism_distribution = organism_dist
      ))
    }
  )
)
