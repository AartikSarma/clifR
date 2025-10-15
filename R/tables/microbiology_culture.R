#' Microbiology Culture Table Class
#'
#' @description
#' R6 class for CLIF microbiology_culture table containing culture-based
#' microbiology results (organism identification from cultures).
#'
#' @export
#' @importFrom R6 R6Class
MicrobiologyCulture <- R6::R6Class(
  classname = "MicrobiologyCulture",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize MicrobiologyCulture table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new MicrobiologyCulture instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "microbiology_culture",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter by organism category
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
    #' Filter by fluid/specimen source
    #'
    #' @param fluid_category Character fluid category to filter by.
    #'
    #' @return Filtered tibble.
    filter_by_fluid = function(fluid_category) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$fluid_category == !!fluid_category)
    },

    #' @description
    #' Get blood culture results
    #'
    #' @return tibble with blood culture results.
    get_blood_cultures = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$fluid_category == "blood_buffy")
    },

    #' @description
    #' Get respiratory culture results
    #'
    #' @return tibble with respiratory culture results.
    get_respiratory_cultures = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$fluid_category %in% c("respiratory_tract", "respiratory_tract_lower"))
    },

    #' @description
    #' Get organism distribution
    #'
    #' @param by_group Logical, group by organism_group instead of organism_category.
    #'
    #' @return tibble with organism counts.
    get_organism_distribution = function(by_group = FALSE) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      grouping_var <- if (by_group) "organism_group" else "organism_category"

      self$df %>%
        dplyr::group_by(!!rlang::sym(grouping_var)) %>%
        dplyr::summarize(
          n_cultures = dplyr::n(),
          n_patients = dplyr::n_distinct(patient_id),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_cultures))
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
          n_cultures = dplyr::n(),
          n_positive = sum(.data$organism_category != "no_growth", na.rm = TRUE),
          n_patients = dplyr::n_distinct(patient_id),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          positivity_rate = n_positive / n_cultures
        ) %>%
        dplyr::arrange(dplyr::desc(n_cultures))
    },

    #' @description
    #' Calculate time to culture result
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
          patient_id, hospitalization_id, organism_id,
          fluid_category, organism_category,
          order_dttm, collect_dttm, result_dttm,
          collection_to_result_hours, order_to_collection_hours, order_to_result_hours
        )
    },

    #' @description
    #' Get positive cultures (exclude no growth)
    #'
    #' @return tibble with positive culture results.
    get_positive_cultures = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$organism_category != "no_growth")
    },

    #' @description
    #' Get cultures for specific patient
    #'
    #' @param patient_id Character patient ID.
    #'
    #' @return tibble with patient's culture results.
    get_patient_cultures = function(patient_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$patient_id == !!patient_id) %>%
        dplyr::arrange(collect_dttm)
    },

    #' @description
    #' Summarize microbiology culture data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Microbiology Culture Summary")

      n_total <- nrow(self$df)
      n_patients <- dplyr::n_distinct(self$df$patient_id)
      n_positive <- sum(self$df$organism_category != "no_growth", na.rm = TRUE)
      positivity_rate <- n_positive / n_total

      cli::cli_text("Total cultures: {.val {n_total}}")
      cli::cli_text("Unique patients: {.val {n_patients}}")
      cli::cli_text("Positive cultures: {.val {n_positive}} ({round(positivity_rate*100, 1)}%)")

      cli::cli_h3("Top 10 Organisms")
      organism_dist <- self$get_organism_distribution() %>%
        dplyr::slice_head(n = 10)
      print(organism_dist)

      cli::cli_h3("Specimen Source Distribution")
      fluid_dist <- self$get_fluid_distribution() %>%
        dplyr::slice_head(n = 10)
      print(fluid_dist)

      invisible(list(
        n_total = n_total,
        n_patients = n_patients,
        n_positive = n_positive,
        positivity_rate = positivity_rate,
        organism_distribution = organism_dist,
        fluid_distribution = fluid_dist
      ))
    }
  )
)
