#' Position Table Class
#'
#' @description
#' R6 class for CLIF position table containing patient positioning data
#' (primarily prone vs. not prone positioning for ARDS management).
#'
#' @export
#' @importFrom R6 R6Class
Position <- R6::R6Class(
  classname = "Position",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize Position table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new Position instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "position",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Get prone positioning records
    #'
    #' @return tibble with prone position records.
    get_prone_positions = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$position_category == "prone")
    },

    #' @description
    #' Get non-prone positioning records
    #'
    #' @return tibble with non-prone position records.
    get_not_prone_positions = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$position_category == "not_prone")
    },

    #' @description
    #' Get position distribution
    #'
    #' @return tibble with position category counts.
    get_position_distribution = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(position_category) %>%
        dplyr::summarize(
          n_observations = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_observations))
    },

    #' @description
    #' Calculate prone positioning duration
    #'
    #' @return tibble with prone positioning session statistics.
    calculate_prone_duration = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      # Get prone sessions
      prone_data <- self$df %>%
        dplyr::filter(.data$position_category == "prone") %>%
        dplyr::arrange(hospitalization_id, recorded_dttm)

      if (nrow(prone_data) == 0) {
        cli::cli_alert_warning("No prone positioning data")
        return(invisible(NULL))
      }

      # Calculate duration per hospitalization
      prone_data %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          first_prone = min(recorded_dttm, na.rm = TRUE),
          last_prone = max(recorded_dttm, na.rm = TRUE),
          n_prone_observations = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          prone_duration_hours = as.numeric(
            difftime(last_prone, first_prone, units = "hours")
          ),
          prone_duration_days = prone_duration_hours / 24
        )
    },

    #' @description
    #' Identify patients who received prone positioning
    #'
    #' @return tibble with patients who were proned.
    get_proned_patients = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$position_category == "prone") %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          first_prone_time = min(recorded_dttm, na.rm = TRUE),
          last_prone_time = max(recorded_dttm, na.rm = TRUE),
          n_prone_observations = dplyr::n(),
          .groups = "drop"
        )
    },

    #' @description
    #' Get positioning for specific hospitalization
    #'
    #' @param hospitalization_id Character hospitalization ID.
    #'
    #' @return tibble with patient's positioning records.
    get_hospitalization_positions = function(hospitalization_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$hospitalization_id == !!hospitalization_id) %>%
        dplyr::arrange(recorded_dttm)
    },

    #' @description
    #' Calculate position changes (prone to not-prone transitions)
    #'
    #' @return tibble with position transition statistics.
    calculate_position_changes = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::arrange(hospitalization_id, recorded_dttm) %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::mutate(
          position_change = .data$position_category != dplyr::lag(.data$position_category, default = "")
        ) %>%
        dplyr::summarize(
          n_position_changes = sum(position_change, na.rm = TRUE),
          n_observations = dplyr::n(),
          .groups = "drop"
        )
    },

    #' @description
    #' Summarize position data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Position Summary")

      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)
      n_prone <- sum(self$df$position_category == "prone", na.rm = TRUE)
      prone_pct <- n_prone / n_total * 100

      cli::cli_text("Total position observations: {.val {n_total}}")
      cli::cli_text("Hospitalizations: {.val {n_hosp}}")
      cli::cli_text("Prone observations: {.val {n_prone}} ({round(prone_pct, 1)}%)")

      cli::cli_h3("Position Distribution")
      position_dist <- self$get_position_distribution()
      print(position_dist)

      # Count proned patients
      proned_patients <- self$get_proned_patients()
      n_proned_hosp <- nrow(proned_patients)
      cli::cli_text("Hospitalizations with prone positioning: {.val {n_proned_hosp}}")

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        n_prone = n_prone,
        prone_pct = prone_pct,
        n_proned_hospitalizations = n_proned_hosp,
        position_distribution = position_dist
      ))
    }
  )
)
