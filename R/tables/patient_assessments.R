#' Patient Assessments Table Class
#'
#' @description
#' R6 class for CLIF patient_assessments table containing clinical assessment
#' scores (GCS, RASS, pain scales, delirium screens, SAT/SBT data, etc.).
#'
#' @export
#' @importFrom R6 R6Class
PatientAssessments <- R6::R6Class(
  classname = "PatientAssessments",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize PatientAssessments table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new PatientAssessments instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "patient_assessments",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter by assessment category
    #'
    #' @param assessment_category Character assessment to filter by.
    #'
    #' @return Filtered tibble.
    filter_by_assessment = function(assessment_category) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$assessment_category == !!assessment_category)
    },

    #' @description
    #' Filter by assessment group
    #'
    #' @param assessment_group Character assessment group.
    #'
    #' @return Filtered tibble.
    filter_by_group = function(assessment_group) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$assessment_group == !!assessment_group)
    },

    #' @description
    #' Get GCS (Glasgow Coma Scale) scores
    #'
    #' @return tibble with GCS assessments.
    get_gcs_scores = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(grepl("^gcs_", .data$assessment_category))
    },

    #' @description
    #' Get RASS (Richmond Agitation-Sedation Scale) scores
    #'
    #' @return tibble with RASS assessments.
    get_rass_scores = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$assessment_category == "RASS")
    },

    #' @description
    #' Get CAM-ICU (delirium) assessments
    #'
    #' @return tibble with CAM-ICU assessments.
    get_cam_assessments = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(grepl("^cam_", .data$assessment_category))
    },

    #' @description
    #' Get pain scores (all pain scales)
    #'
    #' @return tibble with pain assessments.
    get_pain_scores = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$assessment_group == "Pain")
    },

    #' @description
    #' Get SAT (Spontaneous Awakening Trial) data
    #'
    #' @return tibble with SAT assessments.
    get_sat_data = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(grepl("^sat_", .data$assessment_category))
    },

    #' @description
    #' Get SBT (Spontaneous Breathing Trial) data
    #'
    #' @return tibble with SBT assessments.
    get_sbt_data = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(grepl("^sbt_", .data$assessment_category))
    },

    #' @description
    #' Get Braden scale (pressure ulcer risk) scores
    #'
    #' @return tibble with Braden assessments.
    get_braden_scores = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(grepl("^braden_", .data$assessment_category))
    },

    #' @description
    #' Get assessment distribution
    #'
    #' @param by_group Logical, summarize by assessment_group instead of assessment_category.
    #'
    #' @return tibble with assessment counts.
    get_assessment_distribution = function(by_group = FALSE) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      grouping_var <- if (by_group) "assessment_group" else "assessment_category"

      self$df %>%
        dplyr::group_by(!!rlang::sym(grouping_var)) %>%
        dplyr::summarize(
          n_assessments = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_assessments))
    },

    #' @description
    #' Calculate assessment compliance (frequency)
    #'
    #' @param assessment_category Character assessment to analyze.
    #' @param target_hours Numeric target hours between assessments.
    #'
    #' @return tibble with compliance statistics.
    calculate_assessment_compliance = function(assessment_category, target_hours = 4) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$assessment_category == !!assessment_category) %>%
        dplyr::arrange(hospitalization_id, recorded_dttm) %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::mutate(
          time_since_last = as.numeric(
            difftime(recorded_dttm, dplyr::lag(recorded_dttm), units = "hours")
          )
        ) %>%
        dplyr::summarize(
          n_assessments = dplyr::n(),
          median_interval_hours = median(time_since_last, na.rm = TRUE),
          mean_interval_hours = mean(time_since_last, na.rm = TRUE),
          pct_within_target = sum(time_since_last <= target_hours, na.rm = TRUE) /
            sum(!is.na(time_since_last)) * 100,
          .groups = "drop"
        )
    },

    #' @description
    #' Get assessments for specific patient
    #'
    #' @param hospitalization_id Character hospitalization ID.
    #'
    #' @return tibble with patient's assessment results.
    get_hospitalization_assessments = function(hospitalization_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$hospitalization_id == !!hospitalization_id) %>%
        dplyr::arrange(recorded_dttm)
    },

    #' @description
    #' Summarize patient assessments data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Patient Assessments Summary")

      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)
      n_unique_assessments <- dplyr::n_distinct(self$df$assessment_category)

      cli::cli_text("Total assessments: {.val {n_total}}")
      cli::cli_text("Hospitalizations: {.val {n_hosp}}")
      cli::cli_text("Unique assessment types: {.val {n_unique_assessments}}")

      cli::cli_h3("Top 10 Assessment Types")
      assessment_dist <- self$get_assessment_distribution() %>%
        dplyr::slice_head(n = 10)
      print(assessment_dist)

      cli::cli_h3("Assessment Groups")
      group_dist <- self$get_assessment_distribution(by_group = TRUE)
      print(group_dist)

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        n_unique_assessments = n_unique_assessments,
        assessment_distribution = assessment_dist,
        group_distribution = group_dist
      ))
    }
  )
)
