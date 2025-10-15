#' Medication Administration Continuous Table Class
#'
#' @description
#' R6 class for CLIF medication_admin_continuous table containing continuous
#' medication infusions (e.g., vasopressors, sedation, insulin drips).
#'
#' @export
#' @importFrom R6 R6Class
MedicationAdminContinuous <- R6::R6Class(
  classname = "MedicationAdminContinuous",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize MedicationAdminContinuous table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new MedicationAdminContinuous instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "medication_admin_continuous",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter medications by category
    #'
    #' @param med_category Character medication category.
    #'
    #' @return Filtered tibble.
    filter_by_medication = function(med_category) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$med_category == !!med_category)
    },

    #' @description
    #' Filter medications by group
    #'
    #' @param med_group Character medication group (e.g., "vasoactives", "sedation").
    #'
    #' @return Filtered tibble.
    filter_by_group = function(med_group) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$med_group == !!med_group)
    },

    #' @description
    #' Get medication summary statistics
    #'
    #' @param by_category Logical whether to summarize by category (default: TRUE).
    #'
    #' @return tibble with summary statistics.
    get_medication_summary = function(by_category = TRUE) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      if (by_category) {
        self$df %>%
          dplyr::group_by(med_category, med_group) %>%
          dplyr::summarize(
            n_administrations = dplyr::n(),
            n_hospitalizations = dplyr::n_distinct(hospitalization_id),
            n_unique_doses = dplyr::n_distinct(med_dose),
            mean_dose = mean(med_dose, na.rm = TRUE),
            median_dose = median(med_dose, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(dplyr::desc(n_administrations))
      } else {
        self$df %>%
          dplyr::group_by(med_group) %>%
          dplyr::summarize(
            n_administrations = dplyr::n(),
            n_medications = dplyr::n_distinct(med_category),
            n_hospitalizations = dplyr::n_distinct(hospitalization_id),
            .groups = "drop"
          ) %>%
          dplyr::arrange(dplyr::desc(n_administrations))
      }
    },

    #' @description
    #' Get vasopressor usage summary
    #'
    #' @return tibble with vasopressor statistics.
    get_vasopressor_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(med_group == "vasoactives") %>%
        dplyr::group_by(med_category) %>%
        dplyr::summarize(
          n_administrations = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          mean_dose = mean(med_dose, na.rm = TRUE),
          median_dose = median(med_dose, na.rm = TRUE),
          most_common_unit = names(sort(table(med_dose_unit), decreasing = TRUE))[1],
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_administrations))
    },

    #' @description
    #' Get sedation usage summary
    #'
    #' @return tibble with sedation statistics.
    get_sedation_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(med_group == "sedation") %>%
        dplyr::group_by(med_category) %>%
        dplyr::summarize(
          n_administrations = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          mean_dose = mean(med_dose, na.rm = TRUE),
          median_dose = median(med_dose, na.rm = TRUE),
          most_common_unit = names(sort(table(med_dose_unit), decreasing = TRUE))[1],
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_administrations))
    },

    #' @description
    #' Calculate medication duration for each hospitalization
    #'
    #' @param med_category Character medication category (optional, NULL for all).
    #'
    #' @return tibble with duration statistics.
    calculate_medication_duration = function(med_category = NULL) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      data <- if (!is.null(med_category)) {
        self$df %>% dplyr::filter(.data$med_category == !!med_category)
      } else {
        self$df
      }

      data %>%
        dplyr::group_by(hospitalization_id, med_category) %>%
        dplyr::summarize(
          first_admin = min(admin_dttm, na.rm = TRUE),
          last_admin = max(admin_dttm, na.rm = TRUE),
          n_administrations = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          duration_hours = as.numeric(
            difftime(last_admin, first_admin, units = "hours")
          )
        )
    },

    #' @description
    #' Convert medication doses to standard units
    #'
    #' @param target_units Named list of target units by medication.
    #' @param weight_col Character weight column name (default: NULL).
    #'
    #' @return tibble with converted doses.
    convert_doses = function(target_units, weight_col = NULL) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      convert_medication_doses(
        med_data = self$df,
        dose_col = "med_dose",
        unit_col = "med_dose_unit",
        target_units = target_units,
        weight_col = weight_col,
        medication_col = "med_category"
      )
    },

    #' @description
    #' Summarize medication administration data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Medication Administration (Continuous) Summary")

      # Basic counts
      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)
      n_meds <- dplyr::n_distinct(self$df$med_category)
      n_groups <- dplyr::n_distinct(self$df$med_group)

      cli::cli_text("Total administrations: {.val {n_total}}")
      cli::cli_text("Unique hospitalizations: {.val {n_hosp}}")
      cli::cli_text("Unique medications: {.val {n_meds}}")
      cli::cli_text("Medication groups: {.val {n_groups}}")

      # Group summary
      cli::cli_h3("Summary by Medication Group")
      group_summary <- self$get_medication_summary(by_category = FALSE)
      print(group_summary)

      # Vasopressor summary
      if ("vasoactives" %in% self$df$med_group) {
        cli::cli_h3("Vasopressor Usage")
        vaso_summary <- self$get_vasopressor_summary()
        print(vaso_summary)
      }

      # Sedation summary
      if ("sedation" %in% self$df$med_group) {
        cli::cli_h3("Sedation Usage")
        sed_summary <- self$get_sedation_summary()
        print(sed_summary)
      }

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        n_medications = n_meds,
        group_summary = group_summary
      ))
    }
  )
)
