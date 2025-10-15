#' Medication Administration Intermittent Table Class
#'
#' @description
#' R6 class for CLIF medication_admin_intermittent table containing intermittent
#' medication doses (e.g., antibiotics, scheduled medications, PRN doses).
#'
#' @export
#' @importFrom R6 R6Class
MedicationAdminIntermittent <- R6::R6Class(
  classname = "MedicationAdminIntermittent",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize MedicationAdminIntermittent table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new MedicationAdminIntermittent instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "medication_admin_intermittent",
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
    #' @param med_group Character medication group.
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
    #' Get antibiotic administrations
    #'
    #' @return tibble with antibiotic administrations.
    get_antibiotics = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(med_group == "CMS_sepsis_qualifying_antibiotics")
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
    #' Get antibiotic summary
    #'
    #' @return tibble with antibiotic statistics.
    get_antibiotic_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(med_group == "CMS_sepsis_qualifying_antibiotics") %>%
        dplyr::group_by(med_category) %>%
        dplyr::summarize(
          n_administrations = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          mean_dose = mean(med_dose, na.rm = TRUE),
          median_dose = median(med_dose, na.rm = TRUE),
          most_common_unit = names(sort(table(med_dose_unit), decreasing = TRUE))[1],
          most_common_route = names(sort(table(med_route_category), decreasing = TRUE))[1],
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_administrations))
    },

    #' @description
    #' Calculate time to first antibiotic
    #'
    #' @param reference_time_col Character column name for reference time (e.g., admission_dttm).
    #' @param hospitalization_data tibble hospitalization data with reference time.
    #'
    #' @return tibble with time to first antibiotic.
    calculate_time_to_antibiotic = function(reference_time_col = "admission_dttm",
                                           hospitalization_data) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      if (missing(hospitalization_data)) {
        cli::cli_abort("hospitalization_data required")
      }

      # Get first antibiotic time per hospitalization
      first_abx <- self$df %>%
        dplyr::filter(med_group == "CMS_sepsis_qualifying_antibiotics") %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          first_abx_time = min(admin_dttm, na.rm = TRUE),
          first_abx_category = dplyr::first(med_category[admin_dttm == first_abx_time]),
          .groups = "drop"
        )

      # Join with reference time
      result <- hospitalization_data %>%
        dplyr::select(
          hospitalization_id,
          reference_time = !!rlang::sym(reference_time_col)
        ) %>%
        dplyr::inner_join(first_abx, by = "hospitalization_id") %>%
        dplyr::mutate(
          time_to_abx_hours = as.numeric(
            difftime(first_abx_time, reference_time, units = "hours")
          )
        )

      return(result)
    },

    #' @description
    #' Get medication administration frequency
    #'
    #' @param med_category Character medication category.
    #' @param time_window_hours Numeric time window for frequency calculation (default: 24).
    #'
    #' @return tibble with administration frequencies.
    calculate_administration_frequency = function(med_category = NULL,
                                                 time_window_hours = 24) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      data <- if (!is.null(med_category)) {
        self$df %>% dplyr::filter(.data$med_category == !!med_category)
      } else {
        self$df
      }

      data %>%
        dplyr::arrange(hospitalization_id, med_category, admin_dttm) %>%
        dplyr::group_by(hospitalization_id, med_category) %>%
        dplyr::summarize(
          n_doses = dplyr::n(),
          first_dose = min(admin_dttm, na.rm = TRUE),
          last_dose = max(admin_dttm, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          duration_hours = as.numeric(
            difftime(last_dose, first_dose, units = "hours")
          ),
          avg_frequency_per_day = ifelse(
            duration_hours > 0,
            n_doses / (duration_hours / 24),
            NA_real_
          )
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

      cli::cli_h2("Medication Administration (Intermittent) Summary")

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

      # Antibiotic summary
      if ("CMS_sepsis_qualifying_antibiotics" %in% self$df$med_group) {
        cli::cli_h3("Antibiotic Usage (Top 10)")
        abx_summary <- self$get_antibiotic_summary()
        print(head(abx_summary, 10))
      }

      # Route summary
      cli::cli_h3("Administration Routes")
      route_summary <- self$df %>%
        dplyr::group_by(med_route_category) %>%
        dplyr::summarize(
          n_administrations = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_administrations))
      print(route_summary)

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        n_medications = n_meds,
        group_summary = group_summary
      ))
    }
  )
)
