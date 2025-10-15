#' Hospital Diagnosis Table Class
#'
#' @description
#' R6 class for CLIF hospital_diagnosis table containing diagnosis codes
#' (ICD-9 or ICD-10) assigned during hospitalization.
#'
#' @export
#' @importFrom R6 R6Class
HospitalDiagnosis <- R6::R6Class(
  classname = "HospitalDiagnosis",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize HospitalDiagnosis table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new HospitalDiagnosis instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "hospital_diagnosis",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Get diagnosis codes for a specific hospitalization
    #'
    #' @param hospitalization_id Character hospitalization ID.
    #'
    #' @return tibble of diagnosis codes for the hospitalization.
    get_diagnoses_for_hospitalization = function(hospitalization_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$hospitalization_id == !!hospitalization_id) %>%
        dplyr::arrange(dplyr::desc(.data$diagnosis_primary))
    },

    #' @description
    #' Get primary diagnosis for each hospitalization
    #'
    #' @return tibble with one row per hospitalization containing primary diagnosis.
    get_primary_diagnoses = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$diagnosis_primary == 1) %>%
        dplyr::select(
          hospitalization_id,
          diagnosis_code,
          diagnosis_code_format,
          poa_present
        )
    },

    #' @description
    #' Get counts by diagnosis code format (ICD-9 vs ICD-10)
    #'
    #' @return tibble with counts by format.
    get_format_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(diagnosis_code_format) %>%
        dplyr::summarize(
          n_diagnoses = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          n_primary = sum(diagnosis_primary == 1),
          n_poa = sum(poa_present == 1),
          .groups = "drop"
        )
    },

    #' @description
    #' Get most common diagnosis codes
    #'
    #' @param top_n Integer number of top codes to return (default: 10).
    #' @param primary_only Logical whether to include only primary diagnoses (default: FALSE).
    #'
    #' @return tibble with top diagnosis codes and counts.
    get_top_diagnoses = function(top_n = 10, primary_only = FALSE) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      data <- if (primary_only) {
        self$df %>% dplyr::filter(diagnosis_primary == 1)
      } else {
        self$df
      }

      data %>%
        dplyr::group_by(diagnosis_code, diagnosis_code_format) %>%
        dplyr::summarize(
          n_occurrences = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          n_primary = sum(diagnosis_primary == 1),
          n_poa = sum(poa_present == 1),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_occurrences)) %>%
        dplyr::slice_head(n = top_n)
    },

    #' @description
    #' Calculate Charlson Comorbidity Index scores
    #'
    #' @param age_col Character name of age column if joining with hospitalization data.
    #' @param hospitalization_data tibble hospitalization data with age (optional).
    #'
    #' @return tibble with CCI scores per hospitalization.
    calculate_cci_scores = function(age_col = "age_at_admission",
                                    hospitalization_data = NULL) {

      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      # Prepare data for CCI calculation
      cci_data <- self$df %>%
        dplyr::select(
          hospitalization_id,
          icd_code = diagnosis_code,
          icd_version = diagnosis_code_format
        ) %>%
        dplyr::mutate(
          icd_version = dplyr::case_when(
            icd_version == "ICD10CM" ~ "10",
            icd_version == "ICD9CM" ~ "9",
            TRUE ~ "10"
          )
        )

      # Add age if hospitalization data provided
      if (!is.null(hospitalization_data) && age_col %in% names(hospitalization_data)) {
        cci_data <- cci_data %>%
          dplyr::left_join(
            hospitalization_data %>%
              dplyr::select(hospitalization_id, age = !!rlang::sym(age_col)),
            by = "hospitalization_id"
          )
      } else {
        cci_data <- cci_data %>%
          dplyr::mutate(age = NA_real_)
      }

      # Calculate CCI scores
      cci_scores <- calculate_charlson_scores(
        data = cci_data,
        patient_id_col = "hospitalization_id",
        icd_code_col = "icd_code",
        icd_version_col = "icd_version",
        age_col = "age",
        default_icd_version = "10"
      )

      return(cci_scores)
    },

    #' @description
    #' Summarize diagnosis data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Hospital Diagnosis Summary")

      # Basic counts
      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)
      n_primary <- sum(self$df$diagnosis_primary == 1)
      n_poa <- sum(self$df$poa_present == 1)

      cli::cli_text("Total diagnoses: {.val {n_total}}")
      cli::cli_text("Unique hospitalizations: {.val {n_hosp}}")
      cli::cli_text("Primary diagnoses: {.val {n_primary}} ({round(n_primary/n_total*100, 1)}%)")
      cli::cli_text("Present on admission: {.val {n_poa}} ({round(n_poa/n_total*100, 1)}%)")

      # Format breakdown
      cli::cli_h3("Diagnosis Code Format")
      format_summary <- self$get_format_summary()
      print(format_summary)

      # Top diagnoses
      cli::cli_h3("Top 10 Most Common Diagnoses")
      top_dx <- self$get_top_diagnoses(top_n = 10)
      print(top_dx)

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        n_primary = n_primary,
        n_poa = n_poa,
        format_summary = format_summary,
        top_diagnoses = top_dx
      ))
    }
  )
)
