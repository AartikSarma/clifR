#' Patient Procedures Table Class
#'
#' @description
#' R6 class for CLIF patient_procedures table containing billed procedural codes
#' (CPT, ICD-10-PCS, HCPCS) performed during hospitalization.
#'
#' @export
#' @importFrom R6 R6Class
PatientProcedures <- R6::R6Class(
  classname = "PatientProcedures",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize PatientProcedures table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new PatientProcedures instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "patient_procedures",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter by procedure code
    #'
    #' @param procedure_code Character procedure code to filter by.
    #'
    #' @return Filtered tibble.
    filter_by_code = function(procedure_code) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$procedure_code == !!procedure_code)
    },

    #' @description
    #' Filter by code format
    #'
    #' @param code_format Character code format (CPT, ICD10PCS, HCPCS).
    #'
    #' @return Filtered tibble.
    filter_by_format = function(code_format) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$procedure_code_format == !!code_format)
    },

    #' @description
    #' Get CPT codes
    #'
    #' @return tibble with CPT procedure codes.
    get_cpt_codes = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$procedure_code_format == "CPT")
    },

    #' @description
    #' Get ICD-10-PCS codes
    #'
    #' @return tibble with ICD-10-PCS procedure codes.
    get_icd10pcs_codes = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$procedure_code_format == "ICD10PCS")
    },

    #' @description
    #' Get HCPCS codes
    #'
    #' @return tibble with HCPCS procedure codes.
    get_hcpcs_codes = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$procedure_code_format == "HCPCS")
    },

    #' @description
    #' Get procedure distribution
    #'
    #' @param by_format Logical, summarize by code format instead of specific codes.
    #' @param top_n Integer number of top procedures to return.
    #'
    #' @return tibble with procedure counts.
    get_procedure_distribution = function(by_format = FALSE, top_n = 20) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      if (by_format) {
        self$df %>%
          dplyr::group_by(procedure_code_format) %>%
          dplyr::summarize(
            n_procedures = dplyr::n(),
            n_hospitalizations = dplyr::n_distinct(hospitalization_id),
            .groups = "drop"
          ) %>%
          dplyr::arrange(dplyr::desc(n_procedures))
      } else {
        self$df %>%
          dplyr::group_by(procedure_code, procedure_code_format) %>%
          dplyr::summarize(
            n_procedures = dplyr::n(),
            n_hospitalizations = dplyr::n_distinct(hospitalization_id),
            .groups = "drop"
          ) %>%
          dplyr::arrange(dplyr::desc(n_procedures)) %>%
          dplyr::slice_head(n = top_n)
      }
    },

    #' @description
    #' Get procedures for specific hospitalization
    #'
    #' @param hospitalization_id Character hospitalization ID.
    #'
    #' @return tibble with hospitalization's procedures.
    get_hospitalization_procedures = function(hospitalization_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$hospitalization_id == !!hospitalization_id) %>%
        dplyr::arrange(procedure_billed_dttm)
    },

    #' @description
    #' Calculate procedure counts per hospitalization
    #'
    #' @return tibble with procedure count statistics.
    calculate_procedures_per_hospitalization = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          n_procedures = dplyr::n(),
          n_unique_codes = dplyr::n_distinct(procedure_code),
          first_procedure_dttm = min(procedure_billed_dttm, na.rm = TRUE),
          last_procedure_dttm = max(procedure_billed_dttm, na.rm = TRUE),
          .groups = "drop"
        )
    },

    #' @description
    #' Get procedures by provider
    #'
    #' @param provider_type Character "performing" or "billing".
    #'
    #' @return tibble with provider procedure statistics.
    get_provider_summary = function(provider_type = "performing") {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      provider_col <- if (provider_type == "performing") {
        "performing_provider_id"
      } else {
        "billing_provider_id"
      }

      self$df %>%
        dplyr::group_by(!!rlang::sym(provider_col)) %>%
        dplyr::summarize(
          n_procedures = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_procedures))
    },

    #' @description
    #' Summarize patient procedures data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Patient Procedures Summary")

      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)
      n_unique_codes <- dplyr::n_distinct(self$df$procedure_code)

      cli::cli_text("Total procedures: {.val {n_total}}")
      cli::cli_text("Hospitalizations: {.val {n_hosp}}")
      cli::cli_text("Unique procedure codes: {.val {n_unique_codes}}")

      cli::cli_h3("Code Format Distribution")
      format_dist <- self$get_procedure_distribution(by_format = TRUE)
      print(format_dist)

      cli::cli_h3("Top 10 Procedure Codes")
      code_dist <- self$get_procedure_distribution(top_n = 10)
      print(code_dist)

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        n_unique_codes = n_unique_codes,
        format_distribution = format_dist,
        code_distribution = code_dist
      ))
    }
  )
)
