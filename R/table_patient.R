#' Patient Table Class
#'
#' @description
#' R6 class for CLIF patient demographics data.
#' Inherits from BaseTable.
#'
#' @export
Patient <- R6::R6Class(
  classname = "Patient",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize a Patient table instance
    #'
    #' @param data_directory Character. Path to directory containing data files.
    #' @param filetype Character. File type: "csv" or "parquet" (default: "csv").
    #' @param timezone Character. Timezone for datetime columns (default: "UTC").
    #' @param output_directory Character. Path for saving outputs (default: NULL).
    #' @param data Optional tibble. Pre-loaded data (if NULL, loads from file).
    #' @param schema_dir Character. Custom schema directory (default: NULL).
    #'
    #' @return A new Patient table instance.
    initialize = function(data_directory = NULL,
                          filetype = "csv",
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          schema_dir = NULL) {

      super$initialize(
        table_name = "patient",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        schema_dir = schema_dir
      )
    },

    #' @description
    #' Get patient age at a specific date
    #'
    #' @param patient_id Character. Patient ID.
    #' @param reference_date POSIXct. Date for age calculation.
    #'
    #' @return Numeric age in years.
    get_age_at_date = function(patient_id, reference_date) {
      patient_row <- self$df %>%
        dplyr::filter(patient_id == !!patient_id)

      if (nrow(patient_row) == 0) {
        cli::cli_abort("Patient {.val {patient_id}} not found")
      }

      birth_date <- patient_row$birth_date[1]
      age_years <- as.numeric(difftime(reference_date, birth_date, units = "days")) / 365.25

      return(age_years)
    },

    #' @description
    #' Get demographic summary statistics
    #'
    #' @return List of demographic summaries.
    get_demographics_summary = function() {
      summary <- list(
        n_patients = nrow(self$df),
        sex_distribution = self$get_unique_values("sex_category"),
        race_distribution = self$get_unique_values("race_category"),
        ethnicity_distribution = self$get_unique_values("ethnicity_category"),
        n_deaths = sum(!is.na(self$df$death_dttm))
      )

      cli::cli_h3("Demographics Summary")
      cli::cli_text("Total patients: {.val {summary$n_patients}}")
      cli::cli_text("Deaths recorded: {.val {summary$n_deaths}}")

      return(invisible(summary))
    }
  )
)
