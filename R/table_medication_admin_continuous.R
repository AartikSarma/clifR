#' MedicationAdminContinuous table
#'
#' @description
#' R6 class for the CLIF `medication_admin_continuous` table (continuous infusions
#' such as vasopressors, sedatives and insulin drips). Inherits all loading,
#' validation and summary behaviour from [BaseTable] and exposes the schema's
#' medication category-to-group mapping.
#' Port of `clifpy.tables.medication_admin_continuous.MedicationAdminContinuous`.
#'
#' @note clifpy declares `_acceptable_dose_unit_patterns` and
#' `resolve_mar_action_duplicates()` but leaves both as `pass`; they are therefore
#' not implemented here. Dose unit conversion lives in the unit converter utility
#' in both implementations.
#'
#' @export
#' @examples
#' \dontrun{
#' medication_table <- MedicationAdminContinuous$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' medication_table$med_category_to_group_mapping()
#' }
MedicationAdminContinuous <- R6::R6Class(
  classname = "MedicationAdminContinuous",
  inherit = BaseTable,
  public = list(
    #' @field df_converted Dose-converted data set by the orchestrator's
    #'   `convert_dose_units_for_continuous_meds()` when `save_to_table = TRUE`.
    df_converted = NULL,
    #' @field conversion_counts Per-unit conversion tallies accompanying `df_converted`.
    conversion_counts = NULL,

    #' @description Create a MedicationAdminContinuous table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `MedicationAdminContinuous` instance.
    initialize = function(data_directory = NULL,
                          filetype = NULL,
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          clif_version = DEFAULT_CLIF_VERSION) {
      super$initialize(
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        clif_version = clif_version
      )
      private$load_medication_schema_data()
    },

    #' @description Medication category to therapeutic group mapping from the schema.
    #' @return A named list mapping medication category to therapeutic group; empty
    #'   when the schema defines none.
    med_category_to_group_mapping = function() {
      private$med_category_to_group_map
    }
  ),
  private = list(
    med_category_to_group_map = list(),

    load_medication_schema_data = function() {
      if (is.null(self$schema)) {
        return(invisible(NULL))
      }
      private$med_category_to_group_map <- self$schema$med_category_to_group_mapping %||% list()
      invisible(NULL)
    }
  )
)
