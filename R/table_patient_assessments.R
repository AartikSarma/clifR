#' PatientAssessments table
#'
#' @description
#' R6 class for the CLIF `patient_assessments` table (GCS, RASS, pain and delirium
#' scores, SAT/SBT records and similar). Inherits all loading, validation and
#' summary behaviour from [BaseTable] and exposes the schema's assessment
#' category-to-group mapping.
#' Port of `clifpy.tables.patient_assessments.PatientAssessments`.
#'
#' @export
#' @examples
#' \dontrun{
#' assessments_table <- PatientAssessments$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' assessments_table$assessment_category_to_group_mapping()
#' }
PatientAssessments <- R6::R6Class(
  classname = "PatientAssessments",
  inherit = BaseTable,
  public = list(
    #' @description Create a PatientAssessments table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `PatientAssessments` instance.
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
      private$load_assessment_schema_data()
    },

    #' @description Assessment category to group mapping from the schema.
    #' @return A named list mapping assessment category to assessment group; empty
    #'   when the schema defines none.
    assessment_category_to_group_mapping = function() {
      private$assessment_category_to_group_map
    }
  ),
  private = list(
    assessment_category_to_group_map = list(),

    load_assessment_schema_data = function() {
      if (is.null(self$schema)) {
        return(invisible(NULL))
      }
      private$assessment_category_to_group_map <-
        self$schema$assessment_category_to_group_mapping %||% list()
      invisible(NULL)
    }
  )
)
