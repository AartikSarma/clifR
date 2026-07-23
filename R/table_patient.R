#' Patient table
#'
#' @description
#' R6 class for the CLIF `patient` table (demographics and identifiers).
#' Inherits all loading, validation and summary behaviour from [BaseTable].
#' Port of `clifpy.tables.patient.Patient`.
#'
#' @export
#' @examples
#' \dontrun{
#' patient_table <- Patient$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' patient_table$validate()
#' }
Patient <- R6::R6Class(
  classname = "Patient",
  inherit = BaseTable,
  public = list(
    #' @description Create a Patient table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `Patient` instance.
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
    }
  )
)
