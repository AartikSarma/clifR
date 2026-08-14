#' MicrobiologyNonculture table
#'
#' @description
#' R6 class for the CLIF microbiology_nonculture table (non-culture microbiology results).
#' Inherits all loading, validation and summary behaviour from [BaseTable].
#' Port of `clifpy.tables.microbiology_nonculture.MicrobiologyNonculture`.
#'
#' @export
#' @examples
#' \dontrun{
#' microbiology_nonculture_table <- MicrobiologyNonculture$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' microbiology_nonculture_table$validate()
#' }
MicrobiologyNonculture <- R6::R6Class(
  classname = "MicrobiologyNonculture",
  inherit = BaseTable,
  public = list(
    #' @description Create a MicrobiologyNonculture table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `MicrobiologyNonculture` instance.
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
