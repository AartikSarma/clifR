#' EcmoMcs table
#'
#' @description
#' R6 class for the CLIF ecmo_mcs table (ECMO and mechanical circulatory support device settings).
#' Inherits all loading, validation and summary behaviour from [BaseTable].
#' Port of `clifpy.tables.ecmo_mcs.EcmoMcs`.
#'
#' @note Under CLIF 3.0 this table was renamed and redesigned as `mcs`; the schema
#' loader resolves `ecmo_mcs` to `mcs_schema.yaml` when `clif_version = "3.0"`.
#'
#' @export
#' @examples
#' \dontrun{
#' ecmo_mcs_table <- EcmoMcs$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' ecmo_mcs_table$validate()
#' }
EcmoMcs <- R6::R6Class(
  classname = "EcmoMcs",
  inherit = BaseTable,
  public = list(
    #' @description Create a EcmoMcs table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `EcmoMcs` instance.
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
