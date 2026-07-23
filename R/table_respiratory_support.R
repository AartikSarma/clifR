#' RespiratorySupport table
#'
#' @description
#' R6 class for the CLIF `respiratory_support` table (device categories, modes and
#' ventilator settings). Inherits all loading, validation and summary behaviour
#' from [BaseTable] and adds the waterfall cleaning pipeline.
#' Port of `clifpy.tables.respiratory_support.RespiratorySupport`.
#'
#' @export
#' @examples
#' \dontrun{
#' respiratory_table <- RespiratorySupport$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' waterfalled <- respiratory_table$waterfall()
#' }
RespiratorySupport <- R6::R6Class(
  classname = "RespiratorySupport",
  inherit = BaseTable,
  public = list(
    #' @description Create a RespiratorySupport table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `RespiratorySupport` instance.
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
    },

    #' @description Clean and waterfall-fill the respiratory support table.
    #'
    #' The waterfall processor expects UTC timestamps, so `recorded_dttm` is
    #' converted to UTC for processing and relabelled back to its original zone
    #' afterwards. The original object is never modified.
    #'
    #' @param id_col Encounter-level identifier column.
    #' @param bfill If `TRUE`, numeric setters are back-filled after forward-fill.
    #' @param verbose Whether to print progress messages.
    #' @param return_dataframe If `TRUE`, return the processed tibble instead of a
    #'   new table object.
    #' @return A new `RespiratorySupport` instance holding the processed data, or
    #'   the processed tibble when `return_dataframe = TRUE`.
    waterfall = function(id_col = "hospitalization_id",
                         bfill = FALSE,
                         verbose = TRUE,
                         return_dataframe = FALSE) {
      if (is.null(self$df) || nrow(self$df) == 0) {
        cli::cli_abort("No data available to process. Load data first.")
      }

      working_data <- self$df

      original_timezone <- NULL
      if ("recorded_dttm" %in% names(working_data) &&
          inherits(working_data$recorded_dttm, "POSIXct")) {
        original_timezone <- attr(working_data$recorded_dttm, "tzone")
        if (is.null(original_timezone) || !nzchar(original_timezone)) {
          original_timezone <- NULL
        } else {
          if (verbose && original_timezone != "UTC") {
            cli::cli_alert_info(
              "Converting timezone from {.val {original_timezone}} to UTC for waterfall processing"
            )
          }
          attr(working_data$recorded_dttm, "tzone") <- "UTC"
        }
      }

      processed_data <- process_resp_support_waterfall(
        working_data,
        id_col = id_col,
        bfill = bfill,
        verbose = verbose
      )

      if (!is.null(original_timezone) && "recorded_dttm" %in% names(processed_data)) {
        if (verbose && original_timezone != "UTC") {
          cli::cli_alert_info(
            "Converting timezone from UTC back to {.val {original_timezone}} after processing"
          )
        }
        attr(processed_data$recorded_dttm, "tzone") <- original_timezone
      }

      if (return_dataframe) {
        return(dplyr::as_tibble(processed_data))
      }

      RespiratorySupport$new(
        data_directory = self$data_directory,
        filetype = self$filetype,
        timezone = self$timezone,
        output_directory = self$output_directory,
        data = processed_data,
        clif_version = self$clif_version
      )
    }
  )
)
