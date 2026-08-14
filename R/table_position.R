#' Position table
#'
#' @description
#' R6 class for the CLIF `position` table (patient positioning, chiefly prone vs
#' not prone for ARDS management). Inherits all loading, validation and summary
#' behaviour from [BaseTable] and adds per-category counts.
#' Port of `clifpy.tables.position.Position`.
#'
#' @export
#' @examples
#' \dontrun{
#' position_table <- Position$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' position_table$get_position_category_stats()
#' }
Position <- R6::R6Class(
  classname = "Position",
  inherit = BaseTable,
  public = list(
    #' @description Create a Position table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `Position` instance.
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

    #' @description Record and hospitalization counts for each position category.
    #' @return A tibble with columns `position_category`, `count` and `unique`
    #'   (distinct hospitalizations). A named list
    #'   `list(status = "Missing columns")` when `position_category` or
    #'   `hospitalization_id` is absent.
    get_position_category_stats = function() {
      if (is.null(self$df) ||
          !"position_category" %in% names(self$df) ||
          !"hospitalization_id" %in% names(self$df)) {
        return(list(status = "Missing columns"))
      }

      self$df |>
        dplyr::filter(!is.na(.data$position_category)) |>
        dplyr::group_by(.data$position_category) |>
        dplyr::summarise(
          count = sum(!is.na(.data$position_category)),
          unique = dplyr::n_distinct(.data$hospitalization_id, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(.data$position_category)
    }
  )
)
