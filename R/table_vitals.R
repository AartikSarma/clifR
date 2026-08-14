#' Vitals table
#'
#' @description
#' R6 class for the CLIF `vitals` table. Inherits all loading, validation and
#' summary behaviour from [BaseTable] and adds category filtering, per-category
#' summary statistics and accessors for the schema's expected units and ranges.
#' Port of `clifpy.tables.vitals.Vitals`.
#'
#' @note clifpy defines `validate_vital_ranges()` but leaves it commented out, so
#' range checking is not part of validation in either implementation. The
#' `vital_ranges()` accessor exposes the schema values for callers that want to
#' apply their own checks.
#'
#' @export
#' @examples
#' \dontrun{
#' vitals_table <- Vitals$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' vitals_table$filter_by_vital_category("heart_rate")
#' vitals_table$get_vital_summary_stats()
#' }
Vitals <- R6::R6Class(
  classname = "Vitals",
  inherit = BaseTable,
  public = list(
    #' @description Create a Vitals table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `Vitals` instance.
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
      private$load_vitals_schema_data()
    },

    #' @description Expected measurement unit for each vital category.
    #' @return A named list mapping vital category to unit, empty when the schema
    #'   defines none.
    vital_units = function() {
      private$vital_units_map
    },

    #' @description Expected value range for each vital category.
    #' @return A named list of `list(min = , max = )` entries, empty when the
    #'   schema defines none.
    vital_ranges = function() {
      private$vital_ranges_map
    },

    #' @description All records for one vital category.
    #' @param vital_category Category to filter on, e.g. `"heart_rate"`.
    #' @return A tibble of matching rows; empty when the column is absent.
    filter_by_vital_category = function(vital_category) {
      if (is.null(self$df) || !"vital_category" %in% names(self$df)) {
        return(dplyr::tibble())
      }
      dplyr::filter(self$df, .data$vital_category == !!vital_category)
    },

    #' @description Summary statistics for each vital category.
    #' @return A tibble with one row per `vital_category` and columns `count`,
    #'   `mean`, `std`, `min`, `max`, `q1`, `median` and `q3`, rounded to 2
    #'   decimals. Empty tibble when `vital_value` is absent.
    get_vital_summary_stats = function() {
      if (is.null(self$df) || !"vital_value" %in% names(self$df)) {
        return(dplyr::tibble())
      }

      numeric_vital_values <- suppressWarnings(as.numeric(self$df$vital_value))

      self$df |>
        dplyr::mutate(vital_value = numeric_vital_values) |>
        dplyr::filter(!is.na(.data$vital_category)) |>
        dplyr::group_by(.data$vital_category) |>
        dplyr::summarise(
          count = sum(!is.na(.data$vital_value)),
          mean = round(mean(.data$vital_value, na.rm = TRUE), 2),
          std = round(stats::sd(.data$vital_value, na.rm = TRUE), 2),
          min = round(column_min_or_na(.data$vital_value), 2),
          max = round(column_max_or_na(.data$vital_value), 2),
          q1 = round(
            stats::quantile(.data$vital_value, 0.25, na.rm = TRUE, names = FALSE, type = 7),
            2
          ),
          median = round(
            stats::quantile(.data$vital_value, 0.5, na.rm = TRUE, names = FALSE, type = 7),
            2
          ),
          q3 = round(
            stats::quantile(.data$vital_value, 0.75, na.rm = TRUE, names = FALSE, type = 7),
            2
          ),
          .groups = "drop"
        ) |>
        dplyr::arrange(.data$vital_category)
    }
  ),
  private = list(
    vital_units_map = list(),
    vital_ranges_map = list(),

    load_vitals_schema_data = function() {
      if (is.null(self$schema)) {
        return(invisible(NULL))
      }
      private$vital_units_map <- self$schema$vital_units %||% list()
      private$vital_ranges_map <- self$schema$vital_ranges %||% list()
      invisible(NULL)
    }
  )
)
