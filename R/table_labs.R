#' Labs Table Class
#'
#' @description
#' R6 class for CLIF laboratory results data.
#' Inherits from BaseTable.
#'
#' @export
Labs <- R6::R6Class(
  classname = "Labs",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize a Labs table instance
    #'
    #' @param data_directory Character. Path to directory containing data files.
    #' @param filetype Character. File type: "csv" or "parquet" (default: "csv").
    #' @param timezone Character. Timezone for datetime columns (default: "UTC").
    #' @param output_directory Character. Path for saving outputs (default: NULL).
    #' @param data Optional tibble. Pre-loaded data (if NULL, loads from file).
    #' @param schema_dir Character. Custom schema directory (default: NULL).
    #'
    #' @return A new Labs table instance.
    initialize = function(data_directory = NULL,
                          filetype = "csv",
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          schema_dir = NULL) {

      super$initialize(
        table_name = "labs",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        schema_dir = schema_dir
      )
    },

    #' @description
    #' Filter labs by category
    #'
    #' @param lab_category Character. Lab category to filter.
    #'
    #' @return tibble of filtered lab results.
    filter_by_category = function(lab_category) {
      if (!"lab_category" %in% names(self$df)) {
        cli::cli_abort("lab_category column not found")
      }

      filtered <- self$df %>%
        dplyr::filter(lab_category == !!lab_category)

      cli::cli_alert_info(
        "Found {nrow(filtered)} lab results for category: {.val {lab_category}}"
      )

      return(filtered)
    },

    #' @description
    #' Get summary statistics for a specific lab
    #'
    #' @param lab_category Character. Lab category to summarize.
    #'
    #' @return List of summary statistics.
    get_lab_summary = function(lab_category) {
      lab_data <- self$filter_by_category(lab_category)

      if (!"lab_value" %in% names(lab_data)) {
        cli::cli_abort("lab_value column not found")
      }

      summary <- list(
        lab_category = lab_category,
        n_results = nrow(lab_data),
        mean = mean(lab_data$lab_value, na.rm = TRUE),
        median = median(lab_data$lab_value, na.rm = TRUE),
        sd = sd(lab_data$lab_value, na.rm = TRUE),
        min = min(lab_data$lab_value, na.rm = TRUE),
        max = max(lab_data$lab_value, na.rm = TRUE),
        n_missing = sum(is.na(lab_data$lab_value))
      )

      cli::cli_h3("Lab Summary: {lab_category}")
      cli::cli_text("N: {.val {summary$n_results}}")
      cli::cli_text("Mean: {.val {round(summary$mean, 2)}}")
      cli::cli_text("Range: [{.val {round(summary$min, 2)}}, {.val {round(summary$max, 2)}}]")

      return(invisible(summary))
    }
  )
)
