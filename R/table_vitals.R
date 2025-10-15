#' Vitals Table Class
#'
#' @description
#' R6 class for CLIF vital signs data.
#' Inherits from BaseTable.
#'
#' @export
Vitals <- R6::R6Class(
  classname = "Vitals",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize a Vitals table instance
    #'
    #' @param data_directory Character. Path to directory containing data files.
    #' @param filetype Character. File type: "csv" or "parquet" (default: "csv").
    #' @param timezone Character. Timezone for datetime columns (default: "UTC").
    #' @param output_directory Character. Path for saving outputs (default: NULL).
    #' @param data Optional tibble. Pre-loaded data (if NULL, loads from file).
    #' @param schema_dir Character. Custom schema directory (default: NULL).
    #'
    #' @return A new Vitals table instance.
    initialize = function(data_directory = NULL,
                          filetype = "csv",
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          schema_dir = NULL) {

      super$initialize(
        table_name = "vitals",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        schema_dir = schema_dir
      )
    },

    #' @description
    #' Filter vitals by category
    #'
    #' @param vital_category Character. Vital category to filter.
    #'
    #' @return tibble of filtered vital signs.
    filter_by_category = function(vital_category) {
      if (!"vital_category" %in% names(self$df)) {
        cli::cli_abort("vital_category column not found")
      }

      filtered <- self$df %>%
        dplyr::filter(vital_category == !!vital_category)

      cli::cli_alert_info(
        "Found {nrow(filtered)} vital signs for category: {.val {vital_category}}"
      )

      return(filtered)
    },

    #' @description
    #' Get summary statistics for a specific vital sign
    #'
    #' @param vital_category Character. Vital category to summarize.
    #'
    #' @return List of summary statistics.
    get_vital_summary = function(vital_category) {
      vital_data <- self$filter_by_category(vital_category)

      if (!"vital_value" %in% names(vital_data)) {
        cli::cli_abort("vital_value column not found")
      }

      summary <- list(
        vital_category = vital_category,
        n_measurements = nrow(vital_data),
        mean = mean(vital_data$vital_value, na.rm = TRUE),
        median = median(vital_data$vital_value, na.rm = TRUE),
        sd = sd(vital_data$vital_value, na.rm = TRUE),
        min = min(vital_data$vital_value, na.rm = TRUE),
        max = max(vital_data$vital_value, na.rm = TRUE),
        n_missing = sum(is.na(vital_data$vital_value))
      )

      cli::cli_h3("Vital Summary: {vital_category}")
      cli::cli_text("N: {.val {summary$n_measurements}}")
      cli::cli_text("Mean: {.val {round(summary$mean, 2)}}")
      cli::cli_text("Range: [{.val {round(summary$min, 2)}}, {.val {round(summary$max, 2)}}]")

      return(invisible(summary))
    },

    #' @description
    #' Calculate mean arterial pressure (MAP) if not present
    #'
    #' @return tibble with calculated MAP values.
    calculate_map = function() {
      # Check if we have SBP and DBP
      sbp_data <- self$filter_by_category("sbp")
      dbp_data <- self$filter_by_category("dbp")

      if (nrow(sbp_data) == 0 || nrow(dbp_data) == 0) {
        cli::cli_abort("Need both SBP and DBP to calculate MAP")
      }

      # Join and calculate MAP
      map_data <- sbp_data %>%
        dplyr::inner_join(
          dbp_data,
          by = c("hospitalization_id", "recorded_dttm"),
          suffix = c("_sbp", "_dbp")
        ) %>%
        dplyr::mutate(
          vital_category = "map",
          vital_value = (vital_value_sbp + 2 * vital_value_dbp) / 3
        ) %>%
        dplyr::select(hospitalization_id, recorded_dttm, vital_category, vital_value)

      cli::cli_alert_success("Calculated {nrow(map_data)} MAP values")

      return(map_data)
    }
  )
)
