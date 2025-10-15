#' ADT (Admission/Discharge/Transfer) Table Class
#'
#' @description
#' R6 class for CLIF ADT event data.
#' Inherits from BaseTable.
#'
#' @export
Adt <- R6::R6Class(
  classname = "Adt",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize an ADT table instance
    #'
    #' @param data_directory Character. Path to directory containing data files.
    #' @param filetype Character. File type: "csv" or "parquet" (default: "csv").
    #' @param timezone Character. Timezone for datetime columns (default: "UTC").
    #' @param output_directory Character. Path for saving outputs (default: NULL).
    #' @param data Optional tibble. Pre-loaded data (if NULL, loads from file).
    #' @param schema_dir Character. Custom schema directory (default: NULL).
    #'
    #' @return A new ADT table instance.
    initialize = function(data_directory = NULL,
                          filetype = "csv",
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          schema_dir = NULL) {

      super$initialize(
        table_name = "adt",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        schema_dir = schema_dir
      )
    },

    #' @description
    #' Filter ADT events by location category
    #'
    #' @param location_category Character. Location category to filter (e.g., "icu").
    #'
    #' @return tibble of filtered ADT events.
    filter_by_location = function(location_category) {
      if (!"location_category" %in% names(self$df)) {
        cli::cli_abort("location_category column not found")
      }

      filtered <- self$df %>%
        dplyr::filter(location_category == !!location_category)

      cli::cli_alert_info(
        "Found {nrow(filtered)} ADT events for location: {.val {location_category}}"
      )

      return(filtered)
    },

    #' @description
    #' Get ICU stays
    #'
    #' @return tibble of ICU ADT events.
    get_icu_stays = function() {
      self$filter_by_location("icu")
    }
  )
)
