#' Hospitalization Table Class
#'
#' @description
#' R6 class for CLIF hospitalization data.
#' Inherits from BaseTable.
#'
#' @export
Hospitalization <- R6::R6Class(
  classname = "Hospitalization",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize a Hospitalization table instance
    #'
    #' @param data_directory Character. Path to directory containing data files.
    #' @param filetype Character. File type: "csv" or "parquet" (default: "csv").
    #' @param timezone Character. Timezone for datetime columns (default: "UTC").
    #' @param output_directory Character. Path for saving outputs (default: NULL).
    #' @param data Optional tibble. Pre-loaded data (if NULL, loads from file).
    #' @param schema_dir Character. Custom schema directory (default: NULL).
    #'
    #' @return A new Hospitalization table instance.
    initialize = function(data_directory = NULL,
                          filetype = "csv",
                          timezone = "UTC",
                          output_directory = NULL,
                          data = NULL,
                          schema_dir = NULL) {

      super$initialize(
        table_name = "hospitalization",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory,
        data = data,
        schema_dir = schema_dir
      )
    },

    #' @description
    #' Calculate length of stay for each hospitalization
    #'
    #' @param units Character. Time units ("days", "hours", "mins").
    #'
    #' @return tibble with hospitalization_id and length_of_stay.
    calculate_length_of_stay = function(units = "days") {
      self$df %>%
        dplyr::mutate(
          length_of_stay = as.numeric(
            difftime(discharge_dttm, admission_dttm, units = units)
          )
        ) %>%
        dplyr::select(hospitalization_id, length_of_stay, admission_dttm, discharge_dttm)
    },

    #' @description
    #' Get mortality rate
    #'
    #' @return Numeric mortality rate (proportion).
    get_mortality_rate = function() {
      if (!"discharge_category" %in% names(self$df)) {
        cli::cli_abort("discharge_category column not found")
      }

      n_expired <- sum(self$df$discharge_category == "Expired", na.rm = TRUE)
      total <- nrow(self$df)

      mortality_rate <- n_expired / total

      cli::cli_alert_info(
        "Mortality rate: {.val {round(mortality_rate * 100, 2)}}% ({n_expired}/{total})"
      )

      return(mortality_rate)
    },

    #' @description
    #' Get summary statistics for hospitalizations
    #'
    #' @return List of summary statistics.
    get_summary_stats = function() {
      los_data <- self$calculate_length_of_stay(units = "days")

      summary <- list(
        n_hospitalizations = nrow(self$df),
        los_mean = mean(los_data$length_of_stay, na.rm = TRUE),
        los_median = median(los_data$length_of_stay, na.rm = TRUE),
        los_sd = sd(los_data$length_of_stay, na.rm = TRUE),
        age_mean = mean(self$df$age_at_admission, na.rm = TRUE),
        age_median = median(self$df$age_at_admission, na.rm = TRUE),
        mortality_rate = self$get_mortality_rate()
      )

      cli::cli_h3("Hospitalization Summary")
      cli::cli_text("N: {.val {summary$n_hospitalizations}}")
      cli::cli_text("LOS (days): {.val {round(summary$los_mean, 1)}} \u00b1 {.val {round(summary$los_sd, 1)}}")
      cli::cli_text("Age: {.val {round(summary$age_mean, 1)}} years")

      return(invisible(summary))
    }
  )
)
