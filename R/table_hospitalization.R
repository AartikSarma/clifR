#' Hospitalization table
#'
#' @description
#' R6 class for the CLIF `hospitalization` table. Inherits all loading, validation
#' and summary behaviour from [BaseTable] and adds length-of-stay, mortality and
#' per-patient utilization summaries.
#' Port of `clifpy.tables.hospitalization.Hospitalization`.
#'
#' @export
#' @examples
#' \dontrun{
#' hospitalization_table <- Hospitalization$new(
#'   data_directory = "data/clif",
#'   filetype = "parquet",
#'   timezone = "US/Central"
#' )
#' hospitalization_table$get_mortality_rate()
#' }
Hospitalization <- R6::R6Class(
  classname = "Hospitalization",
  inherit = BaseTable,
  public = list(
    #' @description Create a Hospitalization table instance.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `<data_directory>/output`.
    #' @param data Optional pre-loaded data frame. When supplied, no file is read.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #' @return A new `Hospitalization` instance.
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

    #' @description Calculate length of stay for each hospitalization.
    #' @return A tibble: the hospitalization data plus a `length_of_stay_days`
    #'   column. Empty tibble when the required columns are missing.
    calculate_length_of_stay = function() {
      if (is.null(self$df)) {
        return(dplyr::tibble())
      }
      required_columns <- c("admission_dttm", "discharge_dttm")
      missing_columns <- setdiff(required_columns, names(self$df))
      if (length(missing_columns) > 0) {
        cli::cli_alert_warning("Missing required columns: {.val {missing_columns}}")
        return(dplyr::tibble())
      }

      self$df |>
        dplyr::mutate(
          length_of_stay_days = as.numeric(
            difftime(.data$discharge_dttm, .data$admission_dttm, units = "secs")
          ) / (24 * 3600)
        )
    },

    #' @description Calculate the in-hospital mortality rate.
    #' @return Percentage of hospitalizations with `discharge_category == "Expired"`.
    get_mortality_rate = function() {
      if (is.null(self$df) || !"discharge_category" %in% names(self$df)) {
        return(0)
      }
      total_hospitalizations <- nrow(self$df)
      if (total_hospitalizations == 0) {
        return(0)
      }
      expired_count <- sum(self$df$discharge_category == "Expired", na.rm = TRUE)
      (expired_count / total_hospitalizations) * 100
    },

    #' @description Comprehensive summary statistics for hospitalization data.
    #' @return A named list with totals, discharge/admission category counts, date
    #'   range, age statistics, length-of-stay statistics and mortality rate.
    get_summary_stats = function() {
      if (is.null(self$df)) {
        return(list())
      }

      summary_stats <- list(
        total_hospitalizations = nrow(self$df),
        unique_patients = if ("patient_id" %in% names(self$df)) {
          dplyr::n_distinct(self$df$patient_id, na.rm = TRUE)
        } else {
          0L
        },
        discharge_category_counts = if ("discharge_category" %in% names(self$df)) {
          value_counts_named_list(self$df$discharge_category)
        } else {
          list()
        },
        admission_type_counts = if ("admission_type_category" %in% names(self$df)) {
          value_counts_named_list(self$df$admission_type_category)
        } else {
          list()
        },
        date_range = list(
          earliest_admission = if ("admission_dttm" %in% names(self$df)) {
            column_min_or_na(self$df$admission_dttm)
          } else {
            NULL
          },
          latest_admission = if ("admission_dttm" %in% names(self$df)) {
            column_max_or_na(self$df$admission_dttm)
          } else {
            NULL
          },
          earliest_discharge = if ("discharge_dttm" %in% names(self$df)) {
            column_min_or_na(self$df$discharge_dttm)
          } else {
            NULL
          },
          latest_discharge = if ("discharge_dttm" %in% names(self$df)) {
            column_max_or_na(self$df$discharge_dttm)
          } else {
            NULL
          }
        )
      )

      if ("age_at_admission" %in% names(self$df)) {
        age_values <- self$df$age_at_admission[!is.na(self$df$age_at_admission)]
        if (length(age_values) > 0) {
          summary_stats$age_stats <- list(
            mean = round(mean(age_values), 1),
            median = stats::median(age_values),
            min = min(age_values),
            max = max(age_values),
            std = round(stats::sd(age_values), 1)
          )
        }
      }

      if (all(c("admission_dttm", "discharge_dttm") %in% names(self$df))) {
        length_of_stay_frame <- self$calculate_length_of_stay()
        if ("length_of_stay_days" %in% names(length_of_stay_frame)) {
          length_of_stay_values <- length_of_stay_frame$length_of_stay_days
          length_of_stay_values <- length_of_stay_values[!is.na(length_of_stay_values)]
          if (length(length_of_stay_values) > 0) {
            summary_stats$length_of_stay_stats <- list(
              mean_days = round(mean(length_of_stay_values), 1),
              median_days = round(stats::median(length_of_stay_values), 1),
              min_days = round(min(length_of_stay_values), 1),
              max_days = round(max(length_of_stay_values), 1),
              std_days = round(stats::sd(length_of_stay_values), 1)
            )
          }
        }
      }

      summary_stats$mortality_rate_percent <- round(self$get_mortality_rate(), 2)

      summary_stats
    },

    #' @description Hospitalization counts per patient.
    #' @return A tibble with columns `patient_id`, `hospitalization_count`,
    #'   `first_admission`, `last_admission` and `care_span_days`, sorted by
    #'   `hospitalization_count` descending. Empty tibble when `patient_id` is
    #'   missing.
    get_patient_hospitalization_counts = function() {
      if (is.null(self$df) || !"patient_id" %in% names(self$df)) {
        return(dplyr::tibble())
      }

      self$df |>
        dplyr::filter(!is.na(.data$patient_id)) |>
        dplyr::group_by(.data$patient_id) |>
        dplyr::summarise(
          hospitalization_count = sum(!is.na(.data$hospitalization_id)),
          first_admission = column_min_or_na(.data$admission_dttm),
          last_admission = column_max_or_na(.data$admission_dttm),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          care_span_days = as.numeric(
            difftime(.data$last_admission, .data$first_admission, units = "secs")
          ) / (24 * 3600)
        ) |>
        dplyr::arrange(dplyr::desc(.data$hospitalization_count))
    }
  )
)

#' Counts of unique values as a named list, sorted like pandas value_counts()
#'
#' Missing values are dropped and the result is ordered by descending count,
#' matching `Series.value_counts().to_dict()` in the Python implementation.
#'
#' @param values A vector.
#' @return A named list of integer counts.
#' @noRd
value_counts_named_list <- function(values) {
  non_missing_values <- values[!is.na(values)]
  if (length(non_missing_values) == 0) {
    return(list())
  }
  value_counts <- sort(table(non_missing_values), decreasing = TRUE)
  stats::setNames(as.list(as.integer(value_counts)), names(value_counts))
}

#' Minimum of a vector ignoring NA, preserving type; NA when all values missing
#'
#' Matches pandas `Series.min()` semantics (returns NaT/NaN for empty input)
#' without R's `Inf`-with-warning behaviour.
#'
#' @param values A vector (numeric or POSIXct).
#' @return The minimum value, or a typed `NA` when no non-missing values exist.
#' @noRd
column_min_or_na <- function(values) {
  non_missing_values <- values[!is.na(values)]
  if (length(non_missing_values) == 0) {
    return(values[NA_integer_])
  }
  min(non_missing_values)
}

#' Maximum of a vector ignoring NA, preserving type; NA when all values missing
#'
#' @param values A vector (numeric or POSIXct).
#' @return The maximum value, or a typed `NA` when no non-missing values exist.
#' @noRd
column_max_or_na <- function(values) {
  non_missing_values <- values[!is.na(values)]
  if (length(non_missing_values) == 0) {
    return(values[NA_integer_])
  }
  max(non_missing_values)
}
