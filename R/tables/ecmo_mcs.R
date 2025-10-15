#' ECMO/MCS Table Class
#'
#' @description
#' R6 class for CLIF ecmo_mcs table containing ECMO (extracorporeal membrane
#' oxygenation) and mechanical circulatory support device data.
#'
#' @export
#' @importFrom R6 R6Class
EcmoMcs <- R6::R6Class(
  classname = "EcmoMcs",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize EcmoMcs table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new EcmoMcs instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "ecmo_mcs",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter by device category
    #'
    #' @param device_category Character device category to filter by.
    #'
    #' @return Filtered tibble.
    filter_by_device = function(device_category) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$device_category == !!device_category)
    },

    #' @description
    #' Filter by MCS group
    #'
    #' @param mcs_group Character MCS group (ECMO, IABP, RVAD, durable_LVAD, temporary_LVAD).
    #'
    #' @return Filtered tibble.
    filter_by_group = function(mcs_group) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$mcs_group == !!mcs_group)
    },

    #' @description
    #' Get ECMO records (all ECMO types)
    #'
    #' @return tibble with ECMO records.
    get_ecmo_records = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$mcs_group == "ECMO")
    },

    #' @description
    #' Get device summary
    #'
    #' @param by_group Logical, summarize by mcs_group instead of device_category.
    #'
    #' @return tibble with device usage statistics.
    get_device_summary = function(by_group = FALSE) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      grouping_var <- if (by_group) "mcs_group" else "device_category"

      self$df %>%
        dplyr::group_by(!!rlang::sym(grouping_var)) %>%
        dplyr::summarize(
          n_observations = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          mean_flow = mean(flow, na.rm = TRUE),
          mean_device_rate = mean(device_rate, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_observations))
    },

    #' @description
    #' Calculate ECMO duration per hospitalization
    #'
    #' @param device_category Optional character, specific device to calculate duration for.
    #'
    #' @return tibble with ECMO/MCS duration statistics.
    calculate_device_duration = function(device_category = NULL) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      data <- self$df
      if (!is.null(device_category)) {
        data <- data %>%
          dplyr::filter(.data$device_category == !!device_category)
      }

      data %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          first_record = min(recorded_dttm, na.rm = TRUE),
          last_record = max(recorded_dttm, na.rm = TRUE),
          n_observations = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          duration_hours = as.numeric(
            difftime(last_record, first_record, units = "hours")
          ),
          duration_days = duration_hours / 24
        )
    },

    #' @description
    #' Get ECMO settings summary
    #'
    #' @return tibble with ECMO parameter statistics.
    get_ecmo_settings_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      ecmo_data <- self$df %>%
        dplyr::filter(.data$mcs_group == "ECMO")

      if (nrow(ecmo_data) == 0) {
        cli::cli_alert_warning("No ECMO data available")
        return(invisible(NULL))
      }

      ecmo_data %>%
        dplyr::group_by(device_category) %>%
        dplyr::summarize(
          n = dplyr::n(),
          mean_flow = mean(flow, na.rm = TRUE),
          median_flow = median(flow, na.rm = TRUE),
          mean_sweep = mean(sweep, na.rm = TRUE),
          median_sweep = median(sweep, na.rm = TRUE),
          mean_fdo2 = mean(fdO2, na.rm = TRUE),
          median_fdo2 = median(fdO2, na.rm = TRUE),
          .groups = "drop"
        )
    },

    #' @description
    #' Identify patients on multiple devices
    #'
    #' @return tibble with patients on multiple MCS devices.
    get_multi_device_patients = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          n_device_types = dplyr::n_distinct(device_category),
          devices = paste(unique(device_category), collapse = ", "),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_device_types > 1) %>%
        dplyr::arrange(dplyr::desc(n_device_types))
    },

    #' @description
    #' Summarize ECMO/MCS data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("ECMO/MCS Summary")

      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)
      n_ecmo <- sum(self$df$mcs_group == "ECMO", na.rm = TRUE)

      cli::cli_text("Total observations: {.val {n_total}}")
      cli::cli_text("Hospitalizations with ECMO/MCS: {.val {n_hosp}}")
      cli::cli_text("ECMO observations: {.val {n_ecmo}} ({round(n_ecmo/n_total*100, 1)}%)")

      cli::cli_h3("Device Distribution")
      device_summary <- self$get_device_summary()
      print(device_summary)

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        n_ecmo = n_ecmo,
        device_summary = device_summary
      ))
    }
  )
)
