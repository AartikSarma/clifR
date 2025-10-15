#' Respiratory Support Table Class
#'
#' @description
#' R6 class for CLIF respiratory_support table containing ventilator settings,
#' modes, and respiratory support devices.
#'
#' @export
#' @importFrom R6 R6Class
RespiratorySupport <- R6::R6Class(
  classname = "RespiratorySupport",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize RespiratorySupport table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new RespiratorySupport instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "respiratory_support",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter by device category
    #'
    #' @param device_category Character device category (e.g., "IMV", "NIPPV").
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
    #' Get invasive mechanical ventilation (IMV) records
    #'
    #' @return tibble with IMV records.
    get_imv_records = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(device_category == "IMV")
    },

    #' @description
    #' Calculate PaO2/FiO2 ratio from vitals
    #'
    #' @param vitals_data tibble vitals data with PaO2 values.
    #' @param time_tolerance_hours Numeric time tolerance for matching (default: 1).
    #'
    #' @return tibble with P/F ratios.
    calculate_pf_ratio = function(vitals_data, time_tolerance_hours = 1) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      if (missing(vitals_data)) {
        cli::cli_abort("vitals_data required")
      }

      # Get PaO2 values from vitals (assuming it's a vital_category)
      pao2_data <- vitals_data %>%
        dplyr::filter(vital_category == "pao2") %>%
        dplyr::select(
          hospitalization_id,
          pao2_time = recorded_dttm,
          pao2 = vital_value
        )

      # Join respiratory support with PaO2 based on time proximity
      result <- self$df %>%
        dplyr::select(
          hospitalization_id,
          recorded_dttm,
          device_category,
          mode_category,
          fio2_set,
          peep_set
        ) %>%
        dplyr::inner_join(
          pao2_data,
          by = "hospitalization_id"
        ) %>%
        dplyr::mutate(
          time_diff_hours = abs(as.numeric(
            difftime(recorded_dttm, pao2_time, units = "hours")
          ))
        ) %>%
        dplyr::filter(time_diff_hours <= time_tolerance_hours) %>%
        dplyr::group_by(hospitalization_id, recorded_dttm) %>%
        dplyr::slice_min(time_diff_hours, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          fio2_decimal = fio2_set / 100,
          pf_ratio = pao2 / fio2_decimal
        )

      return(result)
    },

    #' @description
    #' Get ventilator mode summary
    #'
    #' @param device_category Character filter by device (default: NULL for all).
    #'
    #' @return tibble with mode statistics.
    get_mode_summary = function(device_category = NULL) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      data <- if (!is.null(device_category)) {
        self$df %>% dplyr::filter(.data$device_category == !!device_category)
      } else {
        self$df
      }

      data %>%
        dplyr::group_by(device_category, mode_category) %>%
        dplyr::summarize(
          n_observations = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(device_category, dplyr::desc(n_observations))
    },

    #' @description
    #' Get ventilator settings summary for IMV
    #'
    #' @return tibble with ventilator setting statistics.
    get_imv_settings_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      imv_data <- self$df %>%
        dplyr::filter(device_category == "IMV")

      list(
        fio2 = list(
          mean = mean(imv_data$fio2_set, na.rm = TRUE),
          median = median(imv_data$fio2_set, na.rm = TRUE),
          sd = sd(imv_data$fio2_set, na.rm = TRUE)
        ),
        peep = list(
          mean = mean(imv_data$peep_set, na.rm = TRUE),
          median = median(imv_data$peep_set, na.rm = TRUE),
          sd = sd(imv_data$peep_set, na.rm = TRUE)
        ),
        tidal_volume = list(
          mean = mean(imv_data$tidal_volume_set, na.rm = TRUE),
          median = median(imv_data$tidal_volume_set, na.rm = TRUE),
          sd = sd(imv_data$tidal_volume_set, na.rm = TRUE)
        ),
        resp_rate = list(
          mean = mean(imv_data$resp_rate_set, na.rm = TRUE),
          median = median(imv_data$resp_rate_set, na.rm = TRUE),
          sd = sd(imv_data$resp_rate_set, na.rm = TRUE)
        )
      )
    },

    #' @description
    #' Calculate mechanical ventilation duration
    #'
    #' @param device_category Character device type (default: "IMV").
    #'
    #' @return tibble with ventilation duration per hospitalization.
    calculate_ventilation_duration = function(device_category = "IMV") {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$device_category == !!device_category) %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          first_vent = min(recorded_dttm, na.rm = TRUE),
          last_vent = max(recorded_dttm, na.rm = TRUE),
          n_observations = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          vent_duration_hours = as.numeric(
            difftime(last_vent, first_vent, units = "hours")
          ),
          vent_duration_days = vent_duration_hours / 24
        )
    },

    #' @description
    #' Identify tracheostomy patients
    #'
    #' @return tibble with tracheostomy statistics.
    get_tracheostomy_patients = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(tracheostomy == 1) %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          first_trach_observation = min(recorded_dttm, na.rm = TRUE),
          n_trach_observations = dplyr::n(),
          .groups = "drop"
        )
    },

    #' @description
    #' Calculate lung-protective ventilation compliance
    #'
    #' @param tidal_volume_threshold Numeric mL/kg threshold (default: 6).
    #' @param plateau_pressure_threshold Numeric cmH2O threshold (default: 30).
    #'
    #' @return tibble with compliance statistics.
    calculate_lung_protective_compliance = function(tidal_volume_threshold = 6,
                                                   plateau_pressure_threshold = 30) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      # This is a simplified calculation - would need patient weight for accurate TV/kg
      self$df %>%
        dplyr::filter(device_category == "IMV") %>%
        dplyr::mutate(
          low_tidal_volume = tidal_volume_obs < (tidal_volume_threshold * 100),  # Assuming ~100kg
          low_plateau_pressure = plateau_pressure_obs < plateau_pressure_threshold,
          lung_protective = low_tidal_volume | is.na(tidal_volume_obs)
        ) %>%
        dplyr::group_by(hospitalization_id) %>%
        dplyr::summarize(
          n_observations = dplyr::n(),
          n_lung_protective = sum(lung_protective, na.rm = TRUE),
          pct_lung_protective = n_lung_protective / n_observations * 100,
          .groups = "drop"
        )
    },

    #' @description
    #' Summarize respiratory support data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Respiratory Support Summary")

      # Basic counts
      n_total <- nrow(self$df)
      n_hosp <- dplyr::n_distinct(self$df$hospitalization_id)
      n_devices <- dplyr::n_distinct(self$df$device_category)

      cli::cli_text("Total observations: {.val {n_total}}")
      cli::cli_text("Unique hospitalizations: {.val {n_hosp}}")
      cli::cli_text("Device types: {.val {n_devices}}")

      # Device category breakdown
      cli::cli_h3("Device Category Distribution")
      device_summary <- self$df %>%
        dplyr::group_by(device_category) %>%
        dplyr::summarize(
          n_observations = dplyr::n(),
          n_hospitalizations = dplyr::n_distinct(hospitalization_id),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_observations))
      print(device_summary)

      # IMV mode summary
      if ("IMV" %in% self$df$device_category) {
        cli::cli_h3("IMV Ventilator Modes")
        mode_summary <- self$get_mode_summary(device_category = "IMV")
        print(mode_summary)

        cli::cli_h3("IMV Ventilator Settings")
        settings <- self$get_imv_settings_summary()
        cli::cli_text("FiO2: {round(settings$fio2$mean, 1)}% (SD: {round(settings$fio2$sd, 1)})")
        cli::cli_text("PEEP: {round(settings$peep$mean, 1)} cmH2O (SD: {round(settings$peep$sd, 1)})")
        cli::cli_text("Tidal Volume: {round(settings$tidal_volume$mean, 0)} mL (SD: {round(settings$tidal_volume$sd, 0)})")
      }

      # Tracheostomy patients
      n_trach <- sum(self$df$tracheostomy == 1)
      n_trach_hosp <- dplyr::n_distinct(
        self$df$hospitalization_id[self$df$tracheostomy == 1]
      )
      cli::cli_text("Tracheostomy observations: {.val {n_trach}} ({n_trach_hosp} hospitalizations)")

      invisible(list(
        n_total = n_total,
        n_hospitalizations = n_hosp,
        device_summary = device_summary
      ))
    }
  )
)
