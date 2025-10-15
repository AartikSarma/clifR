#' SOFA Score Calculation
#'
#' @description
#' Calculate Sequential Organ Failure Assessment (SOFA) scores for ICU patients.
#' SOFA assesses organ dysfunction across 6 systems: respiratory, coagulation,
#' hepatic, cardiovascular, renal, and neurological.

#' Calculate SOFA score from clinical parameters
#'
#' @description
#' Calculate total SOFA score and component scores from clinical measurements.
#'
#' @param pao2_fio2 Numeric. PaO2/FiO2 ratio in mmHg (respiratory).
#' @param platelets Numeric. Platelet count in 10^3/μL (coagulation).
#' @param bilirubin Numeric. Total bilirubin in mg/dL (hepatic).
#' @param map Numeric. Mean arterial pressure in mmHg (cardiovascular).
#' @param vasopressor Logical or character. Vasopressor use (cardiovascular).
#' @param dopamine_dose Numeric. Dopamine dose in mcg/kg/min.
#' @param dobutamine_dose Numeric. Dobutamine dose in mcg/kg/min.
#' @param epinephrine_dose Numeric. Epinephrine dose in mcg/kg/min.
#' @param norepinephrine_dose Numeric. Norepinephrine dose in mcg/kg/min.
#' @param creatinine Numeric. Serum creatinine in mg/dL (renal).
#' @param urine_output Numeric. Urine output in mL/day (renal).
#' @param gcs Numeric. Glasgow Coma Scale score (neurological).
#' @param mechanical_vent Logical. Whether patient is mechanically ventilated.
#'
#' @return List with total SOFA score and component scores.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_sofa(
#'   pao2_fio2 = 250,
#'   platelets = 120,
#'   bilirubin = 1.5,
#'   map = 65,
#'   creatinine = 1.8,
#'   gcs = 13
#' )
#' }
calculate_sofa <- function(pao2_fio2 = NA,
                          platelets = NA,
                          bilirubin = NA,
                          map = NA,
                          vasopressor = FALSE,
                          dopamine_dose = 0,
                          dobutamine_dose = 0,
                          epinephrine_dose = 0,
                          norepinephrine_dose = 0,
                          creatinine = NA,
                          urine_output = NA,
                          gcs = NA,
                          mechanical_vent = FALSE) {

  # Calculate component scores
  resp_score <- calculate_sofa_respiratory(pao2_fio2, mechanical_vent)
  coag_score <- calculate_sofa_coagulation(platelets)
  hepatic_score <- calculate_sofa_hepatic(bilirubin)
  cardio_score <- calculate_sofa_cardiovascular(
    map, dopamine_dose, dobutamine_dose,
    epinephrine_dose, norepinephrine_dose
  )
  renal_score <- calculate_sofa_renal(creatinine, urine_output)
  neuro_score <- calculate_sofa_neurological(gcs)

  # Calculate total (exclude NA)
  scores <- c(resp_score, coag_score, hepatic_score,
              cardio_score, renal_score, neuro_score)
  total_score <- sum(scores, na.rm = TRUE)
  n_components <- sum(!is.na(scores))

  list(
    total_sofa = total_score,
    n_components = n_components,
    respiratory = resp_score,
    coagulation = coag_score,
    hepatic = hepatic_score,
    cardiovascular = cardio_score,
    renal = renal_score,
    neurological = neuro_score
  )
}

#' Calculate respiratory SOFA component
#'
#' @keywords internal
calculate_sofa_respiratory <- function(pao2_fio2, mechanical_vent = FALSE) {
  if (is.na(pao2_fio2)) {
    return(NA_integer_)
  }

  # Score based on PaO2/FiO2 ratio
  if (pao2_fio2 >= 400) {
    score <- 0
  } else if (pao2_fio2 >= 300) {
    score <- 1
  } else if (pao2_fio2 >= 200) {
    score <- 2
  } else if (pao2_fio2 >= 100) {
    score <- 3
  } else {
    score <- 4
  }

  # Scores 3-4 require mechanical ventilation
  if (score >= 3 && !mechanical_vent) {
    score <- 2
  }

  return(score)
}

#' Calculate coagulation SOFA component
#'
#' @keywords internal
calculate_sofa_coagulation <- function(platelets) {
  if (is.na(platelets)) {
    return(NA_integer_)
  }

  if (platelets >= 150) {
    return(0L)
  } else if (platelets >= 100) {
    return(1L)
  } else if (platelets >= 50) {
    return(2L)
  } else if (platelets >= 20) {
    return(3L)
  } else {
    return(4L)
  }
}

#' Calculate hepatic SOFA component
#'
#' @keywords internal
calculate_sofa_hepatic <- function(bilirubin) {
  if (is.na(bilirubin)) {
    return(NA_integer_)
  }

  if (bilirubin < 1.2) {
    return(0L)
  } else if (bilirubin < 2.0) {
    return(1L)
  } else if (bilirubin < 6.0) {
    return(2L)
  } else if (bilirubin < 12.0) {
    return(3L)
  } else {
    return(4L)
  }
}

#' Calculate cardiovascular SOFA component
#'
#' @keywords internal
calculate_sofa_cardiovascular <- function(map,
                                         dopamine_dose = 0,
                                         dobutamine_dose = 0,
                                         epinephrine_dose = 0,
                                         norepinephrine_dose = 0) {

  # Check vasopressor doses
  high_dose_vasopressor <- (dopamine_dose > 15 ||
                           epinephrine_dose > 0.1 ||
                           norepinephrine_dose > 0.1)

  moderate_dose_vasopressor <- (dopamine_dose > 5 ||
                               (epinephrine_dose > 0 && epinephrine_dose <= 0.1) ||
                               (norepinephrine_dose > 0 && norepinephrine_dose <= 0.1))

  low_dose_vasopressor <- (dopamine_dose > 0 && dopamine_dose <= 5) ||
                          dobutamine_dose > 0

  # Score based on MAP and vasopressor use
  if (high_dose_vasopressor) {
    return(4L)
  } else if (moderate_dose_vasopressor) {
    return(3L)
  } else if (low_dose_vasopressor) {
    return(2L)
  } else if (!is.na(map)) {
    if (map >= 70) {
      return(0L)
    } else {
      return(1L)
    }
  } else {
    return(NA_integer_)
  }
}

#' Calculate renal SOFA component
#'
#' @keywords internal
calculate_sofa_renal <- function(creatinine, urine_output = NA) {
  # Score based on creatinine
  creat_score <- if (!is.na(creatinine)) {
    if (creatinine < 1.2) {
      0L
    } else if (creatinine < 2.0) {
      1L
    } else if (creatinine < 3.5) {
      2L
    } else if (creatinine < 5.0) {
      3L
    } else {
      4L
    }
  } else {
    NA_integer_
  }

  # Score based on urine output
  uo_score <- if (!is.na(urine_output)) {
    if (urine_output >= 500) {
      0L
    } else if (urine_output >= 200) {
      3L
    } else {
      4L
    }
  } else {
    NA_integer_
  }

  # Return worst score
  if (is.na(creat_score) && is.na(uo_score)) {
    return(NA_integer_)
  } else if (is.na(creat_score)) {
    return(uo_score)
  } else if (is.na(uo_score)) {
    return(creat_score)
  } else {
    return(max(creat_score, uo_score))
  }
}

#' Calculate neurological SOFA component
#'
#' @keywords internal
calculate_sofa_neurological <- function(gcs) {
  if (is.na(gcs)) {
    return(NA_integer_)
  }

  if (gcs == 15) {
    return(0L)
  } else if (gcs >= 13) {
    return(1L)
  } else if (gcs >= 10) {
    return(2L)
  } else if (gcs >= 6) {
    return(3L)
  } else {
    return(4L)
  }
}

#' Calculate SOFA scores for a dataframe
#'
#' @description
#' Batch calculate SOFA scores for multiple observations (e.g., time series).
#'
#' @param data tibble with clinical measurements.
#' @param pao2_fio2_col Character name of PaO2/FiO2 column.
#' @param platelets_col Character name of platelets column.
#' @param bilirubin_col Character name of bilirubin column.
#' @param map_col Character name of MAP column.
#' @param creatinine_col Character name of creatinine column.
#' @param urine_output_col Character name of urine output column.
#' @param gcs_col Character name of GCS column.
#' @param mechanical_vent_col Character name of mechanical ventilation column.
#' @param dopamine_col Character name of dopamine dose column.
#' @param dobutamine_col Character name of dobutamine dose column.
#' @param epinephrine_col Character name of epinephrine dose column.
#' @param norepinephrine_col Character name of norepinephrine dose column.
#' @param include_components Logical. Include component scores (default: TRUE).
#'
#' @return tibble with SOFA scores added.
#'
#' @export
calculate_sofa_scores <- function(data,
                                  pao2_fio2_col = "pao2_fio2",
                                  platelets_col = "platelets",
                                  bilirubin_col = "bilirubin",
                                  map_col = "map",
                                  creatinine_col = "creatinine",
                                  urine_output_col = "urine_output",
                                  gcs_col = "gcs",
                                  mechanical_vent_col = "mechanical_vent",
                                  dopamine_col = "dopamine_dose",
                                  dobutamine_col = "dobutamine_dose",
                                  epinephrine_col = "epinephrine_dose",
                                  norepinephrine_col = "norepinephrine_dose",
                                  include_components = TRUE) {

  cli::cli_alert_info("Calculating SOFA scores for {nrow(data)} observations")

  # Extract columns (use NA if column doesn't exist)
  get_col <- function(col_name) {
    if (col_name %in% names(data)) {
      data[[col_name]]
    } else {
      rep(NA, nrow(data))
    }
  }

  pao2_fio2_vals <- get_col(pao2_fio2_col)
  platelets_vals <- get_col(platelets_col)
  bilirubin_vals <- get_col(bilirubin_col)
  map_vals <- get_col(map_col)
  creatinine_vals <- get_col(creatinine_col)
  urine_output_vals <- get_col(urine_output_col)
  gcs_vals <- get_col(gcs_col)
  mechanical_vent_vals <- get_col(mechanical_vent_col)
  dopamine_vals <- get_col(dopamine_col)
  dobutamine_vals <- get_col(dobutamine_col)
  epinephrine_vals <- get_col(epinephrine_col)
  norepinephrine_vals <- get_col(norepinephrine_col)

  # Replace NA with defaults for boolean/numeric dose columns
  mechanical_vent_vals[is.na(mechanical_vent_vals)] <- FALSE
  dopamine_vals[is.na(dopamine_vals)] <- 0
  dobutamine_vals[is.na(dobutamine_vals)] <- 0
  epinephrine_vals[is.na(epinephrine_vals)] <- 0
  norepinephrine_vals[is.na(norepinephrine_vals)] <- 0

  # Calculate scores for each row
  sofa_results <- purrr::pmap(
    list(
      pao2_fio2 = pao2_fio2_vals,
      platelets = platelets_vals,
      bilirubin = bilirubin_vals,
      map = map_vals,
      creatinine = creatinine_vals,
      urine_output = urine_output_vals,
      gcs = gcs_vals,
      mechanical_vent = mechanical_vent_vals,
      dopamine_dose = dopamine_vals,
      dobutamine_dose = dobutamine_vals,
      epinephrine_dose = epinephrine_vals,
      norepinephrine_dose = norepinephrine_vals
    ),
    calculate_sofa
  )

  # Extract results into columns
  result <- data %>%
    dplyr::mutate(
      sofa_total = purrr::map_int(sofa_results, ~.x$total_sofa),
      sofa_n_components = purrr::map_int(sofa_results, ~.x$n_components)
    )

  if (include_components) {
    result <- result %>%
      dplyr::mutate(
        sofa_respiratory = purrr::map_int(sofa_results, ~.x$respiratory %||% NA_integer_),
        sofa_coagulation = purrr::map_int(sofa_results, ~.x$coagulation %||% NA_integer_),
        sofa_hepatic = purrr::map_int(sofa_results, ~.x$hepatic %||% NA_integer_),
        sofa_cardiovascular = purrr::map_int(sofa_results, ~.x$cardiovascular %||% NA_integer_),
        sofa_renal = purrr::map_int(sofa_results, ~.x$renal %||% NA_integer_),
        sofa_neurological = purrr::map_int(sofa_results, ~.x$neurological %||% NA_integer_)
      )
  }

  cli::cli_alert_success("SOFA calculation complete")

  return(result)
}

#' Calculate SOFA scores over time
#'
#' @description
#' Calculate SOFA scores for time-series data (e.g., hourly or daily).
#' Requires wide format data with time points.
#'
#' @param wide_data tibble in wide format with measurements.
#' @param id_col Character. ID column name (default: "hospitalization_id").
#' @param time_col Character. Time column name (default: "time_rounded").
#' @param ... Additional arguments passed to calculate_sofa_scores().
#'
#' @return tibble with SOFA scores over time.
#'
#' @export
calculate_sofa_time_series <- function(wide_data,
                                       id_col = "hospitalization_id",
                                       time_col = "time_rounded",
                                       ...) {

  if (!(id_col %in% names(wide_data))) {
    cli::cli_abort("ID column {id_col} not found")
  }

  if (!(time_col %in% names(wide_data))) {
    cli::cli_abort("Time column {time_col} not found")
  }

  cli::cli_h2("Calculating Time-Series SOFA Scores")
  cli::cli_alert_info("Data: {nrow(wide_data)} time points")

  # Calculate SOFA scores
  result <- calculate_sofa_scores(wide_data, ...)

  # Summarize by patient
  sofa_summary <- result %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarize(
      n_timepoints = dplyr::n(),
      mean_sofa = mean(sofa_total, na.rm = TRUE),
      max_sofa = max(sofa_total, na.rm = TRUE),
      min_sofa = min(sofa_total, na.rm = TRUE),
      admission_sofa = dplyr::first(sofa_total),
      discharge_sofa = dplyr::last(sofa_total),
      delta_sofa = discharge_sofa - admission_sofa,
      .groups = "drop"
    )

  cli::cli_alert_success("Time-series SOFA calculation complete")
  cli::cli_text("Summary statistics:")
  cli::cli_text("  Mean SOFA: {round(mean(sofa_summary$mean_sofa, na.rm = TRUE), 1)}")
  cli::cli_text("  Max SOFA: {round(max(sofa_summary$max_sofa, na.rm = TRUE), 1)}")

  attr(result, "summary") <- sofa_summary

  return(result)
}

#' Calculate delta SOFA (change from baseline)
#'
#' @description
#' Calculate change in SOFA score from admission or baseline measurement.
#'
#' @param sofa_data tibble with SOFA scores over time.
#' @param id_col Character. ID column name.
#' @param time_col Character. Time column name.
#' @param sofa_col Character. SOFA score column name (default: "sofa_total").
#' @param baseline Character. Baseline method: "first", "min", "mean" (default: "first").
#'
#' @return tibble with delta SOFA scores.
#'
#' @export
calculate_delta_sofa <- function(sofa_data,
                                 id_col = "hospitalization_id",
                                 time_col = "time_rounded",
                                 sofa_col = "sofa_total",
                                 baseline = "first") {

  cli::cli_alert_info("Calculating delta SOFA (baseline: {baseline})")

  # Calculate baseline for each patient
  baseline_sofa <- sofa_data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarize(
      baseline_sofa = switch(baseline,
        first = dplyr::first(.data[[sofa_col]]),
        min = min(.data[[sofa_col]], na.rm = TRUE),
        mean = mean(.data[[sofa_col]], na.rm = TRUE),
        {
          cli::cli_warn("Unknown baseline method {baseline}, using first")
          dplyr::first(.data[[sofa_col]])
        }
      ),
      .groups = "drop"
    )

  # Join baseline and calculate delta
  result <- sofa_data %>%
    dplyr::left_join(baseline_sofa, by = id_col) %>%
    dplyr::mutate(
      delta_sofa = .data[[sofa_col]] - baseline_sofa
    )

  cli::cli_alert_success("Delta SOFA calculation complete")

  return(result)
}
