#' Unit Conversion for Medical Data
#'
#' @description
#' Convert between medical units, particularly for medication dosing.
#' Supports weight-based and time-based conversions.

#' Convert medication dose units
#'
#' @description
#' Convert medication doses between different units (e.g., mcg/kg/min to mg/hr).
#'
#' @param dose Numeric dose value.
#' @param from_unit Character source unit.
#' @param to_unit Character target unit.
#' @param weight_kg Numeric patient weight in kg (required for weight-based conversions).
#'
#' @return Numeric converted dose value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert norepinephrine from mcg/kg/min to mg/hr (for 70kg patient)
#' convert_medication_dose(0.1, "mcg/kg/min", "mg/hr", weight_kg = 70)
#'
#' # Convert propofol from mcg/kg/min to mg/hr
#' convert_medication_dose(50, "mcg/kg/min", "mg/hr", weight_kg = 80)
#' }
convert_medication_dose <- function(dose, from_unit, to_unit, weight_kg = NULL) {

  # Parse units into components
  from_parsed <- parse_dose_unit(from_unit)
  to_parsed <- parse_dose_unit(to_unit)

  # Convert to base units (mcg/min)
  base_dose <- dose

  # Convert mass component to mcg
  base_dose <- base_dose * get_mass_conversion_factor(from_parsed$mass, "mcg")

  # Apply weight if present
  if (from_parsed$per_kg && is.null(weight_kg)) {
    cli::cli_abort("weight_kg required for per-kg dose conversion")
  }
  if (from_parsed$per_kg) {
    base_dose <- base_dose * weight_kg
  }

  # Convert time component to /min
  base_dose <- base_dose / get_time_conversion_factor(from_parsed$time, "min")

  # Now convert from base units to target units
  result <- base_dose

  # Convert mass from mcg to target
  result <- result / get_mass_conversion_factor(to_parsed$mass, "mcg")

  # Remove weight if target is per-kg
  if (to_parsed$per_kg && is.null(weight_kg)) {
    cli::cli_abort("weight_kg required for per-kg dose conversion")
  }
  if (to_parsed$per_kg) {
    result <- result / weight_kg
  }

  # Convert time from /min to target
  result <- result * get_time_conversion_factor(to_parsed$time, "min")

  return(result)
}

#' Parse dose unit string
#'
#' @keywords internal
parse_dose_unit <- function(unit_string) {
  # Remove spaces
  unit <- gsub("\\s+", "", tolower(unit_string))

  # Check for per-kg
  per_kg <- grepl("/kg", unit)
  unit <- gsub("/kg", "", unit)

  # Split by /
  parts <- strsplit(unit, "/")[[1]]

  if (length(parts) == 0 || length(parts) > 2) {
    cli::cli_abort("Invalid unit format: {unit_string}")
  }

  mass_unit <- parts[1]
  time_unit <- if (length(parts) == 2) parts[2] else "dose"

  list(
    mass = mass_unit,
    time = time_unit,
    per_kg = per_kg
  )
}

#' Get mass conversion factor
#'
#' @keywords internal
get_mass_conversion_factor <- function(from_mass, to_mass) {
  # Conversion factors to mcg
  mass_to_mcg <- c(
    "mcg" = 1,
    "mg" = 1000,
    "g" = 1000000,
    "u" = 1,  # Units (special handling)
    "unit" = 1,
    "ml" = 1  # Volume (special handling)
  )

  if (!(from_mass %in% names(mass_to_mcg))) {
    cli::cli_warn("Unknown mass unit: {from_mass}, assuming 1:1 conversion")
    return(1)
  }

  if (!(to_mass %in% names(mass_to_mcg))) {
    cli::cli_warn("Unknown mass unit: {to_mass}, assuming 1:1 conversion")
    return(1)
  }

  # Convert from -> mcg -> to
  factor <- mass_to_mcg[[from_mass]] / mass_to_mcg[[to_mass]]
  return(factor)
}

#' Get time conversion factor
#'
#' @keywords internal
get_time_conversion_factor <- function(from_time, to_time) {
  # Conversion factors to minutes
  time_to_min <- c(
    "min" = 1,
    "hr" = 60,
    "day" = 1440,
    "dose" = 0  # Single dose (special handling)
  )

  if (!(from_time %in% names(time_to_min))) {
    cli::cli_warn("Unknown time unit: {from_time}, assuming /min")
    from_time <- "min"
  }

  if (!(to_time %in% names(time_to_min))) {
    cli::cli_warn("Unknown time unit: {to_time}, assuming /min")
    to_time <- "min"
  }

  # Special handling for single doses
  if (from_time == "dose" || to_time == "dose") {
    return(1)
  }

  factor <- time_to_min[[from_time]] / time_to_min[[to_time]]
  return(factor)
}

#' Convert medication doses in a dataframe
#'
#' @description
#' Batch convert medication doses in a medication administration table.
#'
#' @param med_data tibble with medication administration data.
#' @param dose_col Character name of dose column.
#' @param unit_col Character name of unit column.
#' @param target_units Named list of target units by medication.
#' @param weight_col Character name of weight column (optional).
#' @param medication_col Character name of medication column.
#'
#' @return tibble with converted doses and new columns.
#'
#' @export
convert_medication_doses <- function(med_data,
                                      dose_col = "dose",
                                      unit_col = "dose_unit",
                                      target_units,
                                      weight_col = "weight_kg",
                                      medication_col = "medication_name") {

  if (!(dose_col %in% names(med_data))) {
    cli::cli_abort("Dose column {dose_col} not found")
  }

  if (!(unit_col %in% names(med_data))) {
    cli::cli_abort("Unit column {unit_col} not found")
  }

  if (!(medication_col %in% names(med_data))) {
    cli::cli_abort("Medication column {medication_col} not found")
  }

  # Add converted dose columns
  result <- med_data %>%
    dplyr::mutate(
      dose_original = .data[[dose_col]],
      unit_original = .data[[unit_col]],
      dose_converted = NA_real_,
      unit_converted = NA_character_,
      conversion_status = "not_converted"
    )

  # Convert each medication type
  for (med_name in names(target_units)) {
    target_unit <- target_units[[med_name]]

    # Find rows for this medication
    med_rows <- which(tolower(result[[medication_col]]) == tolower(med_name))

    if (length(med_rows) == 0) {
      next
    }

    cli::cli_alert_info(
      "Converting {length(med_rows)} doses for {.field {med_name}} to {.val {target_unit}}"
    )

    # Convert each row
    for (i in med_rows) {
      from_unit <- result[[unit_col]][i]
      dose_value <- result[[dose_col]][i]

      if (is.na(dose_value) || is.na(from_unit)) {
        next
      }

      # Get weight if needed
      weight <- if (weight_col %in% names(result)) {
        result[[weight_col]][i]
      } else {
        NULL
      }

      # Convert
      tryCatch({
        converted <- convert_medication_dose(
          dose_value,
          from_unit,
          target_unit,
          weight_kg = weight
        )

        result$dose_converted[i] <- converted
        result$unit_converted[i] <- target_unit
        result$conversion_status[i] <- "converted"
      }, error = function(e) {
        result$conversion_status[i] <<- paste0("error: ", e$message)
      })
    }
  }

  # Summary
  n_converted <- sum(result$conversion_status == "converted")
  n_total <- nrow(result)

  cli::cli_alert_success(
    "Converted {n_converted}/{n_total} doses ({round(n_converted/n_total*100, 1)}%)"
  )

  return(result)
}

#' Temperature conversion
#'
#' @description
#' Convert temperature between Celsius and Fahrenheit.
#'
#' @param temp Numeric temperature value.
#' @param from Character source unit ("C" or "F").
#' @param to Character target unit ("C" or "F").
#'
#' @return Numeric converted temperature.
#'
#' @export
#'
#' @examples
#' convert_temperature(37, "C", "F")  # 98.6
#' convert_temperature(98.6, "F", "C")  # 37
convert_temperature <- function(temp, from = "C", to = "F") {
  from <- toupper(substr(from, 1, 1))
  to <- toupper(substr(to, 1, 1))

  if (from == to) {
    return(temp)
  }

  if (from == "C" && to == "F") {
    return(temp * 9/5 + 32)
  } else if (from == "F" && to == "C") {
    return((temp - 32) * 5/9)
  } else {
    cli::cli_abort("Invalid temperature units: {from} to {to}")
  }
}

#' Pressure conversion
#'
#' @description
#' Convert pressure between mmHg and cmH2O.
#'
#' @param pressure Numeric pressure value.
#' @param from Character source unit ("mmHg" or "cmH2O").
#' @param to Character target unit ("mmHg" or "cmH2O").
#'
#' @return Numeric converted pressure.
#'
#' @export
convert_pressure <- function(pressure, from = "mmHg", to = "cmH2O") {
  from <- tolower(from)
  to <- tolower(to)

  if (from == to) {
    return(pressure)
  }

  # Conversion factor: 1 mmHg = 1.35951 cmH2O
  if (from == "mmhg" && to == "cmh2o") {
    return(pressure * 1.35951)
  } else if (from == "cmh2o" && to == "mmhg") {
    return(pressure / 1.35951)
  } else {
    cli::cli_abort("Invalid pressure units: {from} to {to}")
  }
}

#' Lab value conversion
#'
#' @description
#' Convert common lab values between units (e.g., mg/dL to mmol/L).
#'
#' @param value Numeric lab value.
#' @param lab_type Character type of lab (e.g., "glucose", "creatinine").
#' @param from Character source unit.
#' @param to Character target unit.
#'
#' @return Numeric converted value.
#'
#' @export
convert_lab_value <- function(value, lab_type, from, to) {
  # Define conversion factors for common labs
  # These are approximate molecular weight-based conversions

  conversions <- list(
    glucose = list(
      "mg/dl_to_mmol/l" = 0.0555,  # MW 180.16
      "mmol/l_to_mg/dl" = 18.0
    ),
    creatinine = list(
      "mg/dl_to_umol/l" = 88.4,    # MW 113.12
      "umol/l_to_mg/dl" = 0.0113
    ),
    bun = list(
      "mg/dl_to_mmol/l" = 0.357,   # MW 28.01 (as urea nitrogen)
      "mmol/l_to_mg/dl" = 2.8
    ),
    calcium = list(
      "mg/dl_to_mmol/l" = 0.25,    # MW 40.08
      "mmol/l_to_mg/dl" = 4.0
    ),
    bilirubin = list(
      "mg/dl_to_umol/l" = 17.1,    # MW 584.66
      "umol/l_to_mg/dl" = 0.0585
    )
  )

  lab_type <- tolower(lab_type)

  if (!(lab_type %in% names(conversions))) {
    cli::cli_warn("No conversion available for {lab_type}")
    return(value)
  }

  # Construct conversion key
  from_clean <- tolower(gsub("[^a-z/]", "", from))
  to_clean <- tolower(gsub("[^a-z/]", "", to))
  conversion_key <- paste0(from_clean, "_to_", to_clean)

  if (conversion_key %in% names(conversions[[lab_type]])) {
    factor <- conversions[[lab_type]][[conversion_key]]
    return(value * factor)
  } else {
    cli::cli_warn("Conversion {from} to {to} not available for {lab_type}")
    return(value)
  }
}
