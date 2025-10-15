#' Generate Synthetic CLIF Data for Testing
#'
#' @description
#' Generate realistic synthetic CLIF-compliant data for cross-language validation.
#' This data is generated ONCE and used by both R (clifR) and Python (clifpy)
#' to ensure identical inputs for comparison testing.
#'
#' @details
#' Generates:
#' - Patient demographics
#' - Hospitalizations
#' - ADT events
#' - Vitals (time series)
#' - Labs (time series)
#' - Respiratory support
#' - Medications (continuous)
#'
#' All data saved to tests/fixtures/synthetic_data/

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

set.seed(42)  # For reproducibility

# Configuration
n_patients <- 1000
n_hospitalizations_per_patient <- 1.5  # Average
tz <- "America/New_York"
output_dir <- "tests/fixtures/synthetic_data"

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cat("Generating synthetic CLIF data...\n")
cat("Random seed: 42\n")
cat("Timezone:", tz, "\n")
cat("Number of patients:", n_patients, "\n\n")

# ============================================================================
# 1. PATIENT TABLE
# ============================================================================

cat("Generating patient data...\n")

patient <- tibble(
  patient_id = sprintf("PAT%04d", 1:n_patients),
  birth_date = as.POSIXct(
    seq(as.Date("1940-01-01"), as.Date("1990-12-31"), length.out = n_patients),
    tz = tz
  ),
  death_dttm = as.POSIXct(NA, tz = tz),  # Most survive
  race_name = sample(c("White", "Black", "Asian", "Other"), n_patients, replace = TRUE),
  race_category = race_name,
  ethnicity_name = sample(c("Hispanic", "Non-Hispanic"), n_patients, replace = TRUE),
  ethnicity_category = ethnicity_name,
  sex_name = sample(c("Male", "Female"), n_patients, replace = TRUE),
  sex_category = sex_name,
  language_name = sample(c("English", "Spanish"), n_patients, replace = TRUE, prob = c(0.8, 0.2)),
  language_category = language_name
)

# Add some deaths (20% mortality)
death_idx <- sample(1:n_patients, size = round(n_patients * 0.2))
patient$death_dttm[death_idx] <- as.POSIXct(
  "2024-06-15",
  tz = tz
) + days(sample(0:30, length(death_idx), replace = TRUE))

write_csv(patient, file.path(output_dir, "patient.csv"))
cat("  Generated", nrow(patient), "patients\n")

# ============================================================================
# 2. HOSPITALIZATION TABLE
# ============================================================================

cat("Generating hospitalization data...\n")

# Generate 1-3 hospitalizations per patient
n_hosps <- round(n_patients * n_hospitalizations_per_patient)

hospitalization <- tibble(
  patient_id = sample(patient$patient_id, n_hosps, replace = TRUE),
  hospitalization_id = sprintf("HOSP%04d", 1:n_hosps),
  hospitalization_joined_id = hospitalization_id,  # Not stitched yet
  admission_dttm = as.POSIXct(
    "2024-01-01",
    tz = tz
  ) + days(sample(0:180, n_hosps, replace = TRUE)) +
    hours(sample(0:23, n_hosps, replace = TRUE)),
  discharge_dttm = admission_dttm + days(sample(2:30, n_hosps, replace = TRUE)) +
    hours(sample(0:23, n_hosps, replace = TRUE)),
  age_at_admission = NA_integer_,
  admission_type_name = sample(
    c("Emergency Department", "Direct Admission", "Transfer"),
    n_hosps,
    replace = TRUE,
    prob = c(0.6, 0.3, 0.1)
  ),
  admission_type_category = case_when(
    admission_type_name == "Emergency Department" ~ "ed",
    admission_type_name == "Direct Admission" ~ "direct",
    admission_type_name == "Transfer" ~ "osh"
  ),
  discharge_name = sample(
    c("Home", "SNF", "Expired", "Rehab", "Other Hospital"),
    n_hosps,
    replace = TRUE,
    prob = c(0.6, 0.15, 0.1, 0.1, 0.05)
  ),
  discharge_category = case_when(
    discharge_name == "Home" ~ "Home",
    discharge_name == "SNF" ~ "Skilled Nursing Facility (SNF)",
    discharge_name == "Expired" ~ "Expired",
    discharge_name == "Rehab" ~ "Acute Inpatient Rehab Facility",
    discharge_name == "Other Hospital" ~ "Acute Care Hospital"
  )
)

# Calculate age at admission
hospitalization <- hospitalization %>%
  left_join(patient %>% select(patient_id, birth_date), by = "patient_id") %>%
  mutate(
    age_at_admission = as.integer(
      floor(as.numeric(difftime(admission_dttm, birth_date, units = "days")) / 365.25)
    )
  ) %>%
  select(-birth_date)

write_csv(hospitalization, file.path(output_dir, "hospitalization.csv"))
cat("  Generated", nrow(hospitalization), "hospitalizations\n")

# ============================================================================
# 3. ADT (Admission/Discharge/Transfer) TABLE
# ============================================================================

cat("Generating ADT data...\n")

# Generate ADT events for each hospitalization
adt_list <- list()

for (i in 1:nrow(hospitalization)) {
  hosp <- hospitalization[i, ]

  # Admission event
  adt_admission <- tibble(
    hospitalization_id = hosp$hospitalization_id,
    in_dttm = hosp$admission_dttm,
    out_dttm = hosp$admission_dttm + hours(sample(2:48, 1)),
    location_name = "Emergency Department",
    location_category = "ed"
  )

  # ICU event (50% of patients go to ICU)
  if (runif(1) < 0.5) {
    adt_icu <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      in_dttm = adt_admission$out_dttm,
      out_dttm = adt_admission$out_dttm + days(sample(2:10, 1)),
      location_name = "Medical ICU",
      location_category = "icu"
    )

    # Floor after ICU
    adt_floor <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      in_dttm = adt_icu$out_dttm,
      out_dttm = hosp$discharge_dttm,
      location_name = "General Medicine Floor",
      location_category = "floor"
    )

    adt_list[[length(adt_list) + 1]] <- bind_rows(adt_admission, adt_icu, adt_floor)
  } else {
    # Direct to floor
    adt_floor <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      in_dttm = adt_admission$out_dttm,
      out_dttm = hosp$discharge_dttm,
      location_name = "General Medicine Floor",
      location_category = "floor"
    )

    adt_list[[length(adt_list) + 1]] <- bind_rows(adt_admission, adt_floor)
  }
}

adt <- bind_rows(adt_list)

write_csv(adt, file.path(output_dir, "adt.csv"))
cat("  Generated", nrow(adt), "ADT events\n")

# ============================================================================
# 4. VITALS TABLE
# ============================================================================

cat("Generating vitals data...\n")

# Generate hourly vitals for ICU patients, every 4 hours for floor
vitals_list <- list()

for (i in 1:nrow(hospitalization)) {
  hosp <- hospitalization[i, ]
  hosp_adt <- adt %>% filter(hospitalization_id == hosp$hospitalization_id)

  for (j in 1:nrow(hosp_adt)) {
    location <- hosp_adt[j, ]

    # Determine measurement frequency
    interval_hours <- if_else(location$location_category == "icu", 1, 4)

    # Skip if time period is too short or invalid
    time_diff_hours <- as.numeric(difftime(location$out_dttm, location$in_dttm, units = "hours"))
    if (is.na(time_diff_hours) || time_diff_hours < interval_hours) {
      next
    }

    # Generate time points
    time_seq <- seq(
      from = location$in_dttm,
      to = location$out_dttm,
      by = paste(interval_hours, "hours")
    )

    n_measurements <- length(time_seq)

    # Skip if no measurements
    if (n_measurements == 0) {
      next
    }

    # Generate vital signs
    vitals_temp <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      recorded_dttm = time_seq,
      vital_name = "Temperature",
      vital_category = "temp_c",
      vital_value = rnorm(n_measurements, mean = 37.0, sd = 0.5),
      meas_site_name = "Oral"
    )

    vitals_hr <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      recorded_dttm = time_seq,
      vital_name = "Heart Rate",
      vital_category = "heart_rate",
      vital_value = rnorm(n_measurements, mean = 85, sd = 15),
      meas_site_name = NA_character_
    )

    vitals_sbp <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      recorded_dttm = time_seq,
      vital_name = "Systolic Blood Pressure",
      vital_category = "sbp",
      vital_value = rnorm(n_measurements, mean = 120, sd = 20),
      meas_site_name = "Arterial Line"
    )

    vitals_dbp <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      recorded_dttm = time_seq,
      vital_name = "Diastolic Blood Pressure",
      vital_category = "dbp",
      vital_value = rnorm(n_measurements, mean = 70, sd = 15),
      meas_site_name = "Arterial Line"
    )

    vitals_spo2 <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      recorded_dttm = time_seq,
      vital_name = "Oxygen Saturation",
      vital_category = "spo2",
      vital_value = pmin(100, rnorm(n_measurements, mean = 96, sd = 3)),
      meas_site_name = "Finger"
    )

    vitals_rr <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      recorded_dttm = time_seq,
      vital_name = "Respiratory Rate",
      vital_category = "respiratory_rate",
      vital_value = rnorm(n_measurements, mean = 16, sd = 4),
      meas_site_name = NA_character_
    )

    vitals_list[[length(vitals_list) + 1]] <- bind_rows(
      vitals_temp, vitals_hr, vitals_sbp, vitals_dbp, vitals_spo2, vitals_rr
    )
  }
}

vitals <- bind_rows(vitals_list) %>%
  # Ensure values are within valid ranges
  mutate(
    vital_value = case_when(
      vital_category == "temp_c" ~ pmax(35, pmin(42, vital_value)),
      vital_category == "heart_rate" ~ pmax(40, pmin(200, vital_value)),
      vital_category == "sbp" ~ pmax(60, pmin(250, vital_value)),
      vital_category == "dbp" ~ pmax(30, pmin(150, vital_value)),
      vital_category == "spo2" ~ pmax(70, pmin(100, vital_value)),
      vital_category == "respiratory_rate" ~ pmax(8, pmin(40, vital_value)),
      TRUE ~ vital_value
    ),
    vital_value = round(vital_value, 1)
  )

write_csv(vitals, file.path(output_dir, "vitals.csv"))
cat("  Generated", nrow(vitals), "vital sign measurements\n")

# ============================================================================
# 5. LABS TABLE
# ============================================================================

cat("Generating labs data...\n")

# Generate daily labs
labs_list <- list()

for (i in 1:nrow(hospitalization)) {
  hosp <- hospitalization[i, ]

  # Skip if invalid dates
  if (is.na(hosp$admission_dttm) || is.na(hosp$discharge_dttm)) {
    next
  }

  # Daily labs
  lab_dates <- seq(
    from = as.Date(hosp$admission_dttm),
    to = as.Date(hosp$discharge_dttm),
    by = "1 day"
  )

  for (lab_date in lab_dates) {
    # Order time (early morning)
    order_time <- as.POSIXct(lab_date, tz = tz) + hours(6) + minutes(sample(0:59, 1))
    collect_time <- order_time + minutes(sample(15:45, 1))
    result_time <- collect_time + hours(sample(1:4, 1))

    # Basic metabolic panel (BMP)
    bmp_labs <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      lab_order_dttm = order_time,
      lab_collect_dttm = collect_time,
      lab_result_dttm = result_time,
      lab_order_name = "Basic Metabolic Panel",
      lab_order_category = "bmp",
      lab_name = c("Sodium", "Potassium", "Chloride", "Bicarbonate", "BUN", "Creatinine", "Glucose"),
      lab_category = c("sodium", "potassium", "chloride", "bicarbonate", "bun", "creatinine", "glucose"),
      lab_value = c(
        rnorm(1, 140, 3),     # Sodium
        rnorm(1, 4.0, 0.4),   # Potassium
        rnorm(1, 100, 4),     # Chloride
        rnorm(1, 24, 3),      # Bicarbonate
        rnorm(1, 15, 5),      # BUN
        rnorm(1, 1.0, 0.3),   # Creatinine
        rnorm(1, 100, 20)     # Glucose
      ),
      lab_units = c("mmol/L", "mmol/L", "mmol/L", "mmol/L", "mg/dL", "mg/dL", "mg/dL"),
      reference_unit = lab_units
    )

    # Complete blood count (CBC)
    cbc_labs <- tibble(
      hospitalization_id = hosp$hospitalization_id,
      lab_order_dttm = order_time,
      lab_collect_dttm = collect_time,
      lab_result_dttm = result_time,
      lab_order_name = "Complete Blood Count",
      lab_order_category = "cbc",
      lab_name = c("WBC", "Hemoglobin", "Hematocrit", "Platelets"),
      lab_category = c("wbc", "hemoglobin", "hematocrit", "platelets"),
      lab_value = c(
        rnorm(1, 8, 2),       # WBC
        rnorm(1, 12, 2),      # Hemoglobin
        rnorm(1, 36, 5),      # Hematocrit
        rnorm(1, 200, 50)     # Platelets
      ),
      lab_units = c("K/uL", "g/dL", "%", "K/uL"),
      reference_unit = lab_units
    )

    labs_list[[length(labs_list) + 1]] <- bind_rows(bmp_labs, cbc_labs)
  }
}

labs <- bind_rows(labs_list) %>%
  mutate(lab_value = round(lab_value, 2))

write_csv(labs, file.path(output_dir, "labs.csv"))
cat("  Generated", nrow(labs), "lab results\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== Summary ===\n")
cat("Output directory:", output_dir, "\n")
cat("Files generated:\n")
cat("  - patient.csv (", nrow(patient), "rows )\n")
cat("  - hospitalization.csv (", nrow(hospitalization), "rows )\n")
cat("  - adt.csv (", nrow(adt), "rows )\n")
cat("  - vitals.csv (", nrow(vitals), "rows )\n")
cat("  - labs.csv (", nrow(labs), "rows )\n")
cat("\nThese files can be used for both R (clifR) and Python (clifpy) testing.\n")
cat("Random seed was 42 for reproducibility.\n")
