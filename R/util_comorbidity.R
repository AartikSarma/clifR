#' Charlson Comorbidity Index Calculation
#'
#' @description
#' Calculate Charlson Comorbidity Index (CCI) from ICD diagnosis codes.
#' Supports both ICD-9 and ICD-10 coding systems.

#' Get Charlson comorbidity category mappings
#'
#' @description
#' Returns ICD code patterns for Charlson comorbidity categories.
#'
#' @param icd_version Character. ICD version: "9" or "10" (default: "10").
#'
#' @return Named list of regex patterns for each category.
#'
#' @keywords internal
get_charlson_mappings <- function(icd_version = "10") {

  if (icd_version == "9") {
    # ICD-9 code patterns
    list(
      myocardial_infarction = c("^410", "^412"),
      congestive_heart_failure = c("^428", "^402\\.01", "^402\\.11", "^402\\.91",
                                   "^404\\.01", "^404\\.03", "^404\\.11", "^404\\.13",
                                   "^404\\.91", "^404\\.93"),
      peripheral_vascular_disease = c("^441", "^443\\.9", "^785\\.4", "^V43\\.4",
                                      "^38\\.48", "^38\\.49"),
      cerebrovascular_disease = c("^430", "^431", "^432", "^433", "^434", "^435", "^436", "^437", "^438"),
      dementia = c("^290", "^291\\.2", "^294\\.1", "^331\\.2"),
      chronic_pulmonary_disease = c("^490", "^491", "^492", "^493", "^494", "^495",
                                    "^496", "^500", "^501", "^502", "^503", "^504", "^505"),
      rheumatic_disease = c("^710\\.0", "^710\\.1", "^710\\.4", "^714\\.0", "^714\\.1",
                           "^714\\.2", "^714\\.81", "^725"),
      peptic_ulcer_disease = c("^531", "^532", "^533", "^534"),
      mild_liver_disease = c("^571\\.2", "^571\\.4", "^571\\.5", "^571\\.6"),
      diabetes_without_complication = c("^250\\.0", "^250\\.1", "^250\\.2", "^250\\.3", "^250\\.7"),
      diabetes_with_complication = c("^250\\.4", "^250\\.5", "^250\\.6"),
      hemiplegia_paraplegia = c("^342", "^343", "^344\\.1"),
      renal_disease = c("^582", "^583", "^585", "^586", "^588"),
      malignancy = c("^1[4-9][0-9]", "^20[0-8]"),
      moderate_severe_liver_disease = c("^572\\.2", "^572\\.3", "^572\\.4"),
      metastatic_solid_tumor = c("^19[6-9]"),
      aids_hiv = c("^042", "^043", "^044")
    )
  } else {
    # ICD-10 code patterns
    list(
      myocardial_infarction = c("^I21", "^I22", "^I25\\.2"),
      congestive_heart_failure = c("^I09\\.9", "^I11\\.0", "^I13\\.0", "^I13\\.2",
                                   "^I25\\.5", "^I42\\.0", "^I42\\.5", "^I42\\.6",
                                   "^I42\\.7", "^I42\\.8", "^I42\\.9", "^I43",
                                   "^I50", "^P29\\.0"),
      peripheral_vascular_disease = c("^I70", "^I71", "^I73\\.1", "^I73\\.8",
                                      "^I73\\.9", "^I77\\.1", "^I79\\.0", "^I79\\.2",
                                      "^K55\\.1", "^K55\\.8", "^K55\\.9", "^Z95\\.8", "^Z95\\.9"),
      cerebrovascular_disease = c("^G45", "^G46", "^H34\\.0", "^I60", "^I61", "^I62",
                                  "^I63", "^I64", "^I65", "^I66", "^I67", "^I68", "^I69"),
      dementia = c("^F00", "^F01", "^F02", "^F03", "^F05\\.1", "^G30", "^G31\\.1"),
      chronic_pulmonary_disease = c("^I27\\.8", "^I27\\.9", "^J40", "^J41", "^J42",
                                    "^J43", "^J44", "^J45", "^J46", "^J47", "^J60",
                                    "^J61", "^J62", "^J63", "^J64", "^J65", "^J66", "^J67"),
      rheumatic_disease = c("^M05", "^M06", "^M31\\.5", "^M32", "^M33", "^M34", "^M35\\.1", "^M35\\.3", "^M36\\.0"),
      peptic_ulcer_disease = c("^K25", "^K26", "^K27", "^K28"),
      mild_liver_disease = c("^B18", "^K70\\.0", "^K70\\.1", "^K70\\.2", "^K70\\.3",
                            "^K70\\.9", "^K71\\.3", "^K71\\.4", "^K71\\.5", "^K71\\.7",
                            "^K73", "^K74", "^K76\\.0", "^K76\\.2", "^K76\\.3",
                            "^K76\\.4", "^K76\\.8", "^K76\\.9", "^Z94\\.4"),
      diabetes_without_complication = c("^E10\\.0", "^E10\\.1", "^E10\\.6", "^E10\\.8",
                                        "^E10\\.9", "^E11\\.0", "^E11\\.1", "^E11\\.6",
                                        "^E11\\.8", "^E11\\.9", "^E12\\.0", "^E12\\.1",
                                        "^E12\\.6", "^E12\\.8", "^E12\\.9", "^E13\\.0",
                                        "^E13\\.1", "^E13\\.6", "^E13\\.8", "^E13\\.9",
                                        "^E14\\.0", "^E14\\.1", "^E14\\.6", "^E14\\.8", "^E14\\.9"),
      diabetes_with_complication = c("^E10\\.2", "^E10\\.3", "^E10\\.4", "^E10\\.5", "^E10\\.7",
                                     "^E11\\.2", "^E11\\.3", "^E11\\.4", "^E11\\.5", "^E11\\.7",
                                     "^E12\\.2", "^E12\\.3", "^E12\\.4", "^E12\\.5", "^E12\\.7",
                                     "^E13\\.2", "^E13\\.3", "^E13\\.4", "^E13\\.5", "^E13\\.7",
                                     "^E14\\.2", "^E14\\.3", "^E14\\.4", "^E14\\.5", "^E14\\.7"),
      hemiplegia_paraplegia = c("^G04\\.1", "^G11\\.4", "^G80\\.1", "^G80\\.2", "^G81", "^G82", "^G83\\.0", "^G83\\.4", "^G83\\.9"),
      renal_disease = c("^I12\\.0", "^I13\\.1", "^N03\\.2", "^N03\\.3", "^N03\\.4",
                       "^N03\\.5", "^N03\\.6", "^N03\\.7", "^N05\\.2", "^N05\\.3",
                       "^N05\\.4", "^N05\\.5", "^N05\\.6", "^N05\\.7", "^N18",
                       "^N19", "^N25\\.0", "^Z49\\.0", "^Z49\\.1", "^Z49\\.2",
                       "^Z94\\.0", "^Z99\\.2"),
      malignancy = c("^C00", "^C01", "^C02", "^C03", "^C04", "^C05", "^C06", "^C07",
                    "^C08", "^C09", "^C1[0-9]", "^C2[0-6]", "^C3[0-9]", "^C4[0-1]",
                    "^C43", "^C4[5-9]", "^C5[0-8]", "^C6[0-9]", "^C7[0-6]",
                    "^C81", "^C82", "^C83", "^C84", "^C85", "^C88", "^C90", "^C91",
                    "^C92", "^C93", "^C94", "^C95", "^C96", "^C97"),
      moderate_severe_liver_disease = c("^I85\\.0", "^I85\\.9", "^I86\\.4", "^I98\\.2",
                                        "^K70\\.4", "^K71\\.1", "^K72\\.1", "^K72\\.9",
                                        "^K76\\.5", "^K76\\.6", "^K76\\.7"),
      metastatic_solid_tumor = c("^C77", "^C78", "^C79", "^C80"),
      aids_hiv = c("^B20", "^B21", "^B22", "^B24")
    )
  }
}

#' Get Charlson comorbidity weights
#'
#' @keywords internal
get_charlson_weights <- function() {
  c(
    myocardial_infarction = 1,
    congestive_heart_failure = 1,
    peripheral_vascular_disease = 1,
    cerebrovascular_disease = 1,
    dementia = 1,
    chronic_pulmonary_disease = 1,
    rheumatic_disease = 1,
    peptic_ulcer_disease = 1,
    mild_liver_disease = 1,
    diabetes_without_complication = 1,
    diabetes_with_complication = 2,
    hemiplegia_paraplegia = 2,
    renal_disease = 2,
    malignancy = 2,
    moderate_severe_liver_disease = 3,
    metastatic_solid_tumor = 6,
    aids_hiv = 6
  )
}

#' Classify ICD code into Charlson categories
#'
#' @description
#' Classify a single ICD code into Charlson comorbidity categories.
#'
#' @param icd_code Character. ICD diagnosis code.
#' @param icd_version Character. ICD version: "9" or "10" (default: "10").
#'
#' @return Character vector of matched categories.
#'
#' @keywords internal
classify_charlson_code <- function(icd_code, icd_version = "10") {
  # Remove periods and spaces from ICD code
  clean_code <- gsub("[\\. ]", "", toupper(icd_code))

  mappings <- get_charlson_mappings(icd_version)
  matched_categories <- c()

  for (category in names(mappings)) {
    patterns <- mappings[[category]]
    if (any(sapply(patterns, function(p) grepl(p, clean_code)))) {
      matched_categories <- c(matched_categories, category)
    }
  }

  return(matched_categories)
}

#' Calculate Charlson Comorbidity Index
#'
#' @description
#' Calculate CCI score from a vector of ICD diagnosis codes.
#'
#' @param icd_codes Character vector of ICD diagnosis codes.
#' @param icd_version Character. ICD version: "9" or "10" (default: "10").
#' @param age Numeric. Patient age for age-adjusted CCI (optional).
#' @param return_categories Logical. Return identified categories (default: FALSE).
#'
#' @return List with CCI score and optionally matched categories.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # ICD-10 codes
#' calculate_charlson(
#'   icd_codes = c("I21.0", "E11.9", "I50.9"),
#'   age = 65
#' )
#'
#' # ICD-9 codes
#' calculate_charlson(
#'   icd_codes = c("410.0", "250.00", "428.0"),
#'   icd_version = "9",
#'   age = 70
#' )
#' }
calculate_charlson <- function(icd_codes,
                               icd_version = "10",
                               age = NULL,
                               return_categories = FALSE) {

  if (length(icd_codes) == 0 || all(is.na(icd_codes))) {
    result <- list(
      cci_score = 0,
      age_adjusted_cci = if (!is.null(age)) 0 else NULL,
      n_codes = 0,
      n_categories = 0
    )
    if (return_categories) {
      result$categories <- character(0)
    }
    return(result)
  }

  # Remove NA codes
  icd_codes <- icd_codes[!is.na(icd_codes)]

  # Classify all codes
  all_categories <- unique(unlist(
    lapply(icd_codes, classify_charlson_code, icd_version = icd_version)
  ))

  # Handle hierarchical exclusions
  # If diabetes with complication, remove diabetes without complication
  if ("diabetes_with_complication" %in% all_categories) {
    all_categories <- setdiff(all_categories, "diabetes_without_complication")
  }

  # If moderate/severe liver disease, remove mild liver disease
  if ("moderate_severe_liver_disease" %in% all_categories) {
    all_categories <- setdiff(all_categories, "mild_liver_disease")
  }

  # If metastatic tumor, remove malignancy
  if ("metastatic_solid_tumor" %in% all_categories) {
    all_categories <- setdiff(all_categories, "malignancy")
  }

  # Calculate score
  weights <- get_charlson_weights()
  cci_score <- sum(weights[all_categories])

  # Age adjustment
  age_adjusted_cci <- if (!is.null(age)) {
    age_points <- if (age < 50) {
      0
    } else if (age < 60) {
      1
    } else if (age < 70) {
      2
    } else if (age < 80) {
      3
    } else {
      4
    }
    cci_score + age_points
  } else {
    NULL
  }

  result <- list(
    cci_score = cci_score,
    age_adjusted_cci = age_adjusted_cci,
    n_codes = length(icd_codes),
    n_categories = length(all_categories)
  )

  if (return_categories) {
    result$categories <- all_categories
  }

  return(result)
}

#' Calculate Charlson scores for multiple patients
#'
#' @description
#' Batch calculate CCI scores from a dataframe with diagnosis codes.
#'
#' @param data tibble with patient diagnosis data.
#' @param patient_id_col Character. Patient/hospitalization ID column name.
#' @param icd_code_col Character. ICD code column name.
#' @param icd_version_col Character. ICD version column name (optional).
#' @param age_col Character. Age column name (optional).
#' @param default_icd_version Character. Default ICD version if not in data (default: "10").
#'
#' @return tibble with CCI scores per patient.
#'
#' @export
calculate_charlson_scores <- function(data,
                                      patient_id_col = "hospitalization_id",
                                      icd_code_col = "icd_code",
                                      icd_version_col = NULL,
                                      age_col = NULL,
                                      default_icd_version = "10") {

  if (!(patient_id_col %in% names(data))) {
    cli::cli_abort("Patient ID column {patient_id_col} not found")
  }

  if (!(icd_code_col %in% names(data))) {
    cli::cli_abort("ICD code column {icd_code_col} not found")
  }

  cli::cli_alert_info("Calculating Charlson scores for {length(unique(data[[patient_id_col]]))} patients")

  # Group by patient and calculate CCI
  cci_results <- data %>%
    dplyr::group_by(.data[[patient_id_col]]) %>%
    dplyr::summarize(
      cci_result = list(calculate_charlson(
        icd_codes = .data[[icd_code_col]],
        icd_version = if (!is.null(icd_version_col) && icd_version_col %in% names(data)) {
          dplyr::first(.data[[icd_version_col]])
        } else {
          default_icd_version
        },
        age = if (!is.null(age_col) && age_col %in% names(data)) {
          dplyr::first(.data[[age_col]])
        } else {
          NULL
        },
        return_categories = TRUE
      )),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cci_score = purrr::map_dbl(cci_result, ~.x$cci_score),
      age_adjusted_cci = purrr::map_dbl(cci_result, ~.x$age_adjusted_cci %||% NA_real_),
      n_diagnosis_codes = purrr::map_int(cci_result, ~.x$n_codes),
      n_comorbidity_categories = purrr::map_int(cci_result, ~.x$n_categories),
      comorbidity_categories = purrr::map_chr(cci_result, ~paste(.x$categories, collapse = "; "))
    ) %>%
    dplyr::select(-cci_result)

  cli::cli_alert_success("Charlson calculation complete")
  cli::cli_text("Mean CCI score: {round(mean(cci_results$cci_score, na.rm = TRUE), 1)}")

  return(cci_results)
}

#' Get CCI category descriptions
#'
#' @description
#' Return human-readable descriptions of Charlson categories.
#'
#' @return Named character vector of category descriptions.
#'
#' @export
get_charlson_category_descriptions <- function() {
  c(
    myocardial_infarction = "Myocardial Infarction (1 pt)",
    congestive_heart_failure = "Congestive Heart Failure (1 pt)",
    peripheral_vascular_disease = "Peripheral Vascular Disease (1 pt)",
    cerebrovascular_disease = "Cerebrovascular Disease (1 pt)",
    dementia = "Dementia (1 pt)",
    chronic_pulmonary_disease = "Chronic Pulmonary Disease (1 pt)",
    rheumatic_disease = "Rheumatic Disease (1 pt)",
    peptic_ulcer_disease = "Peptic Ulcer Disease (1 pt)",
    mild_liver_disease = "Mild Liver Disease (1 pt)",
    diabetes_without_complication = "Diabetes without Complication (1 pt)",
    diabetes_with_complication = "Diabetes with Complication (2 pts)",
    hemiplegia_paraplegia = "Hemiplegia or Paraplegia (2 pts)",
    renal_disease = "Renal Disease (2 pts)",
    malignancy = "Malignancy (2 pts)",
    moderate_severe_liver_disease = "Moderate/Severe Liver Disease (3 pts)",
    metastatic_solid_tumor = "Metastatic Solid Tumor (6 pts)",
    aids_hiv = "AIDS/HIV (6 pts)"
  )
}

#' Summarize Charlson comorbidities in a cohort
#'
#' @description
#' Generate summary statistics for Charlson comorbidities in a cohort.
#'
#' @param cci_data tibble with CCI scores (output from calculate_charlson_scores).
#' @param stratify_col Character. Column name for stratification (optional).
#'
#' @return tibble with summary statistics.
#'
#' @export
summarize_charlson_cohort <- function(cci_data, stratify_col = NULL) {

  if (!is.null(stratify_col) && stratify_col %in% names(cci_data)) {
    # Stratified summary
    summary_data <- cci_data %>%
      dplyr::group_by(.data[[stratify_col]]) %>%
      dplyr::summarize(
        n_patients = dplyr::n(),
        mean_cci = mean(cci_score, na.rm = TRUE),
        median_cci = median(cci_score, na.rm = TRUE),
        sd_cci = sd(cci_score, na.rm = TRUE),
        min_cci = min(cci_score, na.rm = TRUE),
        max_cci = max(cci_score, na.rm = TRUE),
        cci_0 = sum(cci_score == 0, na.rm = TRUE),
        cci_1_2 = sum(cci_score >= 1 & cci_score <= 2, na.rm = TRUE),
        cci_3_4 = sum(cci_score >= 3 & cci_score <= 4, na.rm = TRUE),
        cci_5_plus = sum(cci_score >= 5, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # Overall summary
    summary_data <- cci_data %>%
      dplyr::summarize(
        n_patients = dplyr::n(),
        mean_cci = mean(cci_score, na.rm = TRUE),
        median_cci = median(cci_score, na.rm = TRUE),
        sd_cci = sd(cci_score, na.rm = TRUE),
        min_cci = min(cci_score, na.rm = TRUE),
        max_cci = max(cci_score, na.rm = TRUE),
        cci_0 = sum(cci_score == 0, na.rm = TRUE),
        cci_1_2 = sum(cci_score >= 1 & cci_score <= 2, na.rm = TRUE),
        cci_3_4 = sum(cci_score >= 3 & cci_score <= 4, na.rm = TRUE),
        cci_5_plus = sum(cci_score >= 5, na.rm = TRUE)
      )
  }

  cli::cli_h2("Charlson Comorbidity Index Summary")
  print(summary_data)

  return(summary_data)
}
