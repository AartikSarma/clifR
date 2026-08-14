#' Comorbidity index calculation utilities
#'
#' Port of `clifpy/utils/comorbidity.py` (clifpy 0.5.0). Calculates the Charlson
#' Comorbidity Index (CCI) and the Elixhauser Comorbidity Index from hospital
#' diagnosis data, using the Quan (2011) ICD-10-CM adaptations. All condition
#' code lists, weights and hierarchy definitions are read from the packaged YAML
#' configurations (`inst/extdata/comorbidity/cci.yaml` and
#' `inst/extdata/comorbidity/elixhauser.yaml`), which are copied verbatim from
#' clifpy — nothing is hardcoded in R.
#'
#' @name clif-comorbidity
NULL

# ---------------------------------------------------------------------------
# Configuration loading
# ---------------------------------------------------------------------------

# Load a comorbidity configuration YAML from inst/extdata/comorbidity.
# Mirrors clifpy's _load_cci_config / _load_elix_config.
load_comorbidity_config <- function(config_filename) {
  config_path <- clif_extdata_path("comorbidity", config_filename)
  if (is.null(config_path) || !nzchar(config_path) || !file.exists(config_path)) {
    cli::cli_abort(
      "Comorbidity configuration file not found: {.file comorbidity/{config_filename}}"
    )
  }
  # readLines.warn = FALSE: the YAML files are byte-identical copies of clifpy's
  # and lack a trailing newline, which is not a problem worth warning about.
  yaml::read_yaml(config_path, readLines.warn = FALSE)
}

# ---------------------------------------------------------------------------
# Input coercion
# ---------------------------------------------------------------------------

# Accept either a HospitalDiagnosis R6 table object (anything exposing a `df`
# field, mirroring clifpy's hasattr(x, "df") check) or a plain data frame.
resolve_hospital_diagnosis_data <- function(hospital_diagnosis) {
  if (inherits(hospital_diagnosis, "R6") && !is.null(hospital_diagnosis$df)) {
    return(dplyr::as_tibble(hospital_diagnosis$df))
  }
  if (is.data.frame(hospital_diagnosis)) {
    return(dplyr::as_tibble(hospital_diagnosis))
  }
  cli::cli_abort(
    "{.arg hospital_diagnosis} must be a HospitalDiagnosis table object or a data frame."
  )
}

# ---------------------------------------------------------------------------
# Shared calculation pipeline
# ---------------------------------------------------------------------------

# Shared implementation behind calculate_cci() and calculate_elix().
# Reproduces clifpy's pipeline step for step:
#   1. filter to diagnosis_code_format == "icd10cm" (case-insensitive)
#   2. lowercase diagnosis codes and strip decimal points ("I21.45" -> "i2145")
#   3. prefix-match the cleaned codes against each condition's code list
#   4. aggregate condition presence per hospitalization_id (max)
#   5. optionally apply hierarchy ("assign0") logic
#   6. compute the weighted score
calculate_comorbidity_index <- function(hospital_diagnosis,
                                        comorbidity_config,
                                        score_column_name,
                                        hierarchy = TRUE) {
  diagnosis_data <- resolve_hospital_diagnosis_data(hospital_diagnosis)

  required_columns <- c("hospitalization_id", "diagnosis_code", "diagnosis_code_format")
  missing_columns <- setdiff(required_columns, names(diagnosis_data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "Missing required columns in diagnosis data: {.field {missing_columns}}"
    )
  }

  # Echo configuration provenance, as clifpy does.
  cli::cli_alert_info(
    "{comorbidity_config$name} (version: {comorbidity_config$version}; supported formats: {paste(comorbidity_config$supported_formats, collapse = ', ')})"
  )

  # Step 1: keep only ICD-10-CM rows (case-insensitive; other formats discarded).
  diagnosis_filtered <- diagnosis_data |>
    dplyr::filter(tolower(.data$diagnosis_code_format) == "icd10cm")

  # Step 2: lowercase and remove decimals from the diagnosis codes.
  cleaned_diagnosis_codes <- gsub(
    ".", "", tolower(diagnosis_filtered$diagnosis_code),
    fixed = TRUE
  )

  # Step 3: prefix-match each condition's code list against the cleaned codes.
  condition_mappings <- comorbidity_config$diagnosis_code_mappings$ICD10CM
  condition_names <- names(condition_mappings)

  condition_presence <- lapply(condition_mappings, function(condition_info) {
    condition_matched <- rep(FALSE, length(cleaned_diagnosis_codes))
    for (condition_code in condition_info$codes) {
      condition_matched <- condition_matched |
        startsWith(cleaned_diagnosis_codes, tolower(condition_code))
    }
    condition_matched
  })
  names(condition_presence) <- condition_names

  diagnosis_with_conditions <- dplyr::bind_cols(
    dplyr::tibble(hospitalization_id = as.character(diagnosis_filtered$hospitalization_id)),
    dplyr::as_tibble(condition_presence)
  )

  # Step 4: aggregate condition presence per hospitalization (max over rows).
  # na.rm = TRUE matches polars' null-ignoring max() aggregation.
  condition_flags <- diagnosis_with_conditions |>
    dplyr::group_by(.data$hospitalization_id) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(condition_names),
        \(presence_values) as.integer(any(presence_values, na.rm = TRUE))
      ),
      .groups = "drop"
    )

  # Step 5: hierarchy ("assign0") logic.
  if (hierarchy) {
    condition_flags <- apply_hierarchy_logic(
      condition_flags,
      comorbidity_config$hierarchies
    )
  }

  # Step 6: weighted score.
  condition_flags_with_score <- calculate_weighted_comorbidity_score(
    condition_flags,
    comorbidity_config$weights,
    score_column_name
  )

  # Column order matches clifpy: hospitalization_id, conditions in YAML order,
  # then the score column.
  condition_flags_with_score |>
    dplyr::mutate(hospitalization_id = as.character(.data$hospitalization_id)) |>
    dplyr::select(
      "hospitalization_id",
      dplyr::all_of(condition_names),
      dplyr::all_of(score_column_name)
    )
}

# Port of clifpy's _apply_hierarchy_logic: when both the severe and mild forms
# of a condition are present, the mild form is zeroed so it is not double
# counted. The first entry of each hierarchy list is the severe form.
apply_hierarchy_logic <- function(condition_flags, hierarchies) {
  for (condition_list in hierarchies) {
    if (length(condition_list) >= 2) {
      severe_condition <- condition_list[[1]]
      mild_conditions <- condition_list[-1]
      for (mild_condition in mild_conditions) {
        condition_flags[[mild_condition]] <- ifelse(
          condition_flags[[severe_condition]] == 1L,
          0L,
          condition_flags[[mild_condition]]
        )
      }
    }
  }
  condition_flags
}

# Port of clifpy's _calculate_elix_score / _calculate_cci_score: the score is
# the weighted sum of the (post-hierarchy) condition flags.
calculate_weighted_comorbidity_score <- function(condition_flags,
                                                 condition_weights,
                                                 score_column_name) {
  weighted_score <- rep(0L, nrow(condition_flags))
  for (condition_name in names(condition_weights)) {
    if (condition_name %in% names(condition_flags)) {
      weighted_score <- weighted_score +
        condition_flags[[condition_name]] * as.integer(condition_weights[[condition_name]])
    }
  }
  condition_flags[[score_column_name]] <- as.integer(weighted_score)
  condition_flags
}

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Calculate Charlson Comorbidity Index (CCI) for hospitalizations
#'
#' Processes hospital diagnosis data to calculate CCI scores using the Quan
#' (2011) adaptation for ICD-10-CM codes. Only rows with
#' `diagnosis_code_format == "ICD10CM"` (case-insensitive) contribute; other
#' code formats are discarded. Condition definitions and weights are read from
#' the packaged `comorbidity/cci.yaml`, identical to clifpy's.
#'
#' @param hospital_diagnosis A `HospitalDiagnosis` table object or a data frame
#'   with columns `hospitalization_id`, `diagnosis_code` and
#'   `diagnosis_code_format`.
#' @param hierarchy Logical, default `TRUE`. Apply the assign0 hierarchy logic
#'   so that when both mild and severe forms of a condition are present, only
#'   the severe form is counted.
#'
#' @return A tibble with one row per `hospitalization_id`, one binary (0/1)
#'   integer column per Charlson condition (in the order defined by the YAML
#'   configuration), and an integer `cci_score` column with the weighted sum.
#' @export
#'
#' @examples
#' \dontrun{
#' hospital_diagnosis <- load_data("hospital_diagnosis", "data/clif", "parquet")
#' cci_scores <- calculate_cci(hospital_diagnosis)
#' }
calculate_cci <- function(hospital_diagnosis, hierarchy = TRUE) {
  cci_config <- load_comorbidity_config("cci.yaml")
  calculate_comorbidity_index(
    hospital_diagnosis = hospital_diagnosis,
    comorbidity_config = cci_config,
    score_column_name = "cci_score",
    hierarchy = hierarchy
  )
}

#' Calculate Elixhauser Comorbidity Index for hospitalizations
#'
#' Processes hospital diagnosis data to calculate Elixhauser scores using the
#' Quan (2011) adaptation for ICD-10-CM codes with van Walraven weights. Only
#' rows with `diagnosis_code_format == "ICD10CM"` (case-insensitive)
#' contribute; other code formats are discarded. Condition definitions and
#' weights are read from the packaged `comorbidity/elixhauser.yaml`, identical
#' to clifpy's.
#'
#' @param hospital_diagnosis A `HospitalDiagnosis` table object or a data frame
#'   with columns `hospitalization_id`, `diagnosis_code` and
#'   `diagnosis_code_format`.
#' @param hierarchy Logical, default `TRUE`. Apply the assign0 hierarchy logic
#'   so that when both mild and severe forms of a condition are present, only
#'   the severe form is counted.
#'
#' @return A tibble with one row per `hospitalization_id`, one binary (0/1)
#'   integer column per Elixhauser condition (31 conditions, in the order
#'   defined by the YAML configuration), and an integer `elix_score` column
#'   with the van Walraven weighted sum.
#' @export
#'
#' @examples
#' \dontrun{
#' hospital_diagnosis <- load_data("hospital_diagnosis", "data/clif", "parquet")
#' elixhauser_scores <- calculate_elix(hospital_diagnosis)
#' }
calculate_elix <- function(hospital_diagnosis, hierarchy = TRUE) {
  elixhauser_config <- load_comorbidity_config("elixhauser.yaml")
  calculate_comorbidity_index(
    hospital_diagnosis = hospital_diagnosis,
    comorbidity_config = elixhauser_config,
    score_column_name = "elix_score",
    hierarchy = hierarchy
  )
}
