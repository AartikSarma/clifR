#' SOFA score computation from a wide dataset
#'
#' Port of `clifpy/utils/sofa.py`. The Python implementation is a chain of DuckDB
#' queries, so this port runs the same SQL through R's `duckdb` package rather
#' than reimplementing the logic in dplyr: the aggregation, the `CASE` ladders and
#' the null semantics then behave identically in both languages.
#'
#' The pipeline is order-sensitive:
#' 1. outlier clamping (`po2_arterial`, `fio2_set`, `spo2`),
#' 2. PaO2 imputation from SpO2 (Severinghaus, only where `spo2 < 97`),
#' 3. extremal (worst) aggregation per ID,
#' 4. component scoring from the aggregated values.
#'
#' @name clif-sofa
NULL

#' Categories required from each CLIF table for SOFA scoring
#'
#' Named list mirroring clifpy's `REQUIRED_SOFA_CATEGORIES_BY_TABLE`; used as the
#' `category_filters` argument when building a wide dataset for SOFA.
#'
#' @export
REQUIRED_SOFA_CATEGORIES_BY_TABLE <- list(
  labs = c("creatinine", "platelet_count", "po2_arterial", "bilirubin_total"),
  vitals = c("map", "spo2"),
  patient_assessments = c("gcs_total"),
  medication_admin_continuous = c("norepinephrine", "epinephrine", "dopamine", "dobutamine"),
  respiratory_support = c("device_category", "fio2_set")
)

#' Wide-dataset columns aggregated with MAX when taking the worst value
#' @export
MAX_ITEMS <- c(
  "norepinephrine_mcg_kg_min", "epinephrine_mcg_kg_min",
  "dopamine_mcg_kg_min", "dobutamine_mcg_kg_min",
  "fio2_set", "creatinine", "bilirubin_total"
)

#' Wide-dataset columns aggregated with MIN when taking the worst value
#' @export
MIN_ITEMS <- c("map", "spo2", "po2_arterial", "pao2_imputed", "platelet_count", "gcs_total")

#' Respiratory device severity ranks (lower rank = more support)
#' @export
DEVICE_RANK_DICT <- c(
  "IMV" = 1L,
  "NIPPV" = 2L,
  "CPAP" = 3L,
  "High Flow NC" = 4L,
  "Face Mask" = 5L,
  "Trach Collar" = 6L,
  "Nasal Cannula" = 7L,
  "Other" = 8L,
  "Room Air" = 9L
)

#' Device rank lookup table
#'
#' Tibble form of [DEVICE_RANK_DICT], joined onto the wide dataset so that the
#' worst (lowest-ranked) device per ID can be recovered after aggregation.
#'
#' @export
DEVICE_RANK_MAPPING <- tibble::tibble(
  device_category = names(DEVICE_RANK_DICT),
  device_rank = unname(DEVICE_RANK_DICT)
)

#' Format a character vector as a DuckDB list literal
#'
#' @param values Character vector.
#' @return A single string such as `['a', 'b']`.
#' @keywords internal
sql_list_literal <- function(values) {
  paste0("[", paste(sql_quote_value(values), collapse = ", "), "]")
}

#' Impute PaO2 from SpO2 using the Severinghaus equation
#'
#' Adds a `pao2_imputed` column, populated only where `spo2 < 97` because the
#' oxygen dissociation curve is too flat above that to invert reliably.
#'
#' @param connection An open DuckDB connection.
#' @param source_table Name of the table to read from.
#' @param target_table Name of the temp table to create.
#' @return The target table name, invisibly.
#' @keywords internal
impute_pao2_from_spo2 <- function(connection, source_table, target_table) {
  query <- sprintf(
    "CREATE OR REPLACE TEMP TABLE %s AS
     FROM %s
     SELECT *
        , _s: spo2 / 100
        , _a: 11700.0 / ( (1/_s) - 1 )
        , _b: sqrt(50^3 + (_a)^2)
        , pao2_imputed: CASE
            WHEN spo2 < 97 THEN ( _b + _a)^(1.0/3.0) - (_b - _a)^(1.0/3.0)
            END",
    target_table, source_table
  )
  DBI::dbExecute(connection, query)
  invisible(target_table)
}

#' Aggregate extremal (worst) values by ID
#'
#' MAX for the variables that are worse when higher ([MAX_ITEMS]), MIN for those
#' that are worse when lower ([MIN_ITEMS]), plus the lowest device rank seen.
#'
#' @param connection An open DuckDB connection.
#' @param source_table Name of the table to read from.
#' @param target_table Name of the temp table to create.
#' @param extremal_type Either `"worst"` or `"latest"` (not implemented).
#' @param id_name Grouping column.
#' @return The target table name, invisibly.
#' @keywords internal
agg_extremal_values_by_id <- function(connection, source_table, target_table,
                                      extremal_type, id_name) {
  if (identical(extremal_type, "latest")) {
    cli::cli_abort("This is a future feature and currently unavailable.")
  }
  if (!identical(extremal_type, "worst")) {
    cli::cli_abort("Invalid extremal type: {.val {extremal_type}}")
  }

  query <- sprintf(
    'CREATE OR REPLACE TEMP TABLE %s AS
     FROM %s
     LEFT JOIN DEVICE_RANK_MAPPING USING (device_category)
     SELECT "%s"
        , MAX(COLUMNS(%s))
        , MIN(COLUMNS(%s))
        , device_rank: MIN(device_rank)
     GROUP BY "%s"',
    target_table, source_table, id_name,
    sql_list_literal(MAX_ITEMS), sql_list_literal(MIN_ITEMS), id_name
  )
  DBI::dbExecute(connection, query)
  invisible(target_table)
}

#' Compute the six SOFA component scores from aggregated extremal values
#'
#' @param connection An open DuckDB connection.
#' @param source_table Table holding one row per ID of extremal values.
#' @param id_name Grouping column.
#' @return A data frame of component scores and the total.
#' @keywords internal
compute_sofa_from_extremal_values <- function(connection, source_table, id_name) {
  query <- sprintf(
    'FROM %s df
     LEFT JOIN DEVICE_RANK_MAPPING m on df.device_rank = m.device_rank
     SELECT "%s"
        , p_f: po2_arterial / fio2_set
        , p_f_imputed: pao2_imputed / fio2_set
        , sofa_cv_97: CASE
            WHEN dopamine_mcg_kg_min > 15 OR epinephrine_mcg_kg_min > 0.1 OR norepinephrine_mcg_kg_min > 0.1 THEN 4
            WHEN dopamine_mcg_kg_min > 5 OR epinephrine_mcg_kg_min <= 0.1 OR norepinephrine_mcg_kg_min <= 0.1 THEN 3
            WHEN dopamine_mcg_kg_min <= 5 OR dobutamine_mcg_kg_min > 0 THEN 2
            WHEN map < 70 THEN 1
            WHEN map >= 70 THEN 0 END
        , sofa_coag: CASE WHEN platelet_count < 20 THEN 4
            WHEN platelet_count < 50 THEN 3
            WHEN platelet_count < 100 THEN 2
            WHEN platelet_count < 150 THEN 1
            WHEN platelet_count >= 150 THEN 0 END
        , sofa_liver: CASE WHEN bilirubin_total >= 12 THEN 4
            WHEN bilirubin_total < 12 AND bilirubin_total >= 6 THEN 3
            WHEN bilirubin_total < 6 AND bilirubin_total >= 2 THEN 2
            WHEN bilirubin_total < 2 AND bilirubin_total >= 1.2 THEN 1
            WHEN bilirubin_total < 1.2 THEN 0 END
        , sofa_resp: CASE WHEN p_f < 100 AND m.device_category IN (\'IMV\', \'NIPPV\', \'CPAP\') THEN 4
            WHEN p_f >= 100 and p_f < 200 AND m.device_category IN (\'IMV\', \'NIPPV\', \'CPAP\') THEN 3
            WHEN p_f >= 200 AND p_f < 300 THEN 2
            WHEN p_f >= 300 AND p_f < 400 THEN 1
            WHEN p_f >= 400 THEN 0 END
        , sofa_cns: CASE WHEN gcs_total < 6 THEN 4
            WHEN gcs_total >= 6 AND gcs_total <= 9 THEN 3
            WHEN gcs_total >= 10 AND gcs_total <= 12 THEN 2
            WHEN gcs_total >= 13 AND gcs_total <= 14 THEN 1
            WHEN gcs_total == 15 THEN 0 END
        , sofa_renal: CASE WHEN creatinine >= 5 THEN 4
            WHEN creatinine < 5 AND creatinine >= 3.5 THEN 3
            WHEN creatinine < 3.5 AND creatinine >= 2 THEN 2
            WHEN creatinine < 2 AND creatinine >= 1.2 THEN 1
            WHEN creatinine < 1.2 THEN 0 END
        , sofa_total: sofa_cv_97 + sofa_coag + sofa_liver + sofa_resp + sofa_renal + sofa_cns',
    source_table, id_name
  )
  DBI::dbGetQuery(connection, query)
}

#' Names of the six SOFA component score columns
#' @keywords internal
SOFA_SUBSCORE_COLUMNS <- c(
  "sofa_cv_97", "sofa_coag", "sofa_renal", "sofa_liver", "sofa_resp", "sofa_cns"
)

#' Fill missing SOFA component scores with zero
#'
#' The total is first recomputed ignoring missing components (a missing component
#' contributes nothing), then the components themselves are filled with 0.
#'
#' @param sofa_scores Data frame of component scores.
#' @return The data frame with filled components and a recomputed total.
#' @keywords internal
fill_na_scores <- function(sofa_scores) {
  subscore_matrix <- as.matrix(sofa_scores[, SOFA_SUBSCORE_COLUMNS, drop = FALSE])
  sofa_scores[["sofa_total"]] <- as.integer(rowSums(subscore_matrix, na.rm = TRUE))

  for (column_name in SOFA_SUBSCORE_COLUMNS) {
    column_values <- sofa_scores[[column_name]]
    column_values[is.na(column_values)] <- 0L
    sofa_scores[[column_name]] <- as.integer(column_values)
  }
  sofa_scores
}

#' Compute SOFA scores from a wide dataset
#'
#' Port of `clifpy.utils.sofa.compute_sofa`. Takes an already-built wide dataset
#' and reduces it to one worst-value row per ID, then scores the six SOFA
#' components. Medication columns must already be converted to standard units
#' (e.g. `norepinephrine_mcg_kg_min` rather than `norepinephrine`).
#'
#' @param wide_df Wide dataset (one row per ID/time point) containing the SOFA
#'   variables: `po2_arterial`, `fio2_set`, `spo2`, `map`, `platelet_count`,
#'   `bilirubin_total`, `creatinine`, `gcs_total`, `device_category` and the four
#'   `*_mcg_kg_min` vasoactive columns.
#' @param cohort_df Optional data frame with columns `id_name`, `start_time` and
#'   `end_time`; observations outside those windows are dropped.
#' @param extremal_type `"worst"` (default). `"latest"` is not implemented,
#'   matching clifpy.
#' @param id_name Grouping column, e.g. `"encounter_block"` or
#'   `"hospitalization_id"`.
#' @param fill_na_scores_with_zero If `TRUE` (default), missing component scores
#'   are treated as 0 and the total is the sum of the observed components.
#' @param remove_outliers If `TRUE` (default), clamp `po2_arterial` to
#'   `[0, 700]`, `fio2_set` to `[0.21, 1]` and `spo2` to `[50, 100]`, setting
#'   out-of-range values to missing before scoring.
#'
#' @return A tibble with one row per ID: the ID column, `p_f`, `p_f_imputed`, the
#'   six component scores and `sofa_total`.
#' @export
#'
#' @examples
#' \dontrun{
#' wide_dataset <- arrow::read_parquet("wide_df.parquet")
#' sofa_scores <- compute_sofa(wide_dataset, id_name = "hospitalization_id")
#' }
compute_sofa <- function(wide_df,
                         cohort_df = NULL,
                         extremal_type = "worst",
                         id_name = "encounter_block",
                         fill_na_scores_with_zero = TRUE,
                         remove_outliers = TRUE) {
  if (!extremal_type %in% c("worst", "latest")) {
    cli::cli_abort("extremal_type must be {.val worst} or {.val latest}, got {.val {extremal_type}}")
  }
  if (!id_name %in% names(wide_df)) {
    cli::cli_abort("id_name {.val {id_name}} not found in wide_df columns")
  }

  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  duckdb::duckdb_register(connection, "wide_df", as.data.frame(wide_df))
  duckdb::duckdb_register(connection, "DEVICE_RANK_MAPPING", as.data.frame(DEVICE_RANK_MAPPING))

  current_table <- "wide_df"

  if (!is.null(cohort_df)) {
    required_columns <- c(id_name, "start_time", "end_time")
    missing_columns <- setdiff(required_columns, names(cohort_df))
    if (length(missing_columns) > 0) {
      cli::cli_abort(
        "cohort_df must contain columns {.val {required_columns}}. Missing: {.val {missing_columns}}"
      )
    }
    duckdb::duckdb_register(connection, "cohort_df", as.data.frame(cohort_df))
    DBI::dbExecute(connection, sprintf(
      'CREATE OR REPLACE TEMP TABLE sofa_cohort_filtered AS
       FROM %s w
       INNER JOIN cohort_df c
           ON w."%s" = c."%s"
           AND c.start_time <= w.event_time
           AND c.end_time >= w.event_time
       SELECT w.*',
      current_table, id_name, id_name
    ))
    current_table <- "sofa_cohort_filtered"
  }

  if (remove_outliers) {
    cli::cli_alert_info("Removing outliers from wide dataset")
    DBI::dbExecute(connection, sprintf(
      "CREATE OR REPLACE TEMP TABLE sofa_outliers_removed AS
       FROM %s
       SELECT * REPLACE (
         CASE WHEN po2_arterial BETWEEN 0 AND 700 THEN po2_arterial END AS po2_arterial,
         CASE WHEN fio2_set BETWEEN 0.21 AND 1 THEN fio2_set END AS fio2_set,
         CASE WHEN spo2 BETWEEN 50 AND 100 THEN spo2 END AS spo2
       )",
      current_table
    ))
    current_table <- "sofa_outliers_removed"
  }

  impute_pao2_from_spo2(connection, current_table, "sofa_pao2_imputed")
  agg_extremal_values_by_id(
    connection, "sofa_pao2_imputed", "sofa_extremal_values", extremal_type, id_name
  )
  sofa_scores <- compute_sofa_from_extremal_values(connection, "sofa_extremal_values", id_name)

  if (fill_na_scores_with_zero) {
    sofa_scores <- fill_na_scores(sofa_scores)
  }

  dplyr::as_tibble(sofa_scores)
}
