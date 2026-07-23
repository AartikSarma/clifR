#' Unit converter for standardizing medication dose units
#'
#' Port of `clifpy/utils/unit_converter.py` (clifpy 0.5.0). This module is a
#' DuckDB SQL pipeline in clifpy; per the porting conventions the SQL itself is
#' ported, not just the intent — the same queries run through the R `duckdb`
#' package so that results are bit-for-bit comparable across languages.
#'
#' In general, both rate and amount units are converted indiscriminately and
#' reported alongside unrecognized units.
#'
#' @name clif-unit-converter
NULL

# ---------------------------------------------------------------------------
# Module constants (ported verbatim from clifpy.utils.unit_converter)
# ---------------------------------------------------------------------------

# Replacement -> regex pattern, applied sequentially in this exact order.
# NOTE: plural forms always go first to avoid results like "us" or "gs".
UNIT_NAMING_VARIANTS <- c(
  # time
  "/hr" = "/h(r|our)?$",
  "/min" = "/m(in|inute)?$",
  # unit
  "u" = "u(nits|nit)?",
  # milli
  "m" = "milli-?",
  # volume
  "l" = "l(iters|itres|itre|iter)?",
  # mass ("µ" is the micro sign, "μ" is the Greek letter mu)
  "mcg" = "^(u|µ|μ)g",
  "g" = "^g(rams|ram)?"
)

AMOUNT_ENDER <- "($|/*)"
MASS_REGEX <- paste0("^(mcg|mg|ng|g)", AMOUNT_ENDER)
VOLUME_REGEX <- paste0("^(l|ml)", AMOUNT_ENDER)
UNIT_REGEX <- paste0("^(u|mu)", AMOUNT_ENDER)

# time
HR_REGEX <- "/hr$"

# mass
MU_REGEX <- paste0("^(mu)", AMOUNT_ENDER)
MG_REGEX <- paste0("^(mg)", AMOUNT_ENDER)
NG_REGEX <- paste0("^(ng)", AMOUNT_ENDER)
G_REGEX <- paste0("^(g)", AMOUNT_ENDER)

# volume
L_REGEX <- paste0("^l", AMOUNT_ENDER)

# weight
LB_REGEX <- "/lb/"
KG_REGEX <- "/kg/"
WEIGHT_REGEX <- "/(lb|kg)/"

# Regex pattern -> SQL conversion-factor expression.
REGEX_TO_FACTOR_MAPPER <- stats::setNames(
  c(
    # time -> /min
    "1/60",
    # volume -> ml
    "1000",
    # unit -> u
    "1/1000",
    # mass -> mcg
    "1000",
    "1/1000",
    "1000000",
    # weight -> /kg
    "weight_kg",
    "weight_kg * 2.20462"
  ),
  c(HR_REGEX, L_REGEX, MU_REGEX, MG_REGEX, NG_REGEX, G_REGEX, KG_REGEX, LB_REGEX)
)

ACCEPTABLE_AMOUNT_UNITS <- c(
  "ml", "l", # volume
  "mu", "u", # unit
  "mcg", "mg", "ng", "g" # mass
)

#' Generate all acceptable rate unit combinations
#'
#' Cartesian product of amount units, weight qualifiers (`/kg`, `/lb`, none)
#' and time units (`/hr`, `/min`). Port of clifpy's `_acceptable_rate_units`.
#'
#' @return Character vector of all valid rate unit combinations.
#' @noRd
acceptable_rate_units <- function() {
  acceptable_weight_units <- c("/kg", "/lb", "")
  acceptable_time_units <- c("/hr", "/min")
  # find the cartesian product of the three sets
  rate_unit_combinations <- expand.grid(
    amount_unit = ACCEPTABLE_AMOUNT_UNITS,
    weight_unit = acceptable_weight_units,
    time_unit = acceptable_time_units,
    stringsAsFactors = FALSE
  )
  unique(paste0(
    rate_unit_combinations$amount_unit,
    rate_unit_combinations$weight_unit,
    rate_unit_combinations$time_unit
  ))
}

ACCEPTABLE_RATE_UNITS <- acceptable_rate_units()

ALL_ACCEPTABLE_UNITS <- union(ACCEPTABLE_RATE_UNITS, ACCEPTABLE_AMOUNT_UNITS)

#' Convert a set of strings to SQL IN-clause format
#'
#' Port of clifpy's `_convert_set_to_str_for_sql`. The result does not include
#' the outer quotes; those are added in the SQL query.
#'
#' @param string_set Character vector of values.
#' @return Single string with items separated by `','`.
#' @noRd
convert_set_to_str_for_sql <- function(string_set) {
  paste(string_set, collapse = "','")
}

RATE_UNITS_STR <- convert_set_to_str_for_sql(ACCEPTABLE_RATE_UNITS)
AMOUNT_UNITS_STR <- convert_set_to_str_for_sql(ACCEPTABLE_AMOUNT_UNITS)

# ---------------------------------------------------------------------------
# DuckDB plumbing
# ---------------------------------------------------------------------------

#' Run a SQL query against registered data frames
#'
#' Registers each element of `tables` under its list name in a fresh DuckDB
#' connection (with clifpy's session settings) and returns the query result.
#' This mirrors Python duckdb's replacement scans, where `duckdb.sql()` finds
#' data frames by variable name in the calling frame.
#'
#' @param query SQL text referencing the registered table names.
#' @param tables Named list of data frames to register.
#' @return A tibble with the query result.
#' @noRd
run_unit_converter_query <- function(query, tables) {
  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  for (registered_name in names(tables)) {
    duckdb::duckdb_register(connection, registered_name, as.data.frame(tables[[registered_name]]))
  }
  dplyr::as_tibble(DBI::dbGetQuery(connection, query))
}

# ---------------------------------------------------------------------------
# Cleaning helpers
# ---------------------------------------------------------------------------

#' Clean dose unit formatting (vector version)
#'
#' Removes whitespace, lowercases, and replaces empty strings with `NA`.
#' Port of clifpy's `_clean_dose_unit_formats` (pandas Series version).
#'
#' @param dose_unit_values Character vector of dose unit strings.
#' @return Character vector with cleaned formatting.
#' @noRd
clean_dose_unit_formats <- function(dose_unit_values) {
  cleaned_values <- stringr::str_to_lower(
    stringr::str_replace_all(dose_unit_values, "\\s+", "")
  )
  dplyr::na_if(cleaned_values, "")
}

#' Clean dose unit formatting via DuckDB
#'
#' Adds a `_clean_unit` column: whitespace removed, lowercased, empty strings
#' replaced with NULL. Port of clifpy's `_clean_dose_unit_formats_duckdb`.
#'
#' @param med_df Data frame containing the column to clean.
#' @param col Name of the column containing dose unit strings.
#' @return A tibble with the new `_clean_unit` column added.
#' @noRd
clean_dose_unit_formats_duckdb <- function(med_df, col = "med_dose_unit") {
  query <- sprintf("
        SELECT *,
            NULLIF(lower(regexp_replace(%s, '\\s+', '', 'g')), '') as _clean_unit
        FROM med_df
    ", col)
  run_unit_converter_query(query, list(med_df = med_df))
}

#' Clean dose unit name variants (vector version)
#'
#' Applies `UNIT_NAMING_VARIANTS` sequentially to convert unit name variants
#' to their standard abbreviations (e.g. `'milliliter/hour'` to `'ml/hr'`).
#' Port of clifpy's `_clean_dose_unit_names` (pandas Series version).
#'
#' @param dose_unit_values Character vector, already format-cleaned.
#' @return Character vector with clean unit names.
#' @noRd
clean_dose_unit_names <- function(dose_unit_values) {
  for (replacement in names(UNIT_NAMING_VARIANTS)) {
    dose_unit_values <- stringr::str_replace_all(
      dose_unit_values,
      UNIT_NAMING_VARIANTS[[replacement]],
      replacement
    )
  }
  dose_unit_values
}

#' Clean dose unit name variants via DuckDB
#'
#' Builds nested `regexp_replace` calls for all patterns in
#' `UNIT_NAMING_VARIANTS`, in order. Port of clifpy's
#' `_clean_dose_unit_names_duckdb`.
#'
#' @param med_df Data frame containing the column to clean.
#' @param col Name of the column containing dose unit strings.
#' @return A tibble with the column replaced by cleaned values (the cleaned
#'   column moves to the last position, as in clifpy).
#' @noRd
clean_dose_unit_names_duckdb <- function(med_df, col = "_clean_unit") {
  # Build nested regexp_replace calls for all patterns
  replacement_expression <- col
  for (replacement in names(UNIT_NAMING_VARIANTS)) {
    replacement_expression <- sprintf(
      "regexp_replace(%s, '%s', '%s', 'g')",
      replacement_expression, UNIT_NAMING_VARIANTS[[replacement]], replacement
    )
  }

  query <- sprintf("
        SELECT * EXCLUDE (%s), %s as %s
        FROM med_df
    ", col, replacement_expression, col)
  run_unit_converter_query(query, list(med_df = med_df))
}

# ---------------------------------------------------------------------------
# SQL CASE expression builders
# ---------------------------------------------------------------------------

#' Concatenate SQL CASE WHEN statements from patterns
#'
#' Port of clifpy's `_concat_builders_by_patterns`.
#'
#' @param builder Function turning a regex pattern into a `WHEN ... THEN ...` clause.
#' @param patterns Character vector of regex patterns.
#' @param else_case Value for the ELSE clause when no patterns match.
#' @return Complete SQL CASE statement.
#' @noRd
concat_builders_by_patterns <- function(builder, patterns, else_case = "1") {
  paste0(
    "CASE ",
    paste(vapply(patterns, builder, character(1)), collapse = " "),
    sprintf(" ELSE %s END", else_case)
  )
}

#' Build a WHEN clause converting a clean unit to base units
#'
#' Port of clifpy's `_pattern_to_factor_builder_for_base`.
#'
#' @param pattern Regex pattern; must exist in `REGEX_TO_FACTOR_MAPPER`.
#' @return SQL `WHEN ... THEN ...` clause string.
#' @noRd
pattern_to_factor_builder_for_base <- function(pattern) {
  if (pattern %in% names(REGEX_TO_FACTOR_MAPPER)) {
    return(sprintf(
      "WHEN regexp_matches(_clean_unit, '%s') THEN %s",
      pattern, REGEX_TO_FACTOR_MAPPER[[pattern]]
    ))
  }
  cli::cli_abort("regex pattern {pattern} not found in REGEX_TO_FACTOR_MAPPER")
}

#' Build a WHEN clause converting base units to a preferred unit
#'
#' Applies the inverse of the factor used by
#' `pattern_to_factor_builder_for_base()`. Port of clifpy's
#' `_pattern_to_factor_builder_for_preferred`.
#'
#' @param pattern Regex pattern; must exist in `REGEX_TO_FACTOR_MAPPER`.
#' @return SQL `WHEN ... THEN 1/(...)` clause string.
#' @noRd
pattern_to_factor_builder_for_preferred <- function(pattern) {
  if (pattern %in% names(REGEX_TO_FACTOR_MAPPER)) {
    return(sprintf(
      "WHEN regexp_matches(_preferred_unit, '%s') THEN 1/(%s)",
      pattern, REGEX_TO_FACTOR_MAPPER[[pattern]]
    ))
  }
  cli::cli_abort("regex pattern {pattern} not found in REGEX_TO_FACTOR_MAPPER")
}

# ---------------------------------------------------------------------------
# Conversion stages
# ---------------------------------------------------------------------------

#' Convert clean dose units to base units
#'
#' Core first-stage conversion: transforms cleaned dose units into a base set
#' of standard units (mcg/min, ml/min, u/min for rates; mcg, ml, u for
#' amounts). Port of clifpy's `_convert_clean_units_to_base_units`; the SQL is
#' ported verbatim.
#'
#' @param med_df Data frame with `_clean_unit`, `med_dose` and `weight_kg` columns.
#' @return A tibble with added columns `_unit_class`, `_weighted`,
#'   `_amount_multiplier`, `_time_multiplier`, `_weight_multiplier`,
#'   `_base_dose`, `_base_unit`.
#' @noRd
convert_clean_units_to_base_units <- function(med_df) {
  amount_clause <- concat_builders_by_patterns(
    builder = pattern_to_factor_builder_for_base,
    patterns = c(L_REGEX, MU_REGEX, MG_REGEX, NG_REGEX, G_REGEX),
    else_case = "1"
  )

  time_clause <- concat_builders_by_patterns(
    builder = pattern_to_factor_builder_for_base,
    patterns = c(HR_REGEX),
    else_case = "1"
  )

  weight_clause <- concat_builders_by_patterns(
    builder = pattern_to_factor_builder_for_base,
    patterns = c(KG_REGEX, LB_REGEX),
    else_case = "1"
  )

  query <- sprintf("
    SELECT *
        -- classify and check acceptability first
        , _unit_class: CASE
            WHEN _clean_unit IN ('%s') THEN 'rate'
            WHEN _clean_unit IN ('%s') THEN 'amount'
            ELSE 'unrecognized' END
        -- mark if the input unit is adjusted by weight (e.g. 'mcg/kg/hr')
        , _weighted: CASE
            WHEN regexp_matches(_clean_unit, '%s') THEN 1 ELSE 0 END
        -- parse and generate multipliers
        , _amount_multiplier: CASE
            WHEN _unit_class = 'unrecognized' THEN 1 ELSE (%s) END
        , _time_multiplier: CASE
            WHEN _unit_class = 'unrecognized' THEN 1 ELSE (%s) END
        , _weight_multiplier: CASE
            WHEN _unit_class = 'unrecognized' THEN 1 ELSE (%s) END
        -- calculate the base dose
        , _base_dose: CASE
            -- when the input unit is weighted but weight_kg is missing, keep the original dose
            WHEN _weighted = 1 AND weight_kg IS NULL THEN med_dose
            ELSE med_dose * _amount_multiplier * _time_multiplier * _weight_multiplier
            END
        -- id the base unit
        , _base_unit: CASE
            -- when the input unit is weighted but weight_kg is missing, keep the original dose
            WHEN _weighted = 1 AND weight_kg IS NULL THEN _clean_unit
            WHEN _unit_class = 'unrecognized' THEN _clean_unit
            WHEN _unit_class = 'rate' AND regexp_matches(_clean_unit, '%s') THEN 'mcg/min'
            WHEN _unit_class = 'rate' AND regexp_matches(_clean_unit, '%s') THEN 'ml/min'
            WHEN _unit_class = 'rate' AND regexp_matches(_clean_unit, '%s') THEN 'u/min'
            WHEN _unit_class = 'amount' AND regexp_matches(_clean_unit, '%s') THEN 'mcg'
            WHEN _unit_class = 'amount' AND regexp_matches(_clean_unit, '%s') THEN 'ml'
            WHEN _unit_class = 'amount' AND regexp_matches(_clean_unit, '%s') THEN 'u'
            END
    FROM med_df
  ",
    RATE_UNITS_STR, AMOUNT_UNITS_STR,
    WEIGHT_REGEX,
    amount_clause, time_clause, weight_clause,
    MASS_REGEX, VOLUME_REGEX, UNIT_REGEX,
    MASS_REGEX, VOLUME_REGEX, UNIT_REGEX
  )
  run_unit_converter_query(query, list(med_df = med_df))
}

#' Create a summary table of unit conversion counts
#'
#' Port of clifpy's `_create_unit_conversion_counts_table`.
#'
#' @param med_df Data frame produced by the conversion pipeline.
#' @param group_by Character vector of columns to group by.
#' @return A tibble with the grouping columns and a `count` column.
#' @noRd
create_unit_conversion_counts_table <- function(med_df, group_by) {
  # check presence of all the group by columns
  missing_columns <- setdiff(group_by, names(med_df))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The following column(s) are required but not found: {.val {missing_columns}}"
    )
  }

  # build the string that enumerates the group by columns
  # e.g. 'med_dose_unit, med_dose_unit_normalized, unit_class'
  columns_enumeration <- paste(group_by, collapse = ", ")
  order_by_clause <- if ("med_category" %in% group_by) "med_category, count DESC" else "count DESC"

  query <- sprintf("
    SELECT %s
        , COUNT(*) as count
    FROM med_df
    GROUP BY %s
    ORDER BY %s
  ", columns_enumeration, columns_enumeration, order_by_clause)
  run_unit_converter_query(query, list(med_df = med_df))
}

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Find the most recent weight for each medication administration
#'
#' ASOF-joins the latest `weight_kg` vital recorded at or before each
#' medication `admin_dttm`. Port of clifpy's `find_most_recent_weight`; the
#' DuckDB SQL is ported verbatim.
#'
#' @param med_df Medication data frame with `hospitalization_id`, `admin_dttm`
#'   and `med_category` columns.
#' @param vitals_df Vitals data frame with `hospitalization_id`,
#'   `recorded_dttm`, `vital_category` and `vital_value` columns; rows with
#'   `vital_category == 'weight_kg'` supply the weights.
#'
#' @return A tibble with all columns of `med_df` plus `weight_kg` and
#'   `_weight_recorded_dttm`, ordered by `hospitalization_id`, `admin_dttm`,
#'   `med_category`.
#' @export
#'
#' @examples
#' \dontrun{
#' medications_with_weight <- find_most_recent_weight(medication_data, vitals_data)
#' }
find_most_recent_weight <- function(med_df, vitals_df) {
  query <- "
    with weights as (
        SELECT hospitalization_id, recorded_dttm, vital_value
        FROM vitals_df
        WHERE vital_category = 'weight_kg' AND vital_value IS NOT NULL
    )
    SELECT m.*
        , v.vital_value as weight_kg
        , v.recorded_dttm as _weight_recorded_dttm
    FROM med_df m
    ASOF LEFT JOIN weights v
        ON m.hospitalization_id = v.hospitalization_id
        AND v.recorded_dttm <= m.admin_dttm
    ORDER BY m.hospitalization_id, m.admin_dttm, m.med_category
  "
  run_unit_converter_query(query, list(med_df = med_df, vitals_df = vitals_df))
}

#' Standardize medication dose units to a base set of standard units
#'
#' Complete first-stage standardization pipeline: format cleaning, name
#' cleaning and conversion to base units (mcg/min, ml/min, u/min for rates;
#' mcg, ml, u for amounts). Port of clifpy's `standardize_dose_to_base_units`.
#'
#' If `med_df` has no `weight_kg` column, the most recent weight for each
#' administration is pulled from `vitals_df` via [find_most_recent_weight()].
#' Weight-based dosing (`/kg`, `/lb`), time conversions (`/hr` to `/min`),
#' volume (l to ml), mass (mg, ng, g to mcg) and milli-unit conversions are
#' handled automatically; unrecognized units are flagged but preserved.
#'
#' @param med_df Medication data frame with `med_dose_unit` and `med_dose`
#'   columns (and `weight_kg`, unless `vitals_df` is supplied).
#' @param vitals_df Optional vitals data frame used to derive `weight_kg`
#'   when it is missing from `med_df`.
#'
#' @return A named list (clifpy returns a tuple of two DuckDB relations) with
#'   elements:
#' \describe{
#'   \item{base}{tibble of `med_df` with added columns `_clean_unit`,
#'     `_unit_class` ('rate', 'amount' or 'unrecognized'), `_weighted`,
#'     `_amount_multiplier`, `_time_multiplier`, `_weight_multiplier`,
#'     `_base_dose` and `_base_unit`.}
#'   \item{counts}{tibble summarizing conversion patterns, grouped by
#'     `med_dose_unit`, `_clean_unit`, `_base_unit` and `_unit_class`, with a
#'     `count` column.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' standardized <- standardize_dose_to_base_units(medication_data, vitals_data)
#' standardized$base
#' standardized$counts
#' }
standardize_dose_to_base_units <- function(med_df, vitals_df = NULL) {
  if (!"weight_kg" %in% names(med_df)) {
    if (is.null(vitals_df)) {
      cli::cli_abort(
        "med_df has no {.field weight_kg} column and no {.arg vitals_df} was supplied to derive it from."
      )
    }
    cli::cli_alert_info(
      "pulling the most recent weight from the vitals table since no `weight_kg` column exists in the medication table"
    )
    med_df <- find_most_recent_weight(med_df, vitals_df)
  }

  # check if the required columns are present
  required_columns <- c("med_dose_unit", "med_dose", "weight_kg")
  missing_columns <- setdiff(required_columns, names(med_df))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The following column(s) are required but not found: {.val {missing_columns}}"
    )
  }

  # Clean dose units using DuckDB, matching clifpy's pipeline order
  med_df_cleaned <- clean_dose_unit_formats_duckdb(med_df)
  med_df_cleaned <- clean_dose_unit_names_duckdb(med_df_cleaned)
  med_df_base <- convert_clean_units_to_base_units(med_df_cleaned)
  convert_counts_df <- create_unit_conversion_counts_table(
    med_df_base,
    group_by = c("med_dose_unit", "_clean_unit", "_base_unit", "_unit_class")
  )

  list(base = med_df_base, counts = convert_counts_df)
}

#' Convert base standardized units to user-preferred units
#'
#' Second-stage conversion from base units (mcg/min, ml/min, u/min) to
#' medication-specific preferred units, enforcing unit class and subclass
#' consistency. Port of clifpy's `_convert_base_units_to_preferred_units`; the
#' DuckDB SQL is ported verbatim.
#'
#' @param med_df Data frame with `_base_dose` and `_preferred_unit` columns
#'   (plus `weight_kg`, `_base_unit`, `_clean_unit` from the first stage).
#' @param override If `TRUE`, warns instead of erroring on preferred units
#'   that are not in `ALL_ACCEPTABLE_UNITS`.
#' @return A tibble with added classification, status and conversion columns,
#'   including `med_dose_converted` and `med_dose_unit_converted`.
#' @noRd
convert_base_units_to_preferred_units <- function(med_df, override = FALSE) {
  # check presence of all required columns
  required_columns <- c("_base_dose", "_preferred_unit")
  missing_columns <- setdiff(required_columns, names(med_df))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The following column(s) are required but not found: {.val {missing_columns}}"
    )
  }

  # check user-defined _preferred_unit values are in the set of acceptable units.
  # Missing values are excluded, matching clifpy's `- {None}`: a NULL preferred
  # unit means the row had no recognizable original unit, and is reported through
  # _convert_status ('original unit is missing') rather than raised as an error.
  all_preferred_units <- unique(med_df[["_preferred_unit"]])
  unacceptable_preferred_units <- setdiff(
    all_preferred_units[!is.na(all_preferred_units)],
    ALL_ACCEPTABLE_UNITS
  )
  if (length(unacceptable_preferred_units) > 0) {
    error_message <- paste0(
      "Cannot accommodate the conversion to the following preferred units: {",
      paste0("'", unacceptable_preferred_units, "'", collapse = ", "),
      "}. Consult the function documentation for a list of acceptable units."
    )
    if (override) {
      cli::cli_warn("{error_message}")
    } else {
      cli::cli_abort("{error_message}")
    }
  }

  amount_clause <- concat_builders_by_patterns(
    builder = pattern_to_factor_builder_for_preferred,
    patterns = c(L_REGEX, MU_REGEX, MG_REGEX, NG_REGEX, G_REGEX),
    else_case = "1"
  )

  time_clause <- concat_builders_by_patterns(
    builder = pattern_to_factor_builder_for_preferred,
    patterns = c(HR_REGEX),
    else_case = "1"
  )

  weight_clause <- concat_builders_by_patterns(
    builder = pattern_to_factor_builder_for_preferred,
    patterns = c(KG_REGEX, LB_REGEX),
    else_case = "1"
  )

  unit_class_clause <- if (!"_unit_class" %in% names(med_df)) {
    sprintf("
    , _unit_class: CASE
        WHEN _base_unit IN ('%s') THEN 'rate'
        WHEN _base_unit IN ('%s') THEN 'amount'
        ELSE 'unrecognized' END
    ", RATE_UNITS_STR, AMOUNT_UNITS_STR)
  } else {
    ""
  }

  # NOTE: clifpy also builds a `_weighted` clause here but never interpolates
  # it into the query (dead code upstream), so it is intentionally omitted.

  dose_converted_name <- if ("med_dose" %in% names(med_df)) "med_dose" else "_base_dose"
  unit_converted_name <- if ("_clean_unit" %in% names(med_df)) "_clean_unit" else "_base_unit"

  query <- sprintf("
    SELECT l.*
        %s
        , _unit_subclass: CASE
            WHEN regexp_matches(_base_unit, '%s') THEN 'mass'
            WHEN regexp_matches(_base_unit, '%s') THEN 'volume'
            WHEN regexp_matches(_base_unit, '%s') THEN 'unit'
            ELSE 'unrecognized' END
        , _unit_class_preferred: CASE
            WHEN _preferred_unit IN ('%s') THEN 'rate'
            WHEN _preferred_unit IN ('%s') THEN 'amount'
            ELSE 'unrecognized' END
        , _unit_subclass_preferred: CASE
            WHEN regexp_matches(_preferred_unit, '%s') THEN 'mass'
            WHEN regexp_matches(_preferred_unit, '%s') THEN 'volume'
            WHEN regexp_matches(_preferred_unit, '%s') THEN 'unit'
            ELSE 'unrecognized' END
        , _weighted_preferred: CASE
            WHEN regexp_matches(_preferred_unit, '%s') THEN 1 ELSE 0 END
        , _convert_status: CASE
            WHEN _weighted_preferred = 1 AND weight_kg IS NULL
                THEN 'cannot convert to a weighted unit if weight_kg is missing'
            WHEN _base_unit IS NULL THEN 'original unit is missing'
            WHEN _unit_class == 'unrecognized' OR _unit_subclass == 'unrecognized'
                THEN 'original unit ' || _base_unit || ' is not recognized'
            WHEN _unit_class_preferred == 'unrecognized' OR _unit_subclass_preferred == 'unrecognized'
                THEN 'user-preferred unit ' || _preferred_unit || ' is not recognized'
            WHEN _unit_class != _unit_class_preferred
                THEN 'cannot convert ' || _unit_class || ' to ' || _unit_class_preferred
            WHEN _unit_subclass != _unit_subclass_preferred
                THEN 'cannot convert ' || _unit_subclass || ' to ' || _unit_subclass_preferred
            WHEN _unit_class == _unit_class_preferred AND _unit_subclass == _unit_subclass_preferred
                THEN 'success'
            ELSE 'other error - please report'
            END
        , _amount_multiplier_preferred: %s
        , _time_multiplier_preferred: %s
        , _weight_multiplier_preferred: %s
        -- fall back to the base units and dose (i.e. the input) if conversion cannot be accommodated
        , med_dose_converted: CASE
            WHEN _convert_status == 'success' THEN _base_dose * _amount_multiplier_preferred * _time_multiplier_preferred * _weight_multiplier_preferred
            ELSE %s
            END
        , med_dose_unit_converted: CASE
            WHEN _convert_status == 'success' THEN _preferred_unit
            ELSE %s
            END
    FROM med_df l
  ",
    unit_class_clause,
    MASS_REGEX, VOLUME_REGEX, UNIT_REGEX,
    RATE_UNITS_STR, AMOUNT_UNITS_STR,
    MASS_REGEX, VOLUME_REGEX, UNIT_REGEX,
    WEIGHT_REGEX,
    amount_clause, time_clause, weight_clause,
    dose_converted_name,
    unit_converted_name
  )
  run_unit_converter_query(query, list(med_df = med_df))
}

#' Convert medication dose units to preferred units by medication category
#'
#' Two-step conversion, ported from clifpy's
#' `convert_dose_units_by_med_category`:
#'
#' 1. Standardize all dose units to a base set of standard units
#'    (mcg/min, ml/min, u/min for rates; mcg, ml, u for amounts).
#' 2. Convert from base units to each medication category's preferred unit,
#'    maintaining unit class consistency (rates stay rates, amounts stay
#'    amounts) and using patient weight for weight-based dosing.
#'
#' @param med_df Medication data frame with `med_category`, `med_dose` and
#'   `med_dose_unit` columns (and `weight_kg`, unless `vitals_df` is supplied).
#' @param vitals_df Optional vitals data frame used to derive `weight_kg`
#'   when it is missing from `med_df` (see [find_most_recent_weight()]).
#' @param preferred_units Named list or character vector mapping medication
#'   categories to preferred units, e.g.
#'   `list(propofol = "mcg/kg/min", fentanyl = "mcg/hr", insulin = "u/hr")`.
#'   Categories without an entry fall back to their base units.
#' @param show_intermediate If `TRUE`, keeps all intermediate calculation
#'   columns (multipliers and QA columns) in the output.
#' @param override If `TRUE`, warns instead of erroring on unacceptable
#'   preferred units or preferred-unit categories missing from `med_df`.
#'
#' @return A named list (clifpy returns a tuple) with elements:
#' \describe{
#'   \item{converted}{tibble of `med_df` with added columns including
#'     `_clean_unit`, `_unit_class`, `_convert_status`, `med_dose_converted`
#'     and `med_dose_unit_converted` (plus intermediate multiplier and QA
#'     columns when `show_intermediate = TRUE`).}
#'   \item{counts}{tibble of conversion counts grouped by `med_category`,
#'     original/clean/base/preferred units and conversion status.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' conversion_result <- convert_dose_units_by_med_category(
#'   medication_data,
#'   vitals_df = vitals_data,
#'   preferred_units = list(
#'     propofol = "mcg/kg/min",
#'     fentanyl = "mcg/hr",
#'     insulin = "u/hr"
#'   )
#' )
#' conversion_result$converted
#' conversion_result$counts
#' }
convert_dose_units_by_med_category <- function(med_df,
                                               vitals_df = NULL,
                                               preferred_units = NULL,
                                               show_intermediate = FALSE,
                                               override = FALSE) {
  # check if the requested med_categories are in the input med_df
  requested_med_categories <- names(preferred_units) %||% character(0)
  extra_med_categories <- setdiff(requested_med_categories, unique(med_df[["med_category"]]))
  if (length(extra_med_categories) > 0) {
    error_message <- paste0(
      "The following med_categories are given a preferred unit but not found in the input med_df: {",
      paste0("'", extra_med_categories, "'", collapse = ", "),
      "}"
    )
    if (override) {
      cli::cli_warn("{error_message}")
    } else {
      cli::cli_abort("{error_message}")
    }
  }

  standardized <- tryCatch(
    standardize_dose_to_base_units(med_df, vitals_df),
    error = function(condition) {
      standardization_error <- conditionMessage(condition)
      cli::cli_abort("Error standardizing dose units to base units: {standardization_error}")
    }
  )
  med_df_base <- standardized$base

  # join the preferred units to the df
  preferred_units_df <- dplyr::tibble(
    med_category = requested_med_categories,
    "_preferred_unit" = as.character(unlist(preferred_units, use.names = FALSE) %||% character(0))
  )
  join_query <- "
    SELECT l.*
        -- for unspecified preferred units, use the base units by default
        , _preferred_unit: COALESCE(r._preferred_unit, l._base_unit)
    FROM med_df_base l
    LEFT JOIN preferred_units_df r USING (med_category)
  "
  med_df_preferred <- run_unit_converter_query(
    join_query,
    list(med_df_base = med_df_base, preferred_units_df = preferred_units_df)
  )

  med_df_converted <- tryCatch(
    convert_base_units_to_preferred_units(med_df_preferred, override = override),
    error = function(condition) {
      conversion_error <- conditionMessage(condition)
      cli::cli_abort("Error converting dose units to preferred units: {conversion_error}")
    }
  )

  convert_counts_df <- tryCatch(
    create_unit_conversion_counts_table(
      med_df_converted,
      group_by = c(
        "med_category",
        "med_dose_unit", "_clean_unit", "_base_unit", "_unit_class",
        "_preferred_unit", "med_dose_unit_converted", "_convert_status"
      )
    ),
    error = function(condition) {
      counts_error <- conditionMessage(condition)
      cli::cli_abort("Error creating unit conversion counts table: {counts_error}")
    }
  )

  if (show_intermediate) {
    return(list(converted = med_df_converted, counts = convert_counts_df))
  }

  # the default (show_intermediate = FALSE) is to drop multiplier columns which
  # likely are not useful for the user
  multiplier_columns <- grep("multiplier", names(med_df_converted), value = TRUE)
  qa_columns <- c(
    "_weight_recorded_dttm",
    "_weighted", "_weighted_preferred",
    "_base_dose", "_base_unit",
    "_preferred_unit",
    "_unit_class_preferred",
    "_unit_subclass", "_unit_subclass_preferred"
  )
  columns_to_drop <- intersect(c(multiplier_columns, qa_columns), names(med_df_converted))

  list(
    converted = med_df_converted[, setdiff(names(med_df_converted), columns_to_drop), drop = FALSE],
    counts = convert_counts_df
  )
}
