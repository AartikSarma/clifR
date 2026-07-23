#' MDRO (multi-drug resistant organism) flag calculation
#'
#' Port of `clifpy/utils/mdro_flags.py`. Derives MDR, XDR, PDR and DTR flags for
#' a named organism from antimicrobial susceptibility testing results, using the
#' antimicrobial groupings and resistance definitions in `inst/extdata/mdro.yaml`.
#'
#' @name clif-mdro-flags
NULL

# Susceptibility values ranked most to least resistant; ties in the wide pivot
# resolve to the most resistant result.
MDRO_SUSCEPTIBILITY_PRIORITY <- c(
  non_susceptible = 1L,
  intermediate = 2L,
  susceptible = 3L,
  `NA` = 4L
)

MDRO_UNKNOWN_SUSCEPTIBILITY_PRIORITY <- 5L

#' Load the MDRO configuration
#'
#' @param config_path Optional path to an `mdro.yaml` file. Defaults to the
#'   copy bundled with clifR.
#' @return The parsed configuration as a named list.
#' @keywords internal
load_mdro_config <- function(config_path = NULL) {
  if (is.null(config_path)) {
    config_path <- clif_extdata_path("mdro.yaml")
  }
  if (!nzchar(config_path) || !file.exists(config_path)) {
    cli::cli_abort("MDRO configuration file not found: {.file {config_path}}")
  }
  yaml::read_yaml(config_path)
}

#' Extract the data frame from a clifR table object
#'
#' Accepts either an R6 CLIF table or a plain data frame, so the MDRO helpers can
#' be used with in-memory data as well as loaded tables.
#'
#' @param table_object An R6 table object or a data frame.
#' @param argument_name Name used in error messages.
#' @return A tibble.
#' @keywords internal
extract_table_dataframe <- function(table_object, argument_name) {
  table_data <- if (inherits(table_object, "R6")) table_object$df else table_object
  if (is.null(table_data) || !is.data.frame(table_data)) {
    cli::cli_abort("{.arg {argument_name}} must be a CLIF table object with data, or a data frame.")
  }
  dplyr::as_tibble(table_data)
}

#' Rank a susceptibility value for duplicate handling
#'
#' Lower is more resistant: `non_susceptible` (1), `intermediate` (2),
#' `susceptible` (3), `"NA"` (4), anything else (5).
#'
#' @param susceptibility_values Character vector of susceptibility categories.
#' @return An integer vector of priority ranks.
#' @keywords internal
prioritize_susceptibility <- function(susceptibility_values) {
  matched_positions <- match(susceptibility_values, names(MDRO_SUSCEPTIBILITY_PRIORITY))
  ranks <- unname(MDRO_SUSCEPTIBILITY_PRIORITY[matched_positions])
  ranks[is.na(matched_positions)] <- MDRO_UNKNOWN_SUSCEPTIBILITY_PRIORITY
  as.integer(ranks)
}

#' Filter culture rows to a cohort's date windows
#'
#' Keeps only culture results whose timestamp falls inside the matching
#' hospitalization's `[start_dttm, end_dttm]` window. The timestamp column is the
#' first of `result_dttm`, `collect_dttm` or `culture_dttm` present in the data.
#'
#' @param culture_df Culture table data.
#' @param cohort Data frame with `hospitalization_id`, `start_dttm`, `end_dttm`.
#' @return The filtered culture data, without the cohort window columns.
#' @keywords internal
apply_cohort_filter_to_culture <- function(culture_df, cohort) {
  candidate_datetime_columns <- c("result_dttm", "collect_dttm", "culture_dttm")
  culture_datetime_column <- candidate_datetime_columns[
    candidate_datetime_columns %in% names(culture_df)
  ][1]

  if (is.na(culture_datetime_column)) {
    cli::cli_abort(c(
      "Cannot apply cohort filtering: no datetime column found in culture table.",
      "i" = "Expected one of: {.field {candidate_datetime_columns}}"
    ))
  }

  cohort_windows <- dplyr::as_tibble(cohort) |>
    dplyr::select(dplyr::all_of(c("hospitalization_id", "start_dttm", "end_dttm")))

  culture_df |>
    dplyr::inner_join(cohort_windows, by = "hospitalization_id") |>
    dplyr::filter(
      .data[[culture_datetime_column]] >= .data$start_dttm,
      .data[[culture_datetime_column]] <= .data$end_dttm
    ) |>
    dplyr::select(-dplyr::all_of(c("start_dttm", "end_dttm")))
}

#' Warn about antimicrobials defined in the config but absent from the data
#'
#' Agents required by a `specific_agents_resistant` criterion (DTR) are called
#' out separately, because a missing required agent silently suppresses that
#' flag.
#'
#' @param merged_df Merged culture and susceptibility data.
#' @param organism_config Organism section of the MDRO configuration.
#' @param organism_name Organism being analysed, for the message text.
#' @return `NULL`, invisibly.
#' @keywords internal
check_missing_antimicrobials <- function(merged_df, organism_config, organism_name) {
  antimicrobial_groups <- organism_config$antimicrobial_groups
  all_defined_antimicrobials <- unique(unlist(antimicrobial_groups, use.names = FALSE))
  tested_antimicrobials <- unique(
    merged_df$antimicrobial_category[!is.na(merged_df$antimicrobial_category)]
  )
  missing_antimicrobials <- setdiff(all_defined_antimicrobials, tested_antimicrobials)

  if (length(missing_antimicrobials) == 0) {
    return(invisible(NULL))
  }

  resistance_definitions <- organism_config$resistance_definitions %||% list()
  critical_missing <- character(0)
  for (flag_definition in resistance_definitions) {
    criteria <- flag_definition$criteria %||% list()
    if (identical(criteria$type, "specific_agents_resistant")) {
      required_agents <- unlist(criteria$required_agents %||% list(), use.names = FALSE)
      critical_missing <- union(critical_missing, intersect(required_agents, missing_antimicrobials))
    }
  }

  if (length(critical_missing) > 0) {
    cli::cli_alert_danger(
      "CRITICAL WARNING: missing required antimicrobials for {.val {organism_name}}"
    )
    for (agent in sort(critical_missing)) {
      flags_requiring <- character(0)
      for (flag_name in names(resistance_definitions)) {
        flag_definition <- resistance_definitions[[flag_name]]
        criteria <- flag_definition$criteria %||% list()
        if (identical(criteria$type, "specific_agents_resistant")) {
          required_agents <- unlist(criteria$required_agents %||% list(), use.names = FALSE)
          if (agent %in% required_agents) {
            flags_requiring <- c(flags_requiring, flag_definition$name %||% toupper(flag_name))
          }
        }
      }
      if (length(flags_requiring) > 0) {
        cli::cli_bullets(c("*" = "{agent} — required for: {paste(flags_requiring, collapse = ', ')}"))
      } else {
        cli::cli_bullets(c("*" = "{agent}"))
      }
    }
    cli::cli_alert_info(
      "Organisms missing these agents will NOT be flagged for the resistance categories listed above, even if otherwise resistant."
    )
  }

  non_critical_missing <- setdiff(missing_antimicrobials, critical_missing)
  if (length(non_critical_missing) > 0) {
    cli::cli_alert_info("Missing antimicrobials from the {.val {organism_name}} dataset:")
    for (agent in sort(non_critical_missing)) {
      group_name <- NA_character_
      for (candidate_group in names(antimicrobial_groups)) {
        if (agent %in% unlist(antimicrobial_groups[[candidate_group]], use.names = FALSE)) {
          group_name <- candidate_group
          break
        }
      }
      cli::cli_bullets(c("*" = "{agent} (group: {group_name})"))
    }
  }

  invisible(NULL)
}

#' Calculate MDRO flags for a single organism culture
#'
#' @param group_data Susceptibility data for one `(hospitalization_id,
#'   organism_id)` pair, with `antimicrobial_category`, `antimicrobial_group` and
#'   `is_resistant` columns.
#' @param resistance_definitions Resistance definitions from the configuration.
#' @param antimicrobial_groups Antimicrobial group definitions.
#' @return A named list of flag column names to `0`/`1`.
#' @keywords internal
calculate_flags_for_organism <- function(group_data,
                                         resistance_definitions,
                                         antimicrobial_groups) {
  resistant_data <- group_data[group_data$is_resistant, , drop = FALSE]

  resistant_groups <- unique(
    resistant_data$antimicrobial_group[!is.na(resistant_data$antimicrobial_group)]
  )
  num_resistant_groups <- length(resistant_groups)

  tested_agents <- unique(group_data$antimicrobial_category)
  resistant_agents <- unique(resistant_data$antimicrobial_category)

  flags <- list()
  for (flag_name in names(resistance_definitions)) {
    flag_definition <- resistance_definitions[[flag_name]]
    criteria <- flag_definition$criteria
    criteria_type <- criteria$type
    column_name <- flag_definition$column_name

    flags[[column_name]] <- switch(criteria_type,
      # MDR: resistant in at least `min_groups` antimicrobial groups.
      min_groups_resistant = as.integer(num_resistant_groups >= criteria$min_groups),

      # XDR: resistant in all but at most `max_groups_susceptible` of the groups
      # DEFINED in the config, not just the groups that happened to be tested.
      max_groups_susceptible = {
        minimum_resistant_groups <- length(antimicrobial_groups) - criteria$max_groups_susceptible
        as.integer(num_resistant_groups >= minimum_resistant_groups)
      },

      # PDR: every defined agent was tested, and every one is resistant.
      all_tested_resistant = {
        all_defined_agents <- unique(unlist(antimicrobial_groups, use.names = FALSE))
        all_defined_tested <- all(all_defined_agents %in% tested_agents)
        all_defined_resistant <- all(all_defined_agents %in% resistant_agents)
        as.integer(all_defined_tested && all_defined_resistant)
      },

      # DTR: every required agent was tested, and every one is resistant.
      specific_agents_resistant = {
        required_agents <- unlist(criteria$required_agents, use.names = FALSE)
        all_required_tested <- all(required_agents %in% tested_agents)
        all_required_resistant <- all(required_agents %in% resistant_agents)
        as.integer(all_required_tested && all_required_resistant)
      },

      cli::cli_abort("Unknown MDRO criteria type: {.val {criteria_type}}")
    )
  }

  flags
}

#' Pivot susceptibility results into one column per antimicrobial
#'
#' Duplicate tests of the same agent collapse to the most resistant result.
#' Agents that appear as a column but were not tested for a given organism are
#' filled with `"not_tested"`.
#'
#' @param merged_df Merged culture and susceptibility data.
#' @return A tibble keyed by `hospitalization_id` and `organism_id`, with one
#'   `<antimicrobial>_agent` column per tested antimicrobial.
#' @keywords internal
pivot_susceptibility_data <- function(merged_df) {
  merged_df |>
    dplyr::mutate(susceptibility_priority = prioritize_susceptibility(.data$susceptibility_category)) |>
    dplyr::arrange(.data$susceptibility_priority) |>
    dplyr::distinct(
      .data$hospitalization_id, .data$organism_id, .data$antimicrobial_category,
      .keep_all = TRUE
    ) |>
    dplyr::mutate(agent_column = paste0(.data$antimicrobial_category, "_agent")) |>
    dplyr::select(dplyr::all_of(c(
      "hospitalization_id", "organism_id", "agent_column", "susceptibility_category"
    ))) |>
    tidyr::pivot_wider(
      names_from = "agent_column",
      values_from = "susceptibility_category",
      names_sort = TRUE,
      values_fill = "not_tested"
    ) |>
    # pandas' pivot_table drops rows whose value is NaN, which then surface as
    # "not_tested" in the fill step; reproduce that for untyped susceptibilities.
    dplyr::mutate(dplyr::across(
      dplyr::ends_with("_agent"),
      ~ dplyr::coalesce(.x, "not_tested")
    )) |>
    dplyr::arrange(.data$hospitalization_id, .data$organism_id)
}

#' Create binary resistance columns per antimicrobial group
#'
#' A group column is `1` when any agent in that group is resistant for the
#' organism, and `0` when every tested agent in the group is susceptible or the
#' group was not tested at all.
#'
#' @param merged_df Merged culture and susceptibility data with an
#'   `antimicrobial_group` column.
#' @param resistant_categories Susceptibility categories counted as resistant.
#' @return A tibble keyed by `hospitalization_id` and `organism_id` with one
#'   `<group>_group` column per observed group.
#' @keywords internal
create_group_columns <- function(merged_df, resistant_categories) {
  merged_df |>
    dplyr::mutate(is_resistant = .data$susceptibility_category %in% resistant_categories) |>
    dplyr::group_by(.data$hospitalization_id, .data$organism_id, .data$antimicrobial_group) |>
    dplyr::summarise(is_resistant = any(.data$is_resistant), .groups = "drop") |>
    dplyr::mutate(group_column = paste0(.data$antimicrobial_group, "_group")) |>
    dplyr::select(dplyr::all_of(c(
      "hospitalization_id", "organism_id", "group_column", "is_resistant"
    ))) |>
    tidyr::pivot_wider(
      names_from = "group_column",
      values_from = "is_resistant",
      names_sort = TRUE,
      values_fill = FALSE
    ) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_group"), ~ as.integer(.x))) |>
    dplyr::arrange(.data$hospitalization_id, .data$organism_id)
}

#' Empty MDRO result frame
#'
#' @return A zero-row tibble with `hospitalization_id` and `organism_id`.
#' @keywords internal
empty_mdro_result <- function() {
  dplyr::tibble(
    hospitalization_id = character(0),
    organism_id = character(0)
  )
}

#' Calculate MDRO flags for an organism
#'
#' Analyses antimicrobial susceptibility results to determine whether each
#' organism culture meets the multi-drug resistance criteria defined for that
#' organism in `mdro.yaml`.
#'
#' @param culture A `MicrobiologyCulture` table object (or data frame) with
#'   `organism_id`, `hospitalization_id` and `organism_category` columns.
#' @param susceptibility A `MicrobiologySusceptibility` table object (or data
#'   frame) with `organism_id`, `antimicrobial_category` and
#'   `susceptibility_category` columns.
#' @param organism_name Organism to calculate flags for; must match an
#'   `organism_category` value and a key in the configuration, e.g.
#'   `"pseudomonas_aeruginosa"`.
#' @param cohort Optional data frame with `hospitalization_id`, `start_dttm` and
#'   `end_dttm`. When supplied, culture results outside each window are dropped.
#' @param hospitalization_ids Optional character vector restricting the analysis
#'   to specific hospitalizations.
#' @param config_path Optional path to an `mdro.yaml` file. Defaults to the copy
#'   bundled with clifR.
#'
#' @return A wide tibble with one row per `(hospitalization_id, organism_id)`:
#'   * `hospitalization_id`, `organism_id` — identifiers.
#'   * `<antimicrobial>_agent` — the susceptibility result for each agent, or
#'     `"not_tested"`.
#'   * `<group>_group` — `1` when any agent in the group is resistant, else `0`.
#'   * the resistance flag columns named by the configuration (`MDR`, `XDR`,
#'     `PDR`, `DTR` for *P. aeruginosa*), each `0` or `1`.
#'
#'   Columns are ordered identifiers, then agents, then groups, then flags, each
#'   block sorted by name.
#' @export
#'
#' @examples
#' \dontrun{
#' culture <- MicrobiologyCulture$new(data_directory = "./data", filetype = "parquet")
#' susceptibility <- MicrobiologySusceptibility$new(
#'   data_directory = "./data", filetype = "parquet"
#' )
#' mdro_flags <- calculate_mdro_flags(
#'   culture = culture,
#'   susceptibility = susceptibility,
#'   organism_name = "pseudomonas_aeruginosa"
#' )
#' }
calculate_mdro_flags <- function(culture,
                                 susceptibility,
                                 organism_name,
                                 cohort = NULL,
                                 hospitalization_ids = NULL,
                                 config_path = NULL) {
  logger <- get_logger("utils.mdro_flags")

  mdro_config <- load_mdro_config(config_path)
  if (!organism_name %in% names(mdro_config$organisms)) {
    cli::cli_abort(c(
      "Organism {.val {organism_name}} not found in configuration.",
      "i" = "Available organisms: {.val {names(mdro_config$organisms)}}"
    ))
  }
  organism_config <- mdro_config$organisms[[organism_name]]

  culture_df <- extract_table_dataframe(culture, "culture")
  susceptibility_df <- extract_table_dataframe(susceptibility, "susceptibility")

  required_culture_columns <- c("organism_id", "hospitalization_id", "organism_category")
  missing_culture_columns <- setdiff(required_culture_columns, names(culture_df))
  if (length(missing_culture_columns) > 0) {
    cli::cli_abort("Missing required columns in culture table: {.field {missing_culture_columns}}")
  }

  required_susceptibility_columns <- c(
    "organism_id", "antimicrobial_category", "susceptibility_category"
  )
  missing_susceptibility_columns <- setdiff(required_susceptibility_columns, names(susceptibility_df))
  if (length(missing_susceptibility_columns) > 0) {
    cli::cli_abort(
      "Missing required columns in susceptibility table: {.field {missing_susceptibility_columns}}"
    )
  }

  culture_filtered <- culture_df[
    !is.na(culture_df$organism_category) & culture_df$organism_category == organism_name, ,
    drop = FALSE
  ]
  logger$info(sprintf(
    "Filtered culture to %s: %d rows", organism_name, nrow(culture_filtered)
  ))
  if (nrow(culture_filtered) == 0) {
    logger$warning(sprintf("No data found for organism: %s", organism_name))
    return(empty_mdro_result())
  }

  if (!is.null(cohort)) {
    culture_filtered <- apply_cohort_filter_to_culture(culture_filtered, cohort)
    logger$info(sprintf("After cohort filtering: %d rows", nrow(culture_filtered)))
  }

  if (!is.null(hospitalization_ids)) {
    culture_filtered <- culture_filtered[
      culture_filtered$hospitalization_id %in% hospitalization_ids, ,
      drop = FALSE
    ]
    logger$info(sprintf("After hospitalization_id filtering: %d rows", nrow(culture_filtered)))
  }

  if (nrow(culture_filtered) == 0) {
    logger$warning("No data remaining after filtering")
    return(empty_mdro_result())
  }

  # LEFT JOIN preserves culture rows that have no susceptibility testing, so they
  # can be counted before being dropped.
  merged_df <- culture_filtered |>
    dplyr::select(dplyr::all_of(required_culture_columns)) |>
    dplyr::left_join(
      dplyr::select(susceptibility_df, dplyr::all_of(required_susceptibility_columns)),
      by = "organism_id",
      relationship = "many-to-many"
    )
  logger$info(sprintf("Merged culture and susceptibility data: %d rows", nrow(merged_df)))

  organisms_without_susceptibility <- dplyr::n_distinct(
    merged_df$organism_id[is.na(merged_df$antimicrobial_category)]
  )
  if (organisms_without_susceptibility > 0) {
    logger$info(sprintf(
      "%d organism(s) have no susceptibility testing data", organisms_without_susceptibility
    ))
  }

  merged_df <- merged_df[!is.na(merged_df$antimicrobial_category), , drop = FALSE]
  if (nrow(merged_df) == 0) {
    logger$warning("No organisms with susceptibility data found")
    return(empty_mdro_result())
  }

  antimicrobial_groups <- organism_config$antimicrobial_groups
  category_to_group <- character(0)
  for (group_name in names(antimicrobial_groups)) {
    for (category in unlist(antimicrobial_groups[[group_name]], use.names = FALSE)) {
      category_to_group[[category]] <- group_name
    }
  }
  merged_df$antimicrobial_group <- unname(
    category_to_group[match(merged_df$antimicrobial_category, names(category_to_group))]
  )

  # Keep only antimicrobials the configuration defines for this organism.
  num_rows_before <- nrow(merged_df)
  merged_df <- merged_df[!is.na(merged_df$antimicrobial_group), , drop = FALSE]
  logger$info(sprintf(
    "Filtered antimicrobials: %d -> %d rows (%d excluded)",
    num_rows_before, nrow(merged_df), num_rows_before - nrow(merged_df)
  ))

  resistant_categories <- unlist(
    organism_config$resistant_categories %||% list("non_susceptible", "intermediate"),
    use.names = FALSE
  )
  merged_df$is_resistant <- merged_df$susceptibility_category %in% resistant_categories

  check_missing_antimicrobials(merged_df, organism_config, organism_name)

  if (nrow(merged_df) == 0) {
    return(empty_mdro_result())
  }

  resistance_definitions <- organism_config$resistance_definitions
  organism_keys <- merged_df |>
    dplyr::distinct(.data$hospitalization_id, .data$organism_id) |>
    dplyr::arrange(.data$hospitalization_id, .data$organism_id)

  flags_df <- purrr::map2(
    organism_keys$hospitalization_id,
    organism_keys$organism_id,
    function(hospitalization_identifier, organism_identifier) {
      group_data <- merged_df[
        merged_df$hospitalization_id == hospitalization_identifier &
          merged_df$organism_id == organism_identifier, ,
        drop = FALSE
      ]
      flags <- calculate_flags_for_organism(
        group_data, resistance_definitions, antimicrobial_groups
      )
      flags$hospitalization_id <- hospitalization_identifier
      flags$organism_id <- organism_identifier
      dplyr::as_tibble(flags)
    }
  ) |>
    dplyr::bind_rows()

  antimicrobial_df <- pivot_susceptibility_data(merged_df)
  logger$info(sprintf("Created %d antimicrobial columns", ncol(antimicrobial_df) - 2L))

  group_df <- create_group_columns(merged_df, resistant_categories)
  logger$info(sprintf("Created %d antimicrobial group columns", ncol(group_df) - 2L))

  identifier_columns <- c("hospitalization_id", "organism_id")
  result_df <- flags_df |>
    dplyr::left_join(antimicrobial_df, by = identifier_columns) |>
    dplyr::left_join(group_df, by = identifier_columns)

  # Column order: identifiers, agents, groups, flags — each block sorted by name.
  # radix sort reproduces Python's byte-order `sorted()`.
  flag_columns <- sort(setdiff(names(flags_df), identifier_columns), method = "radix")
  group_columns <- sort(setdiff(names(group_df), identifier_columns), method = "radix")
  antimicrobial_columns <- sort(setdiff(names(antimicrobial_df), identifier_columns), method = "radix")

  result_df <- result_df[
    , c(identifier_columns, antimicrobial_columns, group_columns, flag_columns),
    drop = FALSE
  ]

  logger$info(sprintf("Calculated MDRO flags for %d organism cultures", nrow(result_df)))
  result_df
}
