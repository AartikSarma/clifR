#' ClifOrchestrator: unified interface across CLIF tables
#'
#' Centralized entry point for loading, validating and analysing multiple CLIF tables
#' with one shared configuration. Ported from `clifpy/clif_orchestrator.py`; method
#' names and argument defaults follow the Python original.
#'
#' @details
#' Configuration is resolved with the same priority clifpy uses: explicit arguments
#' first, then a config file, then auto-detection of `config.json` in the working
#' directory. Arguments always override config file values.
#'
#' Each CLIF table is exposed as a field of the same name, so `orchestrator$vitals`
#' is the [Vitals] object once loaded and `NULL` before that.
#'
#' @export
#' @importFrom R6 R6Class
ClifOrchestrator <- R6::R6Class(
  classname = "ClifOrchestrator",
  public = list(
    #' @field data_directory Directory containing the CLIF data files.
    data_directory = NULL,
    #' @field filetype Either `"csv"` or `"parquet"`.
    filetype = NULL,
    #' @field timezone Olson timezone used for datetime columns.
    timezone = NULL,
    #' @field clif_version CLIF schema version used when loading tables.
    clif_version = NULL,
    #' @field output_directory Directory for logs and outputs.
    output_directory = NULL,
    #' @field stitch_encounter Whether to stitch encounters after loading.
    stitch_encounter = NULL,
    #' @field stitch_time_interval Hours between discharge and next admission that
    #'   still count as one encounter.
    stitch_time_interval = NULL,
    #' @field encounter_mapping Mapping of hospitalization_id to encounter_block.
    encounter_mapping = NULL,
    #' @field wide_df Wide dataset produced by `create_wide_dataset()`.
    wide_df = NULL,
    #' @field wide_df_sofa Wide dataset built specifically for SOFA computation.
    wide_df_sofa = NULL,
    #' @field sofa_df SOFA scores produced by `compute_sofa_scores()`.
    sofa_df = NULL,

    #' @field patient Patient table object.
    patient = NULL,
    #' @field hospitalization Hospitalization table object.
    hospitalization = NULL,
    #' @field adt ADT table object.
    adt = NULL,
    #' @field labs Labs table object.
    labs = NULL,
    #' @field vitals Vitals table object.
    vitals = NULL,
    #' @field medication_admin_continuous Continuous medication administration table object.
    medication_admin_continuous = NULL,
    #' @field medication_admin_intermittent Intermittent medication administration table object.
    medication_admin_intermittent = NULL,
    #' @field patient_assessments Patient assessments table object.
    patient_assessments = NULL,
    #' @field respiratory_support Respiratory support table object.
    respiratory_support = NULL,
    #' @field position Position table object.
    position = NULL,
    #' @field hospital_diagnosis Hospital diagnosis table object.
    hospital_diagnosis = NULL,
    #' @field microbiology_culture Microbiology culture table object.
    microbiology_culture = NULL,
    #' @field crrt_therapy CRRT therapy table object.
    crrt_therapy = NULL,
    #' @field patient_procedures Patient procedures table object.
    patient_procedures = NULL,
    #' @field microbiology_susceptibility Microbiology susceptibility table object.
    microbiology_susceptibility = NULL,
    #' @field ecmo_mcs ECMO/MCS table object.
    ecmo_mcs = NULL,
    #' @field microbiology_nonculture Microbiology non-culture table object.
    microbiology_nonculture = NULL,
    #' @field code_status Code status table object.
    code_status = NULL,

    #' @description Create an orchestrator.
    #' @param config_path Optional path to a JSON or YAML config file.
    #' @param data_directory Directory containing the CLIF data files.
    #' @param filetype Either `"csv"` or `"parquet"`.
    #' @param timezone Olson timezone for datetime columns.
    #' @param output_directory Directory for logs and outputs. Defaults to
    #'   `output/` under the working directory.
    #' @param stitch_encounter Whether to stitch encounters during `initialize_tables()`.
    #' @param stitch_time_interval Hours between encounters to treat as linked.
    #'   Defaults to 6, matching clifpy.
    #' @param clif_version CLIF schema version. Defaults to [DEFAULT_CLIF_VERSION].
    #'
    #' @return A new ClifOrchestrator instance.
    initialize = function(config_path = NULL,
                          data_directory = NULL,
                          filetype = NULL,
                          timezone = NULL,
                          output_directory = NULL,
                          stitch_encounter = FALSE,
                          stitch_time_interval = 6,
                          clif_version = NULL) {
      resolved_config <- get_config_or_params(
        config_path = config_path,
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )

      self$data_directory <- resolved_config$data_directory
      self$filetype <- resolved_config$filetype
      self$timezone <- resolved_config$timezone
      self$clif_version <- clif_version %||% resolved_config$clif_version %||% DEFAULT_CLIF_VERSION

      self$output_directory <- resolved_config$output_directory %||% file.path(getwd(), "output")
      if (!dir.exists(self$output_directory)) {
        dir.create(self$output_directory, recursive = TRUE, showWarnings = FALSE)
      }

      self$stitch_encounter <- stitch_encounter
      self$stitch_time_interval <- stitch_time_interval
      self$encounter_mapping <- NULL

      invisible(self)
    },

    #' @description Load one CLIF table and store it on the orchestrator.
    #' @param table_name snake_case CLIF table name.
    #' @param sample_size Optional maximum number of rows to read.
    #' @param columns Optional character vector of columns to read.
    #' @param filters Optional named list of equality filters applied at read time.
    #' @return The loaded table object.
    load_table = function(table_name, sample_size = NULL, columns = NULL, filters = NULL) {
      if (!table_name %in% CLIF_TABLE_NAMES) {
        cli::cli_abort(c(
          "Unknown table: {.val {table_name}}",
          "i" = "Available tables: {.val {CLIF_TABLE_NAMES}}"
        ))
      }

      table_object <- clif_table_from_file(
        table_name = table_name,
        data_directory = self$data_directory,
        filetype = self$filetype,
        timezone = self$timezone,
        output_directory = self$output_directory,
        sample_size = sample_size,
        columns = columns,
        filters = filters,
        clif_version = self$clif_version
      )

      self[[table_name]] <- table_object
      table_object
    },

    #' @description Load several tables at once.
    #'
    #' Runs encounter stitching afterwards when `stitch_encounter` is `TRUE`.
    #' @param tables Character vector of table names. Defaults to `"patient"`.
    #' @param sample_size Optional maximum number of rows per table.
    #' @param columns Optional named list mapping table names to column vectors.
    #' @param filters Optional named list mapping table names to filter lists.
    #' @return The orchestrator, invisibly.
    initialize_tables = function(tables = NULL, sample_size = NULL, columns = NULL, filters = NULL) {
      if (is.null(tables)) {
        tables <- "patient"
      }

      for (table_name in tables) {
        table_columns <- if (!is.null(columns)) columns[[table_name]] else NULL
        table_filters <- if (!is.null(filters)) filters[[table_name]] else NULL

        tryCatch(
          self$load_table(table_name, sample_size, table_columns, table_filters),
          error = function(condition) {
            cli::cli_alert_warning("Could not load {.val {table_name}}: {conditionMessage(condition)}")
          }
        )
      }

      if (isTRUE(self$stitch_encounter)) {
        self$run_stitch_encounters()
      }

      invisible(self)
    },

    #' @description Link hospitalizations that belong to one continuous encounter.
    #'
    #' Updates `hospitalization$df` and `adt$df` in place and stores the mapping in
    #' `encounter_mapping`.
    #' @return The orchestrator, invisibly.
    run_stitch_encounters = function() {
      if (is.null(self$hospitalization) || is.null(self$adt)) {
        self$load_table("hospitalization")
        self$load_table("adt")
      }

      stitched <- stitch_encounters(
        self$hospitalization$df,
        self$adt$df,
        time_interval = self$stitch_time_interval
      )

      self$hospitalization$df <- stitched$hospitalization
      self$adt$df <- stitched$adt
      self$encounter_mapping <- stitched$encounter_mapping

      invisible(self)
    },

    #' @description Names of the tables currently loaded.
    #' @return Character vector of table names, in registry order.
    get_loaded_tables = function() {
      Filter(function(table_name) !is.null(self[[table_name]]), CLIF_TABLE_NAMES)
    },

    #' @description The loaded table objects.
    #' @return A named list of table objects, in registry order.
    get_tables_obj_list = function() {
      loaded_table_names <- self$get_loaded_tables()
      stats::setNames(
        lapply(loaded_table_names, function(table_name) self[[table_name]]),
        loaded_table_names
      )
    },

    #' @description The encounter mapping, computing it if needed.
    #' @return A tibble mapping hospitalization_id to encounter_block.
    get_encounter_mapping = function() {
      if (is.null(self$encounter_mapping)) {
        self$run_stitch_encounters()
      }
      self$encounter_mapping
    },

    #' @description Validate every loaded table.
    #' @param verbose Whether to print per-table results.
    #' @return The orchestrator, invisibly.
    validate_all = function(verbose = TRUE) {
      loaded_table_names <- self$get_loaded_tables()

      if (length(loaded_table_names) == 0) {
        cli::cli_alert_info("No tables loaded to validate")
        return(invisible(self))
      }

      for (table_name in loaded_table_names) {
        self[[table_name]]$validate(verbose = verbose)
      }

      invisible(self)
    },

    #' @description Build a wide, time-aligned dataset across tables.
    #'
    #' Delegates to [create_wide_dataset()] and stores the result in `wide_df`.
    #' @param tables_to_load Character vector of tables to include.
    #' @param category_filters Named list of filters. For pivot-type tables the values
    #'   are category values; for wide-type tables they are column names.
    #' @param sample Whether to sample hospitalizations.
    #' @param hospitalization_ids Optional character vector of hospitalizations to include.
    #' @param encounter_blocks Optional character vector of encounter blocks to include.
    #' @param cohort_df Optional cohort data frame restricting rows by time window.
    #' @param output_format Either `"dataframe"` or a file format to save.
    #' @param save_to_data_location Whether to write the result next to the input data.
    #' @param output_filename Optional output file name.
    #' @param return_dataframe Whether to return the result as well as storing it.
    #' @param batch_size Hospitalizations processed per batch.
    #' @param memory_limit Optional DuckDB memory limit, e.g. `"4GB"`.
    #' @param threads Optional DuckDB thread count.
    #' @param show_progress Whether to show a progress bar.
    #' @return The wide dataset as a tibble.
    create_wide_dataset = function(tables_to_load = NULL,
                                   category_filters = NULL,
                                   sample = FALSE,
                                   hospitalization_ids = NULL,
                                   encounter_blocks = NULL,
                                   cohort_df = NULL,
                                   output_format = "dataframe",
                                   save_to_data_location = FALSE,
                                   output_filename = NULL,
                                   return_dataframe = TRUE,
                                   batch_size = 1000,
                                   memory_limit = NULL,
                                   threads = NULL,
                                   show_progress = TRUE) {
      # Encounter blocks are resolved to hospitalization ids up front, since the
      # underlying CLIF tables are keyed by hospitalization rather than encounter.
      if (!is.null(encounter_blocks)) {
        encounter_mapping <- self$get_encounter_mapping()
        matching_rows <- encounter_mapping$encounter_block %in% encounter_blocks
        hospitalization_ids <- unique(encounter_mapping$hospitalization_id[matching_rows])
      }

      # clifpy builds the coalesced assessment_value column in the orchestrator,
      # before the wide engine runs, because the wide config expects that column to
      # already exist. Skipping this leaves patient_assessments unpivotable and
      # silently drops every assessment row from the result.
      assessment_requested <-
        (!is.null(tables_to_load) && "patient_assessments" %in% tables_to_load) ||
        (!is.null(category_filters) && "patient_assessments" %in% names(category_filters))
      if (assessment_requested) {
        private$prepare_patient_assessment_value()
      }

      wide_dataset <- create_wide_dataset(
        clif_instance = self,
        optional_tables = tables_to_load,
        category_filters = category_filters,
        sample = sample,
        hospitalization_ids = hospitalization_ids,
        cohort_df = cohort_df,
        output_format = output_format,
        save_to_data_location = save_to_data_location,
        output_filename = output_filename,
        return_dataframe = return_dataframe,
        batch_size = batch_size,
        memory_limit = memory_limit,
        threads = threads,
        show_progress = show_progress
      )

      if (assessment_requested && !is.null(wide_dataset)) {
        wide_dataset <- private$optimize_assessment_column_types(wide_dataset, category_filters)
      }

      self$wide_df <- wide_dataset

      if (return_dataframe) wide_dataset else invisible(wide_dataset)
    },

    #' @description Aggregate a wide dataset into fixed-width time windows.
    #' @param aggregation_config Named list mapping aggregation method to column names.
    #'   Methods: `mean`, `max`, `min`, `median`, `first`, `last`, `boolean`, `one_hot_encode`.
    #' @param wide_df Wide dataset to aggregate. Defaults to `self$wide_df`.
    #' @param id_name Grouping column, e.g. `"hospitalization_id"`.
    #' @param hourly_window Window width in hours.
    #' @param fill_gaps Whether to emit rows for windows with no observations.
    #' @param memory_limit Optional DuckDB memory limit.
    #' @param temp_directory Optional DuckDB spill directory.
    #' @param batch_size Optional batch size.
    #' @return An aggregated tibble.
    convert_wide_to_hourly = function(aggregation_config,
                                      wide_df = NULL,
                                      id_name = "hospitalization_id",
                                      hourly_window = 1,
                                      fill_gaps = FALSE,
                                      memory_limit = "4GB",
                                      temp_directory = NULL,
                                      batch_size = NULL) {
      wide_dataset <- wide_df %||% self$wide_df
      if (is.null(wide_dataset)) {
        cli::cli_abort(c(
          "No wide dataset available.",
          "i" = "Call {.fn create_wide_dataset} first or pass {.arg wide_df}."
        ))
      }

      convert_wide_to_hourly(
        wide_df = wide_dataset,
        aggregation_config = aggregation_config,
        id_name = id_name,
        hourly_window = hourly_window,
        fill_gaps = fill_gaps,
        memory_limit = memory_limit,
        temp_directory = temp_directory,
        batch_size = batch_size,
        timezone = self$timezone
      )
    },

    #' @description Convert continuous medication doses to preferred units.
    #'
    #' Loads the medication and weight data it needs, then stores the converted data
    #' on the medication table as `df_converted` with `conversion_counts` alongside.
    #' @param preferred_units Named list mapping medication category to target unit.
    #' @param vitals_df Optional vitals data frame supplying `weight_kg`.
    #' @param hospitalization_ids Optional character vector to restrict processing.
    #' @param show_intermediate Whether to keep intermediate calculation columns.
    #' @param override Whether to continue past unacceptable target units.
    #' @param save_to_table Whether to store results on the table object.
    #' @return A named list with `converted` and `counts` when `save_to_table` is
    #'   `FALSE`; otherwise the orchestrator, invisibly.
    convert_dose_units_for_continuous_meds = function(preferred_units,
                                                      vitals_df = NULL,
                                                      hospitalization_ids = NULL,
                                                      show_intermediate = FALSE,
                                                      override = FALSE,
                                                      save_to_table = TRUE) {
      private$convert_dose_units_for_table(
        table_name = "medication_admin_continuous",
        preferred_units = preferred_units,
        vitals_df = vitals_df,
        hospitalization_ids = hospitalization_ids,
        show_intermediate = show_intermediate,
        override = override,
        save_to_table = save_to_table
      )
    },

    #' @description Convert intermittent medication doses to preferred units.
    #' @param preferred_units Named list mapping medication category to target unit.
    #' @param vitals_df Optional vitals data frame supplying `weight_kg`.
    #' @param hospitalization_ids Optional character vector to restrict processing.
    #' @param show_intermediate Whether to keep intermediate calculation columns.
    #' @param override Whether to continue past unacceptable target units.
    #' @param save_to_table Whether to store results on the table object.
    #' @return A named list with `converted` and `counts` when `save_to_table` is
    #'   `FALSE`; otherwise the orchestrator, invisibly.
    convert_dose_units_for_intermittent_meds = function(preferred_units,
                                                        vitals_df = NULL,
                                                        hospitalization_ids = NULL,
                                                        show_intermediate = FALSE,
                                                        override = FALSE,
                                                        save_to_table = TRUE) {
      private$convert_dose_units_for_table(
        table_name = "medication_admin_intermittent",
        preferred_units = preferred_units,
        vitals_df = vitals_df,
        hospitalization_ids = hospitalization_ids,
        show_intermediate = show_intermediate,
        override = override,
        save_to_table = save_to_table
      )
    },

    #' @description Compute SOFA scores.
    #'
    #' Builds the wide dataset SOFA needs when one is not supplied, then delegates to
    #' [compute_sofa()]. Results are stored in `sofa_df`.
    #' @param wide_df Optional pre-built wide dataset.
    #' @param cohort_df Optional cohort restricting observations by time window.
    #' @param extremal_type `"worst"` (default) or `"latest"`.
    #' @param id_name Grouping column: `"encounter_block"` or `"hospitalization_id"`.
    #' @param fill_na_scores_with_zero Whether missing component scores default to 0.
    #' @param remove_outliers Whether to nullify out-of-range values first.
    #' @param create_new_wide_df Whether to build a fresh wide dataset for SOFA.
    #' @return A tibble of SOFA component scores and totals per ID.
    compute_sofa_scores = function(wide_df = NULL,
                                   cohort_df = NULL,
                                   extremal_type = "worst",
                                   id_name = "encounter_block",
                                   fill_na_scores_with_zero = TRUE,
                                   remove_outliers = TRUE,
                                   create_new_wide_df = TRUE) {
      if (!is.null(cohort_df) && !id_name %in% names(cohort_df)) {
        cli::cli_abort("{.arg id_name} {.val {id_name}} not found in {.arg cohort_df} columns")
      }

      if (!is.null(wide_df)) {
        sofa_input <- wide_df
      } else if (create_new_wide_df) {
        sofa_input <- self$create_wide_dataset(
          tables_to_load = names(REQUIRED_SOFA_CATEGORIES_BY_TABLE),
          category_filters = REQUIRED_SOFA_CATEGORIES_BY_TABLE,
          cohort_df = cohort_df,
          return_dataframe = TRUE
        )
        self$wide_df_sofa <- sofa_input
      } else if (!is.null(self$wide_df)) {
        sofa_input <- self$wide_df
      } else {
        sofa_input <- self$create_wide_dataset(
          tables_to_load = names(REQUIRED_SOFA_CATEGORIES_BY_TABLE),
          category_filters = REQUIRED_SOFA_CATEGORIES_BY_TABLE,
          cohort_df = cohort_df,
          return_dataframe = TRUE
        )
      }

      if (!id_name %in% names(sofa_input)) {
        if (is.null(self$encounter_mapping)) {
          self$run_stitch_encounters()
        }
        sofa_input <- dplyr::left_join(sofa_input, self$encounter_mapping, by = "hospitalization_id")
        self$wide_df <- sofa_input
      }

      sofa_scores <- compute_sofa(
        wide_df = sofa_input,
        cohort_df = cohort_df,
        extremal_type = extremal_type,
        id_name = id_name,
        fill_na_scores_with_zero = fill_na_scores_with_zero,
        remove_outliers = remove_outliers
      )

      self$sofa_df <- sofa_scores
      sofa_scores
    },

    #' @description Charlson Comorbidity Index for the loaded diagnosis table.
    #' @param hierarchy Whether to apply the comorbidity hierarchy rules.
    #' @return A tibble of per-hospitalization condition flags and `cci_score`.
    compute_cci_scores = function(hierarchy = TRUE) {
      if (is.null(self$hospital_diagnosis)) {
        self$load_table("hospital_diagnosis")
      }
      calculate_cci(self$hospital_diagnosis, hierarchy = hierarchy)
    },

    #' @description Elixhauser comorbidity index for the loaded diagnosis table.
    #' @param hierarchy Whether to apply the comorbidity hierarchy rules.
    #' @return A tibble of per-hospitalization condition flags and `elix_score`.
    compute_elix_scores = function(hierarchy = TRUE) {
      if (is.null(self$hospital_diagnosis)) {
        self$load_table("hospital_diagnosis")
      }
      calculate_elix(self$hospital_diagnosis, hierarchy = hierarchy)
    },

    #' @description System resource information.
    #' @param print_summary Whether to print a human-readable summary.
    #' @return A named list with logical and physical CPU counts.
    get_sys_resource_info = function(print_summary = TRUE) {
      resource_info <- list(
        cpu_count = parallel::detectCores(logical = TRUE),
        cpu_count_physical = parallel::detectCores(logical = FALSE)
      )

      if (print_summary) {
        cli::cli_h3("System resources")
        cli::cli_li("Logical CPUs: {.val {resource_info$cpu_count}}")
        cli::cli_li("Physical CPUs: {.val {resource_info$cpu_count_physical}}")
      }

      resource_info
    },

    #' @description Print an overview of the orchestrator state.
    #' @param ... Unused; present for print compatibility.
    #' @return The orchestrator, invisibly.
    print = function(...) {
      cli::cli_h2("ClifOrchestrator (CLIF {self$clif_version})")
      cli::cli_li("Data directory: {.file {self$data_directory}}")
      cli::cli_li("Filetype: {.val {self$filetype}}")
      cli::cli_li("Timezone: {.val {self$timezone}}")

      loaded_table_names <- self$get_loaded_tables()
      if (length(loaded_table_names) == 0) {
        cli::cli_alert_info("No tables loaded")
      } else {
        cli::cli_h3("Loaded tables")
        for (table_name in loaded_table_names) {
          table_object <- self[[table_name]]
          cli::cli_li("{table_name}: {.val {nrow(table_object$df)}} rows")
        }
      }
      invisible(self)
    }
  ),
  private = list(
    # Coalesce patient_assessments numerical_value and categorical_value into a
    # single string assessment_value column, the form the wide config pivots on.
    # numerical_value wins where both are present, matching clifpy's Polars coalesce.
    # The wide engine cannot pivot patient_assessments without this column, so it must
    # run before create_wide_dataset() delegates.
    prepare_patient_assessment_value = function() {
      if (is.null(self$patient_assessments)) {
        self$load_table("patient_assessments")
      }

      assessments_table <- self$patient_assessments
      if (is.null(assessments_table) || is.null(assessments_table$df)) {
        return(invisible(NULL))
      }

      assessment_frame <- assessments_table$df
      has_source_columns <- all(c("numerical_value", "categorical_value") %in% names(assessment_frame))
      already_prepared <- "assessment_value" %in% names(assessment_frame)
      if (!has_source_columns || already_prepared) {
        return(invisible(NULL))
      }

      coalesced_value <- ifelse(
        is.na(assessment_frame$numerical_value),
        as.character(assessment_frame$categorical_value),
        as.character(assessment_frame$numerical_value)
      )
      assessment_frame$assessment_value <- coalesced_value
      assessments_table$df <- assessment_frame

      invisible(NULL)
    },

    # After the wide dataset is built, an assessment column that is overwhelmingly
    # numeric is converted from string to numeric; a column with meaningful text
    # values is left as-is. clifpy uses a 95% parse-success threshold, so a handful
    # of stray text values in an otherwise-numeric column does not block conversion.
    optimize_assessment_column_types = function(wide_dataset, category_filters) {
      if (!is.null(category_filters) && !is.null(category_filters[["patient_assessments"]])) {
        candidate_columns <- category_filters[["patient_assessments"]]
      } else if (!is.null(self$patient_assessments) && !is.null(self$patient_assessments$schema)) {
        candidate_columns <- schema_permissible_values(
          self$patient_assessments$schema, "assessment_category"
        )
      } else {
        candidate_columns <- character(0)
      }
      candidate_columns <- intersect(candidate_columns, names(wide_dataset))

      for (column_name in candidate_columns) {
        column_values <- wide_dataset[[column_name]]
        if (is.numeric(column_values)) {
          next
        }
        non_missing_count <- sum(!is.na(column_values))
        if (non_missing_count == 0) {
          next
        }
        parsed_values <- suppressWarnings(as.numeric(as.character(column_values)))
        parsed_count <- sum(!is.na(parsed_values))
        if (parsed_count / non_missing_count >= 0.95) {
          wide_dataset[[column_name]] <- parsed_values
        }
      }

      wide_dataset
    },

    # Both medication conversion entry points differ only in which table they read,
    # so the loading and delegation logic lives here once.
    convert_dose_units_for_table = function(table_name,
                                            preferred_units,
                                            vitals_df,
                                            hospitalization_ids,
                                            show_intermediate,
                                            override,
                                            save_to_table) {
      if (is.null(self[[table_name]])) {
        if (!is.null(hospitalization_ids)) {
          self$load_table(table_name, filters = list(hospitalization_id = hospitalization_ids))
        } else {
          self$load_table(table_name)
        }
      }

      medication_table <- self[[table_name]]

      if (is.null(hospitalization_ids)) {
        hospitalization_ids <- unique(medication_table$df$hospitalization_id)
      }

      if (is.null(vitals_df)) {
        if (is.null(self$vitals) || is.null(self$vitals$df)) {
          self$load_table(
            "vitals",
            filters = list(
              hospitalization_id = hospitalization_ids,
              vital_category = "weight_kg"
            )
          )
        }
        vitals_df <- self$vitals$df
      }

      conversion_result <- convert_dose_units_by_med_category(
        med_df = medication_table$df,
        vitals_df = vitals_df,
        preferred_units = preferred_units,
        show_intermediate = show_intermediate,
        override = override
      )

      if (save_to_table) {
        medication_table$df_converted <- conversion_result$converted
        medication_table$conversion_counts <- conversion_result$counts
        invisible(self)
      } else {
        conversion_result
      }
    }
  )
)

#' Create an orchestrator from a config file
#'
#' Convenience wrapper matching clifpy's `ClifOrchestrator.from_config` classmethod.
#'
#' @param config_path Path to a JSON or YAML config file.
#' @return A [ClifOrchestrator] instance.
#' @export
#'
#' @examples
#' \dontrun{
#' orchestrator <- clif_orchestrator_from_config("config.json")
#' }
clif_orchestrator_from_config <- function(config_path = "./config.json") {
  ClifOrchestrator$new(config_path = config_path)
}
