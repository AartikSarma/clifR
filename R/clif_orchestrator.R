#' CLIF Orchestrator Class
#'
#' @description
#' Main orchestration class for managing multiple CLIF tables and coordinating
#' operations across them. Provides a unified interface for loading, validating,
#' and analyzing CLIF data.
#'
#' @export
#' @importFrom R6 R6Class
ClifOrchestrator <- R6::R6Class(
  classname = "ClifOrchestrator",

  public = list(
    #' @field config_path Character path to configuration file
    config_path = NULL,

    #' @field config List containing configuration
    config = NULL,

    #' @field data_directory Character path to data directory
    data_directory = NULL,

    #' @field filetype Character file type ("csv" or "parquet")
    filetype = NULL,

    #' @field timezone Character timezone for datetime columns
    timezone = NULL,

    #' @field output_directory Character path for output files
    output_directory = NULL,

    #' @field stitch_encounter Logical whether to stitch encounters
    stitch_encounter = FALSE,

    #' @field stitch_time_interval Numeric hours between encounters for stitching
    stitch_time_interval = 24,

    #' @field patient Patient table object
    patient = NULL,

    #' @field hospitalization Hospitalization table object
    hospitalization = NULL,

    #' @field adt ADT table object
    adt = NULL,

    #' @field vitals Vitals table object
    vitals = NULL,

    #' @field labs Labs table object
    labs = NULL,

    #' @field encounter_mapping tibble mapping original to stitched IDs
    encounter_mapping = NULL,

    #' @description
    #' Initialize ClifOrchestrator
    #'
    #' @param config_path Character. Path to configuration YAML file (optional).
    #' @param data_directory Character. Path to directory containing CLIF data files.
    #' @param filetype Character. File type: "csv" or "parquet" (default: "csv").
    #' @param timezone Character. Timezone for datetime columns (default: "UTC").
    #' @param output_directory Character. Path for saving outputs (default: NULL).
    #' @param stitch_encounter Logical. Whether to stitch related encounters (default: FALSE).
    #' @param stitch_time_interval Numeric. Hours between encounters to stitch (default: 24).
    #'
    #' @return A new ClifOrchestrator instance.
    initialize = function(config_path = NULL,
                          data_directory = NULL,
                          filetype = "csv",
                          timezone = "UTC",
                          output_directory = NULL,
                          stitch_encounter = FALSE,
                          stitch_time_interval = 24) {

      # Load configuration if provided
      if (!is.null(config_path)) {
        self$config_path <- config_path
        self$config <- load_config(config_path)

        # Override with config values
        self$data_directory <- self$config$data_directory %||% data_directory
        self$filetype <- self$config$filetype %||% filetype
        self$timezone <- self$config$timezone %||% timezone
        self$output_directory <- self$config$output_directory %||% output_directory
        self$stitch_encounter <- self$config$stitch_encounter %||% stitch_encounter
        self$stitch_time_interval <- self$config$stitch_time_interval %||% stitch_time_interval
      } else {
        self$data_directory <- data_directory
        self$filetype <- filetype
        self$timezone <- timezone
        self$output_directory <- output_directory
        self$stitch_encounter <- stitch_encounter
        self$stitch_time_interval <- stitch_time_interval
      }

      # Validate required parameters
      if (is.null(self$data_directory)) {
        cli::cli_abort("data_directory must be specified")
      }

      cli::cli_h1("Initializing CLIF Orchestrator")
      cli::cli_text("Data directory: {.file {self$data_directory}}")
      cli::cli_text("File type: {.val {self$filetype}}")
      cli::cli_text("Timezone: {.val {self$timezone}}")

      invisible(self)
    },

    #' @description
    #' Load a specific CLIF table
    #'
    #' @param table_name Character. Name of table to load.
    #'
    #' @return The loaded table object.
    load_table = function(table_name) {
      cli::cli_alert_info("Loading {.field {table_name}} table")

      table_obj <- switch(table_name,
        patient = Patient$new(
          data_directory = self$data_directory,
          filetype = self$filetype,
          timezone = self$timezone,
          output_directory = self$output_directory
        ),
        hospitalization = Hospitalization$new(
          data_directory = self$data_directory,
          filetype = self$filetype,
          timezone = self$timezone,
          output_directory = self$output_directory
        ),
        adt = Adt$new(
          data_directory = self$data_directory,
          filetype = self$filetype,
          timezone = self$timezone,
          output_directory = self$output_directory
        ),
        vitals = Vitals$new(
          data_directory = self$data_directory,
          filetype = self$filetype,
          timezone = self$timezone,
          output_directory = self$output_directory
        ),
        labs = Labs$new(
          data_directory = self$data_directory,
          filetype = self$filetype,
          timezone = self$timezone,
          output_directory = self$output_directory
        ),
        {
          cli::cli_abort("Unknown table: {.val {table_name}}")
        }
      )

      # Store in appropriate field
      self[[table_name]] <- table_obj

      return(table_obj)
    },

    #' @description
    #' Initialize and load multiple tables
    #'
    #' @param tables Character vector of table names to load. If NULL, loads all available.
    #' @param validate Logical. Whether to validate after loading (default: TRUE).
    #'
    #' @return Invisible self.
    initialize_tables = function(tables = NULL, validate = TRUE) {

      # Default tables to load
      if (is.null(tables)) {
        tables <- c("patient", "hospitalization", "adt", "vitals", "labs")
      }

      cli::cli_h2("Loading Tables")

      # Load each table
      for (table_name in tables) {
        file_path <- file.path(
          self$data_directory,
          paste0(table_name, ".", self$filetype)
        )

        if (file.exists(file_path)) {
          tryCatch({
            self$load_table(table_name)
          }, error = function(e) {
            cli::cli_alert_warning(
              "Failed to load {.field {table_name}}: {e$message}"
            )
          })
        } else {
          cli::cli_alert_warning(
            "Table {.field {table_name}} not found, skipping"
          )
        }
      }

      # Apply encounter stitching if requested
      if (self$stitch_encounter && !is.null(self$hospitalization)) {
        cli::cli_h2("Stitching Encounters")
        self$apply_stitching()
      }

      # Validate if requested
      if (validate) {
        self$validate_all()
      }

      invisible(self)
    },

    #' @description
    #' Validate all loaded tables
    #'
    #' @param verbose Logical. Print validation details (default: TRUE).
    #'
    #' @return List of validation results for each table.
    validate_all = function(verbose = TRUE) {
      cli::cli_h2("Validating All Tables")

      validation_results <- list()

      for (table_name in c("patient", "hospitalization", "adt", "vitals", "labs")) {
        table_obj <- self[[table_name]]

        if (!is.null(table_obj)) {
          cli::cli_rule(table_name)
          validation_results[[table_name]] <- table_obj$validate(verbose = verbose)
        }
      }

      return(validation_results)
    },

    #' @description
    #' Apply encounter stitching to hospitalization data
    #'
    #' @return Invisible self.
    apply_stitching = function() {
      if (is.null(self$hospitalization)) {
        cli::cli_abort("Hospitalization table not loaded")
      }

      # Get stitching mapping
      self$encounter_mapping <- stitch_encounters(
        self$hospitalization,
        time_interval_hours = self$stitch_time_interval
      )

      # Update hospitalization data
      self$hospitalization$df <- apply_encounter_stitching(
        self$hospitalization,
        time_interval_hours = self$stitch_time_interval
      )

      invisible(self)
    },

    #' @description
    #' Get summary of all loaded tables
    #'
    #' @return List of summary information.
    summary = function() {
      cli::cli_h1("CLIF Data Summary")

      summary_info <- list(
        data_directory = self$data_directory,
        timezone = self$timezone,
        tables_loaded = list()
      )

      for (table_name in c("patient", "hospitalization", "adt", "vitals", "labs")) {
        table_obj <- self[[table_name]]

        if (!is.null(table_obj)) {
          cli::cli_h2(table_name)
          summary_info$tables_loaded[[table_name]] <- list(
            n_rows = nrow(table_obj$df),
            n_cols = ncol(table_obj$df),
            is_valid = table_obj$is_valid()
          )

          cli::cli_text("Rows: {.val {nrow(table_obj$df)}}")
          cli::cli_text("Columns: {.val {ncol(table_obj$df)}}")

          if (!is.null(table_obj$validation_results)) {
            status <- if (table_obj$validation_results$is_valid) {
              cli::col_green("Valid")
            } else {
              cli::col_red("Invalid")
            }
            cli::cli_text("Status: {status}")
          }
          cli::cli_text("")
        }
      }

      return(invisible(summary_info))
    },

    #' @description
    #' Export all validation reports
    #'
    #' @param output_dir Character. Directory for reports (default: uses self$output_directory).
    #'
    #' @return Invisible self.
    export_validation_reports = function(output_dir = NULL) {
      out_dir <- output_dir %||% self$output_directory

      if (is.null(out_dir)) {
        cli::cli_abort("No output directory specified")
      }

      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE)
      }

      cli::cli_h2("Exporting Validation Reports")

      for (table_name in c("patient", "hospitalization", "adt", "vitals", "labs")) {
        table_obj <- self[[table_name]]

        if (!is.null(table_obj)) {
          output_file <- file.path(
            out_dir,
            paste0(table_name, "_validation_report.md")
          )
          table_obj$export_validation_report(output_file)
        }
      }

      cli::cli_alert_success("Reports exported to {.file {out_dir}}")

      invisible(self)
    },

    #' @description
    #' Print method
    print = function() {
      cli::cli_h1("CLIF Orchestrator")

      cli::cli_text("Data directory: {.file {self$data_directory}}")
      cli::cli_text("Timezone: {.val {self$timezone}}")

      if (self$stitch_encounter) {
        cli::cli_text(
          "Encounter stitching: ENABLED ({self$stitch_time_interval} hours)"
        )
      }

      cli::cli_rule("Loaded Tables")

      tables_loaded <- c()
      for (table_name in c("patient", "hospitalization", "adt", "vitals", "labs")) {
        if (!is.null(self[[table_name]])) {
          tables_loaded <- c(tables_loaded, table_name)
          cli::cli_text(
            "  {cli::symbol$tick} {.field {table_name}} ({nrow(self[[table_name]]$df)} rows)"
          )
        }
      }

      if (length(tables_loaded) == 0) {
        cli::cli_text("  {cli::col_silver('No tables loaded yet')}")
        cli::cli_text("  {cli::col_silver('Use $initialize_tables() to load data')}")
      }

      invisible(self)
    },

    #' @description
    #' Create wide dataset from vitals and labs
    #'
    #' @param time_resolution Character. Time resolution: "hour", "4hour", "day" (default: "hour").
    #' @param id_col Character. ID column name (default: "hospitalization_id").
    #'
    #' @return tibble in wide format.
    create_wide_dataset = function(time_resolution = "hour",
                                   id_col = "hospitalization_id") {

      if (is.null(self$vitals) && is.null(self$labs)) {
        cli::cli_abort("At least one of vitals or labs must be loaded")
      }

      wide_data <- create_wide_dataset(
        vitals_data = if (!is.null(self$vitals)) self$vitals$df else NULL,
        labs_data = if (!is.null(self$labs)) self$labs$df else NULL,
        hospitalization_data = if (!is.null(self$hospitalization)) self$hospitalization$df else NULL,
        time_resolution = time_resolution,
        id_col = id_col
      )

      return(wide_data)
    },

    #' @description
    #' Calculate SOFA scores from wide dataset
    #'
    #' @param wide_data tibble. Wide format data (optional, will create if not provided).
    #' @param time_resolution Character. Time resolution if creating wide data (default: "hour").
    #' @param ... Additional arguments passed to calculate_sofa_time_series().
    #'
    #' @return tibble with SOFA scores.
    calculate_sofa_scores = function(wide_data = NULL,
                                     time_resolution = "hour",
                                     ...) {

      if (is.null(wide_data)) {
        cli::cli_alert_info("Creating wide dataset for SOFA calculation...")
        wide_data <- self$create_wide_dataset(time_resolution = time_resolution)
      }

      sofa_data <- calculate_sofa_time_series(
        wide_data = wide_data,
        id_col = "hospitalization_id",
        time_col = "time_rounded",
        ...
      )

      return(sofa_data)
    },

    #' @description
    #' Calculate Charlson Comorbidity Index for patients
    #'
    #' @param diagnosis_data tibble. Diagnosis data with ICD codes (required).
    #' @param patient_id_col Character. Patient ID column (default: "hospitalization_id").
    #' @param icd_code_col Character. ICD code column (default: "icd_code").
    #' @param icd_version_col Character. ICD version column (optional).
    #' @param age_col Character. Age column (optional).
    #' @param default_icd_version Character. Default ICD version (default: "10").
    #'
    #' @return tibble with CCI scores.
    calculate_charlson_scores = function(diagnosis_data,
                                        patient_id_col = "hospitalization_id",
                                        icd_code_col = "icd_code",
                                        icd_version_col = NULL,
                                        age_col = NULL,
                                        default_icd_version = "10") {

      cci_data <- calculate_charlson_scores(
        data = diagnosis_data,
        patient_id_col = patient_id_col,
        icd_code_col = icd_code_col,
        icd_version_col = icd_version_col,
        age_col = age_col,
        default_icd_version = default_icd_version
      )

      return(cci_data)
    },

    #' @description
    #' Convert medication doses to standard units
    #'
    #' @param target_units Named list. Target units by medication name.
    #' @param dose_col Character. Dose column name (default: "dose").
    #' @param unit_col Character. Unit column name (default: "dose_unit").
    #' @param medication_col Character. Medication name column (default: "medication_name").
    #' @param weight_col Character. Weight column name (default: "weight_kg").
    #'
    #' @return tibble with converted medication doses.
    convert_medication_doses = function(target_units,
                                       dose_col = "dose",
                                       unit_col = "dose_unit",
                                       medication_col = "medication_name",
                                       weight_col = "weight_kg") {

      if (is.null(self$medication_admin_continuous)) {
        cli::cli_abort("Medication administration table not loaded")
      }

      converted_data <- convert_medication_doses(
        med_data = self$medication_admin_continuous$df,
        dose_col = dose_col,
        unit_col = unit_col,
        target_units = target_units,
        weight_col = weight_col,
        medication_col = medication_col
      )

      return(converted_data)
    },

    #' @description
    #' Generate comprehensive analysis report
    #'
    #' @param include_sofa Logical. Include SOFA scores (default: FALSE).
    #' @param include_charlson Logical. Include Charlson scores (default: FALSE).
    #' @param diagnosis_data tibble. Diagnosis data for Charlson (required if include_charlson = TRUE).
    #' @param output_file Character. Output file path (optional).
    #'
    #' @return List with analysis results.
    generate_analysis_report = function(include_sofa = FALSE,
                                        include_charlson = FALSE,
                                        diagnosis_data = NULL,
                                        output_file = NULL) {

      cli::cli_h1("Generating Analysis Report")

      report <- list(
        summary = self$summary(),
        validation = self$validate_all(verbose = FALSE)
      )

      # Add SOFA scores if requested
      if (include_sofa && (!is.null(self$vitals) || !is.null(self$labs))) {
        cli::cli_h2("Calculating SOFA Scores")
        tryCatch({
          report$sofa <- self$calculate_sofa_scores()
          report$sofa_summary <- attr(report$sofa, "summary")
        }, error = function(e) {
          cli::cli_alert_warning("SOFA calculation failed: {e$message}")
          report$sofa <- NULL
        })
      }

      # Add Charlson scores if requested
      if (include_charlson && !is.null(diagnosis_data)) {
        cli::cli_h2("Calculating Charlson Comorbidity Index")
        tryCatch({
          report$charlson <- self$calculate_charlson_scores(diagnosis_data)
        }, error = function(e) {
          cli::cli_alert_warning("Charlson calculation failed: {e$message}")
          report$charlson <- NULL
        })
      }

      # Save report if output file specified
      if (!is.null(output_file)) {
        cli::cli_alert_info("Saving report to {.file {output_file}}")
        saveRDS(report, output_file)
      }

      cli::cli_alert_success("Analysis report complete")

      return(report)
    }
  )
)
