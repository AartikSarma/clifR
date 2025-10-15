#' Microbiology Susceptibility Table Class
#'
#' @description
#' R6 class for CLIF microbiology_susceptibility table containing antibiotic
#' susceptibility testing results linked to culture organisms.
#'
#' @export
#' @importFrom R6 R6Class
MicrobiologySusceptibility <- R6::R6Class(
  classname = "MicrobiologySusceptibility",
  inherit = BaseTable,

  public = list(
    #' @description
    #' Initialize MicrobiologySusceptibility table
    #'
    #' @param data_directory Character path to data directory.
    #' @param filetype Character file type ("csv" or "parquet").
    #' @param timezone Character timezone for datetime columns.
    #' @param output_directory Character path for output files.
    #'
    #' @return A new MicrobiologySusceptibility instance.
    initialize = function(data_directory = NULL,
                         filetype = "csv",
                         timezone = "UTC",
                         output_directory = NULL) {

      super$initialize(
        table_name = "microbiology_susceptibility",
        data_directory = data_directory,
        filetype = filetype,
        timezone = timezone,
        output_directory = output_directory
      )
    },

    #' @description
    #' Filter by antimicrobial agent
    #'
    #' @param antimicrobial_category Character antimicrobial to filter by.
    #'
    #' @return Filtered tibble.
    filter_by_antimicrobial = function(antimicrobial_category) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$antimicrobial_category == !!antimicrobial_category)
    },

    #' @description
    #' Filter by susceptibility result
    #'
    #' @param susceptibility Character susceptibility result (susceptible, non_susceptible, intermediate).
    #'
    #' @return Filtered tibble.
    filter_by_susceptibility = function(susceptibility) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$susceptibility_category == !!susceptibility)
    },

    #' @description
    #' Get resistant organisms (non-susceptible results)
    #'
    #' @return tibble with resistant organism results.
    get_resistant_organisms = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$susceptibility_category == "non_susceptible")
    },

    #' @description
    #' Get susceptibility results for specific organism
    #'
    #' @param organism_id Character organism ID to filter by.
    #'
    #' @return tibble with organism's susceptibility results.
    get_organism_susceptibilities = function(organism_id) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$organism_id == !!organism_id)
    },

    #' @description
    #' Calculate susceptibility rates by antimicrobial
    #'
    #' @return tibble with susceptibility statistics by drug.
    get_antimicrobial_summary = function() {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::group_by(antimicrobial_category) %>%
        dplyr::summarize(
          n_tests = dplyr::n(),
          n_susceptible = sum(.data$susceptibility_category == "susceptible", na.rm = TRUE),
          n_intermediate = sum(.data$susceptibility_category == "intermediate", na.rm = TRUE),
          n_resistant = sum(.data$susceptibility_category == "non_susceptible", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          susceptibility_rate = n_susceptible / n_tests,
          resistance_rate = n_resistant / n_tests
        ) %>%
        dplyr::arrange(dplyr::desc(n_tests))
    },

    #' @description
    #' Identify multi-drug resistant organisms
    #'
    #' @param min_resistances Integer minimum number of resistances to classify as MDR.
    #'
    #' @return tibble with MDR organism IDs.
    get_mdr_organisms = function(min_resistances = 3) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      self$df %>%
        dplyr::filter(.data$susceptibility_category == "non_susceptible") %>%
        dplyr::group_by(organism_id) %>%
        dplyr::summarize(
          n_resistances = dplyr::n(),
          resistant_drugs = paste(unique(antimicrobial_category), collapse = ", "),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_resistances >= min_resistances) %>%
        dplyr::arrange(dplyr::desc(n_resistances))
    },

    #' @description
    #' Get susceptibility patterns for common antibiotics
    #'
    #' @param common_only Logical, only show common antibiotics.
    #'
    #' @return tibble with antibiotic susceptibility patterns.
    get_antibiogram = function(common_only = TRUE) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      # Common antibiotics for ICU empiric therapy
      common_antibiotics <- c(
        "vancomycin", "piperacillin_tazobactam", "cefepime",
        "meropenem", "ciprofloxacin", "levofloxacin",
        "ceftriaxone", "ampicillin_sulbactam", "gentamicin",
        "tobramycin", "trimethoprim_sulfamethoxazole"
      )

      data <- self$df
      if (common_only) {
        data <- data %>%
          dplyr::filter(.data$antimicrobial_category %in% common_antibiotics)
      }

      data %>%
        dplyr::group_by(antimicrobial_category) %>%
        dplyr::summarize(
          n_tested = dplyr::n(),
          pct_susceptible = round(
            sum(.data$susceptibility_category == "susceptible", na.rm = TRUE) / dplyr::n() * 100,
            1
          ),
          pct_intermediate = round(
            sum(.data$susceptibility_category == "intermediate", na.rm = TRUE) / dplyr::n() * 100,
            1
          ),
          pct_resistant = round(
            sum(.data$susceptibility_category == "non_susceptible", na.rm = TRUE) / dplyr::n() * 100,
            1
          ),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_tested))
    },

    #' @description
    #' Link susceptibility results with culture data
    #'
    #' @param culture_data tibble from MicrobiologyCulture table.
    #'
    #' @return tibble with combined culture and susceptibility data.
    join_with_cultures = function(culture_data) {
      if (is.null(self$df)) {
        cli::cli_abort("No data loaded")
      }

      if (missing(culture_data) || is.null(culture_data)) {
        cli::cli_abort("culture_data required")
      }

      self$df %>%
        dplyr::inner_join(
          culture_data %>%
            dplyr::select(
              organism_id, patient_id, hospitalization_id,
              organism_category, organism_group,
              fluid_category, collect_dttm
            ),
          by = "organism_id"
        )
    },

    #' @description
    #' Summarize susceptibility data
    #'
    #' @return List with summary information.
    summarize = function() {
      if (is.null(self$df)) {
        cli::cli_alert_warning("No data to summarize")
        return(invisible(NULL))
      }

      cli::cli_h2("Microbiology Susceptibility Summary")

      n_total <- nrow(self$df)
      n_organisms <- dplyr::n_distinct(self$df$organism_id)
      n_resistant <- sum(self$df$susceptibility_category == "non_susceptible", na.rm = TRUE)
      resistance_rate <- n_resistant / n_total

      cli::cli_text("Total susceptibility tests: {.val {n_total}}")
      cli::cli_text("Unique organisms tested: {.val {n_organisms}}")
      cli::cli_text("Resistant results: {.val {n_resistant}} ({round(resistance_rate*100, 1)}%)")

      cli::cli_h3("Top 10 Tested Antimicrobials")
      antimicrobial_summary <- self$get_antimicrobial_summary() %>%
        dplyr::slice_head(n = 10)
      print(antimicrobial_summary)

      invisible(list(
        n_total = n_total,
        n_organisms = n_organisms,
        n_resistant = n_resistant,
        resistance_rate = resistance_rate,
        antimicrobial_summary = antimicrobial_summary
      ))
    }
  )
)
