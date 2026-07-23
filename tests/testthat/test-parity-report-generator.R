# Parity for the DQA report generator (R/util_report_generator.R), a port of
# clifpy/utils/report_generator.py.
#
# The exactly-comparable outputs must match clifpy byte-for-byte (modulo the run
# timestamp): compute_table_stats, collect_dqa_issues, the plain-text report and
# the consolidated CSV. The PDF entry points are intentionally divergent — clifR
# emits self-contained HTML instead — so those are exercised only as smoke tests,
# not compared to any clifpy artifact.
#
# collect_dqa_issues / generate_text_report / generate_consolidated_csv are fed
# the SAME validation_data clifpy produced (persisted under
# tests/baseline/report_dqa_json/), isolating the report-generator logic from any
# validator differences. compute_table_stats is run on the R-loaded fixture and
# compared to clifpy's compute_table_stats baseline.
#
# Regenerate baselines with:
#   cd tests/baseline_py && uv run python generate_report_baseline.py

report_json_dir <- function() {
  file.path(parity_baseline_dir(), "report_dqa_json")
}

read_validation_data <- function(table_name) {
  jsonlite::fromJSON(
    file.path(report_json_dir(), paste0(table_name, "_dqa.json")),
    simplifyVector = FALSE
  )
}

strip_generated_line <- function(text) {
  gsub("Generated: [0-9][^\n]*", "Generated: <TIMESTAMP>", text)
}

# Project an R enriched issue to the comparable fields, matching the Python
# baseline's issue_for_json.
project_issue <- function(issue) {
  list(
    category = issue$category %||% NA_character_,
    check_type = issue$check_type %||% NA_character_,
    severity = issue$severity %||% NA_character_,
    rule_code = issue$rule_code %||% "",
    rule_description = issue$rule_description %||% "",
    column_field = issue$column_field %||% "NA",
    finding = issue$finding %||% issue$message %||% "",
    message = issue$message %||% "",
    atomic_count = as.numeric(issue$atomic_count %||% 1)
  )
}

present_report_tables <- function() {
  names_baseline <- read_parity_baseline("report_table_names")
  unlist(names_baseline$present, use.names = FALSE)
}


test_that("compute_table_stats matches clifpy for every present table", {
  skip_if_no_parity_fixture("report_table_stats")

  python_stats <- read_parity_baseline("report_table_stats")

  for (table_name in present_report_tables()) {
    r_table <- parity_table(table_name)
    r_stats <- compute_table_stats(r_table$df, r_table$schema)
    py_table_stats <- python_stats[[table_name]]

    expect_equal(
      length(r_stats), length(py_table_stats),
      info = sprintf("table '%s': column count in table_stats differs", table_name)
    )

    for (index in seq_along(py_table_stats)) {
      py_stat <- py_table_stats[[index]]
      r_stat <- r_stats[[index]]
      label <- sprintf("table '%s' column '%s'", table_name, py_stat$column)

      expect_equal(r_stat$column, py_stat$column, info = label)
      expect_equal(r_stat$dtype, py_stat$dtype, info = paste(label, "dtype"))
      expect_equal(as.numeric(r_stat$null_count), as.numeric(py_stat$null_count),
                   info = paste(label, "null_count"))
      expect_equal(as.numeric(r_stat$null_pct), as.numeric(py_stat$null_pct),
                   info = paste(label, "null_pct"))
      expect_equal(as.numeric(r_stat$unique), as.numeric(py_stat$unique),
                   info = paste(label, "unique"))
      expect_equal(r_stat$min %||% NA_character_, py_stat$min %||% NA_character_,
                   info = paste(label, "min"))
      expect_equal(r_stat$max %||% NA_character_, py_stat$max %||% NA_character_,
                   info = paste(label, "max"))
    }
  }
})


test_that("collect_dqa_issues matches clifpy structurally and in values", {
  skip_if_no_parity_fixture("report_collect_dqa_issues")

  python_collected <- read_parity_baseline("report_collect_dqa_issues")

  for (table_name in present_report_tables()) {
    validation_data <- read_validation_data(table_name)
    collected <- collect_dqa_issues(validation_data)
    py_result <- python_collected[[table_name]]

    # Category scores
    for (category in names(py_result$category_scores)) {
      py_score <- as.numeric(unlist(py_result$category_scores[[category]]))
      r_score <- as.numeric(collected$category_scores[[category]])
      expect_equal(r_score, py_score,
                   info = sprintf("table '%s' category '%s' score differs", table_name, category))
    }
    expect_setequal(names(collected$category_scores), names(py_result$category_scores))

    # Aggregate error/warning atom counts
    r_errors <- sum_atomic_count(collected$all_issues, "error")
    r_warnings <- sum_atomic_count(collected$all_issues, "warning")
    expect_equal(r_errors, as.numeric(py_result$error_count),
                 info = sprintf("table '%s' error_count differs", table_name))
    expect_equal(r_warnings, as.numeric(py_result$warning_count),
                 info = sprintf("table '%s' warning_count differs", table_name))

    # Issue-by-issue structural + value comparison, in order
    py_issues <- py_result$issues
    expect_equal(
      length(collected$all_issues), length(py_issues),
      info = sprintf("table '%s': issue count differs", table_name)
    )
    for (index in seq_along(py_issues)) {
      r_projected <- project_issue(collected$all_issues[[index]])
      py_issue <- py_issues[[index]]
      label <- sprintf("table '%s' issue %d (%s)", table_name, index, py_issue$rule_code %||% "")
      for (field in c("category", "check_type", "severity", "rule_code",
                      "rule_description", "column_field", "finding", "message")) {
        expect_equal(
          as.character(r_projected[[field]]), as.character(py_issue[[field]] %||% ""),
          info = paste(label, field)
        )
      }
      expect_equal(
        r_projected$atomic_count, as.numeric(py_issue$atomic_count),
        info = paste(label, "atomic_count")
      )
    }
  }
})


test_that("generate_text_report reproduces clifpy's report lines", {
  skip_if_no_parity_fixture("report_text_reports")

  python_reports <- read_parity_baseline("report_text_reports")
  output_directory <- parity_output_dir()

  for (table_name in present_report_tables()) {
    validation_data <- read_validation_data(table_name)
    report_path <- file.path(output_directory, sprintf("%s_report_r.txt", table_name))
    generate_text_report(validation_data, table_name, report_path, site_name = "Test Site")

    r_text <- strip_generated_line(paste(readLines(report_path, warn = FALSE), collapse = "\n"))
    py_text <- python_reports[[table_name]]

    r_lines <- strsplit(r_text, "\n", fixed = TRUE)[[1]]
    py_lines <- strsplit(py_text, "\n", fixed = TRUE)[[1]]

    expect_equal(
      length(r_lines), length(py_lines),
      info = sprintf("table '%s': text report line count differs", table_name)
    )
    for (index in seq_along(py_lines)) {
      expect_equal(
        r_lines[index], py_lines[index],
        info = sprintf("table '%s': text report line %d differs", table_name, index)
      )
    }
  }
})


test_that("generate_consolidated_csv matches clifpy byte-for-byte", {
  skip_if_no_parity_fixture("report_table_names")
  baseline_path <- file.path(parity_baseline_dir(), "report_consolidated_validation.csv")
  skip_if_not(file.exists(baseline_path), "consolidated CSV baseline not generated")

  table_names <- unlist(read_parity_baseline("report_table_names")$all, use.names = FALSE)
  loaded <- collect_table_results(report_json_dir(), table_names)

  csv_path <- file.path(parity_output_dir(), "report_consolidated_r.csv")
  generate_consolidated_csv(loaded$results, csv_path, table_names)

  r_bytes <- readBin(csv_path, "raw", n = file.info(csv_path)$size)
  py_bytes <- readBin(baseline_path, "raw", n = file.info(baseline_path)$size)

  # Line-level diff first for a readable failure, then assert exact bytes.
  r_lines <- strsplit(rawToChar(r_bytes), "\r\n", fixed = TRUE)[[1]]
  py_lines <- strsplit(rawToChar(py_bytes), "\r\n", fixed = TRUE)[[1]]
  expect_equal(length(r_lines), length(py_lines), info = "consolidated CSV row count differs")
  for (index in seq_along(py_lines)) {
    expect_equal(r_lines[index], py_lines[index],
                 info = sprintf("consolidated CSV line %d differs", index))
  }
  expect_identical(r_bytes, py_bytes)
})


test_that("generate_combined_report renders the absent-table CSV path", {
  skip_if_no_parity_fixture("report_table_names")
  baseline_path <- file.path(parity_baseline_dir(), "report_consolidated_partial.csv")
  skip_if_not(file.exists(baseline_path), "partial CSV baseline not generated")

  partial_names <- unlist(read_parity_baseline("report_table_names")$partial, use.names = FALSE)
  partial_json_dir <- file.path(parity_baseline_dir(), "report_dqa_json_partial")
  skip_if_not(dir.exists(partial_json_dir), "partial DQA JSON dir not generated")

  output_directory <- file.path(parity_output_dir(), "combined_report")
  html_path <- generate_combined_report(partial_json_dir, output_directory, partial_names)

  # HTML is the intentionally-divergent PDF stand-in: it must be produced and
  # be self-contained, but is not compared to any clifpy artifact.
  expect_true(!is.null(html_path) && file.exists(html_path))
  html_text <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("http://|https://|src=", html_text))

  # The consolidated CSV, however, is exact-parity including the absent row.
  csv_path <- file.path(output_directory, "consolidated_validation.csv")
  r_bytes <- readBin(csv_path, "raw", n = file.info(csv_path)$size)
  py_bytes <- readBin(baseline_path, "raw", n = file.info(baseline_path)$size)

  r_lines <- strsplit(rawToChar(r_bytes), "\r\n", fixed = TRUE)[[1]]
  py_lines <- strsplit(rawToChar(py_bytes), "\r\n", fixed = TRUE)[[1]]
  expect_equal(length(r_lines), length(py_lines), info = "partial CSV row count differs")
  for (index in seq_along(py_lines)) {
    expect_equal(r_lines[index], py_lines[index],
                 info = sprintf("partial CSV line %d differs", index))
  }
  expect_identical(r_bytes, py_bytes)
})


test_that("HTML report stand-ins are self-contained and PDF aliases forward", {
  skip_if_no_parity_fixture("report_table_names")

  validation_data <- read_validation_data("patient")
  output_directory <- parity_output_dir()

  html_path <- file.path(output_directory, "patient_report.html")
  expect_message(
    generate_validation_pdf(validation_data, "patient", html_path),
    "HTML"
  )
  expect_true(file.exists(html_path))
  html_text <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("<!doctype html>", html_text, fixed = TRUE))
  # No external asset references (CSP-friendly, fully portable).
  expect_false(grepl("http://|https://|src=|<link", html_text))
  # Same section structure as the text report.
  expect_true(grepl("DQA Summary", html_text))
  expect_true(grepl("Data Profile", html_text))
  expect_true(grepl("Details", html_text))

  # Combined HTML overview
  table_names <- unlist(read_parity_baseline("report_table_names")$all, use.names = FALSE)
  loaded <- collect_table_results(report_json_dir(), table_names)
  combined_path <- file.path(output_directory, "combined_report.html")
  expect_message(
    generate_combined_validation_pdf(loaded$results, combined_path, table_names),
    "HTML"
  )
  expect_true(file.exists(combined_path))
  combined_text <- paste(readLines(combined_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("DQA Overview", combined_text))
  expect_false(grepl("http://|https://|src=|<link", combined_text))
})
