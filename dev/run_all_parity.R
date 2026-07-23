suppressMessages(devtools::load_all("/Users/aartiksarma/Research/clifR", quiet = TRUE))
suppressMessages(library(testthat))

test_files <- list.files(
  "/Users/aartiksarma/Research/clifR/tests/testthat",
  pattern = "^test-parity-.*\\.R$",
  full.names = TRUE
)

report_lines <- character(0)
grand_passed <- 0
grand_failed <- 0
grand_error <- 0
grand_skipped <- 0

for (test_file_path in test_files) {
  result <- tryCatch(
    as.data.frame(test_file(test_file_path, reporter = "silent")),
    error = function(condition) NULL
  )
  if (is.null(result)) {
    report_lines <- c(report_lines, sprintf("%-44s LOAD-ERROR", basename(test_file_path)))
    next
  }
  passed <- sum(result$passed)
  failed <- sum(result$failed)
  errored <- sum(result$error)
  skipped <- sum(result$skipped)
  grand_passed <- grand_passed + passed
  grand_failed <- grand_failed + failed
  grand_error <- grand_error + errored
  grand_skipped <- grand_skipped + skipped
  report_lines <- c(report_lines, sprintf(
    "%-44s p=%-4d f=%-2d e=%-2d s=%-2d", basename(test_file_path), passed, failed, errored, skipped
  ))
}

report_lines <- c(report_lines, sprintf(
  "%-44s p=%-4d f=%-2d e=%-2d s=%-2d", "TOTAL", grand_passed, grand_failed, grand_error, grand_skipped
))
writeLines(report_lines, "/Users/aartiksarma/Research/clifR/dev/last_parity_run.txt")
