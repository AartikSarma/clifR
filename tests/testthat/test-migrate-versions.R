# CLIF 2.1 -> 3.0 directory migration runner. The fixture cohort is already CLIF
# 3.0, so these tests build a tiny synthetic 2.1-style input in tempdir() carrying
# 2.1 category values (title-case race_category, `l&d` ADT location) and assert
# the runner preserves row/ID counts and normalizes values to the 3.0 form that
# crosswalk_table_2_1_to_3_0() produces on the same input.

# Write a minimal 2.1-style parquet folder; returns its path.
write_synthetic_clif_21 <- function(directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  patient <- tibble::tibble(
    patient_id = c("1", "2", "3"),
    race_category = c("White", "Black or African American", "Asian")
  )
  adt <- tibble::tibble(
    hospitalization_id = c("10", "11", "12"),
    patient_id = c("1", "2", "3"),
    location_category = c("l&d", "icu", "ward"),
    in_dttm = as.POSIXct(
      c("2020-01-01 00:00", "2020-01-02 00:00", "2020-01-03 00:00"),
      tz = "UTC"
    )
  )
  vitals <- tibble::tibble(
    hospitalization_id = c("10", "10", "11"),
    vital_category = c("heart_rate", "sbp", "spo2"),
    vital_value = c(80, 120, 98)
  )

  arrow::write_parquet(patient, file.path(directory, "clif_patient.parquet"))
  arrow::write_parquet(adt, file.path(directory, "clif_adt.parquet"))
  arrow::write_parquet(vitals, file.path(directory, "clif_vitals.parquet"))
  # A non-beta scratch file that must never be written to the output.
  arrow::write_parquet(tibble::tibble(x = 1L), file.path(directory, "clif_scratch.parquet"))

  invisible(directory)
}

test_that("audit buckets beta, non-beta and missing tables", {
  input_directory <- file.path(tempfile("clif21_"))
  output_directory <- file.path(tempfile("clif30_"))
  write_synthetic_clif_21(input_directory)

  runner <- suppressMessages(CrosswalkMigrationRunner$new(
    data_dir = input_directory, output_dir = output_directory
  ))
  audit_result <- runner$audit()

  expect_setequal(audit_result$beta, c("adt", "patient", "vitals"))
  expect_equal(audit_result$non_beta, "scratch")
  expect_false("adt" %in% audit_result$missing)
  expect_true("hospitalization" %in% audit_result$missing)
})

test_that("run migrates beta tables, preserving row and ID counts", {
  input_directory <- file.path(tempfile("clif21_"))
  output_directory <- file.path(tempfile("clif30_"))
  write_synthetic_clif_21(input_directory)

  migration_succeeded <- suppressMessages(
    CrosswalkMigrationRunner$new(data_dir = input_directory, output_dir = output_directory)$run()
  )
  expect_true(migration_succeeded)

  # Beta tables are written; the non-beta scratch file is not.
  expect_true(file.exists(file.path(output_directory, "clif_patient.parquet")))
  expect_true(file.exists(file.path(output_directory, "clif_adt.parquet")))
  expect_true(file.exists(file.path(output_directory, "clif_vitals.parquet")))
  expect_false(file.exists(file.path(output_directory, "clif_scratch.parquet")))

  # (a) Row counts preserved for each migrated table.
  for (table_name in c("patient", "adt", "vitals")) {
    source_frame <- arrow::read_parquet(file.path(input_directory, sprintf("clif_%s.parquet", table_name)))
    migrated_frame <- arrow::read_parquet(file.path(output_directory, sprintf("clif_%s.parquet", table_name)))
    expect_equal(nrow(migrated_frame), nrow(source_frame),
                 info = sprintf("row count changed for %s", table_name))
    expect_setequal(names(migrated_frame), names(source_frame))
  }

  # Distinct ID counts preserved.
  migrated_adt <- arrow::read_parquet(file.path(output_directory, "clif_adt.parquet"))
  expect_equal(dplyr::n_distinct(migrated_adt$patient_id), 3L)
  expect_equal(dplyr::n_distinct(migrated_adt$hospitalization_id), 3L)
})

test_that("migrated values match crosswalk_table_2_1_to_3_0 on the same input", {
  input_directory <- file.path(tempfile("clif21_"))
  output_directory <- file.path(tempfile("clif30_"))
  write_synthetic_clif_21(input_directory)

  suppressMessages(
    CrosswalkMigrationRunner$new(data_dir = input_directory, output_dir = output_directory)$run()
  )

  # (b) The runner's per-table output must equal the in-memory crosswalk output.
  for (table_name in c("patient", "adt", "vitals")) {
    source_frame <- arrow::read_parquet(file.path(input_directory, sprintf("clif_%s.parquet", table_name)))
    expected <- crosswalk_table_2_1_to_3_0(source_frame, table_name)$data
    migrated_frame <- arrow::read_parquet(file.path(output_directory, sprintf("clif_%s.parquet", table_name)))

    for (standardized_column in intersect(names(expected), names(migrated_frame))) {
      if (endsWith(standardized_column, "_category")) {
        expect_equal(
          migrated_frame[[standardized_column]], expected[[standardized_column]],
          info = sprintf("value mismatch for %s.%s", table_name, standardized_column)
        )
      }
    }
  }

  # Spot-check the specific 2.1 -> 3.0 normalizations.
  migrated_patient <- arrow::read_parquet(file.path(output_directory, "clif_patient.parquet"))
  expect_equal(migrated_patient$race_category, c("white", "black_or_african_american", "asian"))
  migrated_adt <- arrow::read_parquet(file.path(output_directory, "clif_adt.parquet"))
  expect_equal(migrated_adt$location_category, c("l_and_d", "icu", "ward"))
})

test_that("dry_run audits only and writes nothing", {
  input_directory <- file.path(tempfile("clif21_"))
  output_directory <- file.path(tempfile("clif30_"))
  write_synthetic_clif_21(input_directory)

  migration_succeeded <- suppressMessages(
    CrosswalkMigrationRunner$new(data_dir = input_directory, output_dir = output_directory)$run(dry_run = TRUE)
  )
  expect_true(migration_succeeded)

  # (c) No data files written under dry-run (the logs/ folder may exist).
  written_data_files <- list.files(output_directory, pattern = "\\.parquet$", recursive = TRUE)
  expect_length(written_data_files, 0)
})

test_that("migrate_clif_2_1_to_3_0 wrapper runs the migration", {
  input_directory <- file.path(tempfile("clif21_"))
  output_directory <- file.path(tempfile("clif30_"))
  write_synthetic_clif_21(input_directory)

  migration_succeeded <- suppressMessages(
    migrate_clif_2_1_to_3_0(input_directory, output_directory)
  )
  expect_true(migration_succeeded)
  expect_true(file.exists(file.path(output_directory, "clif_patient.parquet")))
})

test_that("re-running skips tables whose output already exists", {
  input_directory <- file.path(tempfile("clif21_"))
  output_directory <- file.path(tempfile("clif30_"))
  write_synthetic_clif_21(input_directory)

  runner <- suppressMessages(CrosswalkMigrationRunner$new(
    data_dir = input_directory, output_dir = output_directory
  ))
  suppressMessages(runner$run())
  # Second run: every beta output already exists, so all are skipped, none fail.
  expect_true(suppressMessages(runner$run()))
})

test_that("tz_status flags a real zone change but not a relabel to UTC", {
  # A tz-aware column relabeled to UTC is instant-preserving: not a concern.
  relabel <- tz_status(list(in_dttm = "America/New_York"), list(in_dttm = "UTC"))
  expect_false(relabel$is_concern)

  # Identical maps: match.
  unchanged <- tz_status(list(in_dttm = "UTC"), list(in_dttm = "UTC"))
  expect_false(unchanged$is_concern)
  expect_equal(unchanged$description, "match")

  # A genuine zone shift is a concern.
  shifted <- tz_status(list(in_dttm = "America/New_York"), list(in_dttm = "America/Chicago"))
  expect_true(shifted$is_concern)

  # A tz-aware column becoming naive is a concern.
  became_naive <- tz_status(list(in_dttm = "UTC"), list(in_dttm = NA_character_))
  expect_true(became_naive$is_concern)
})

test_that("zones summarizes timezone maps compactly", {
  expect_equal(zones(list()), "-")
  expect_equal(zones(list(a = "UTC", b = "UTC")), "UTC")
  expect_equal(zones(list(a = "UTC", b = NA_character_)), "UTC,naive")
})
