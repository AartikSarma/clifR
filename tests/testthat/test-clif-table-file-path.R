test_that("clif_table_file_path builds the clif_-prefixed path", {
  expect_equal(
    clif_table_file_path("data/clif", "patient", "parquet"),
    file.path("data/clif", "clif_patient.parquet")
  )
  expect_equal(
    clif_table_file_path("data/clif", "medication_admin_continuous", "csv"),
    file.path("data/clif", "clif_medication_admin_continuous.csv")
  )
})

test_that("clif_table_file_path does not require the file to exist", {
  temporary_directory <- withr::local_tempdir()

  expect_equal(
    clif_table_file_path(temporary_directory, "adt", "parquet"),
    file.path(temporary_directory, "clif_adt.parquet")
  )
})

test_that("load_data reads a clif_-prefixed file", {
  temporary_directory <- withr::local_tempdir()
  utils::write.csv(
    data.frame(patient_id = c("1", "2"), sex_category = c("Female", "Male")),
    file.path(temporary_directory, "clif_patient.csv"),
    row.names = FALSE
  )

  loaded <- load_data("patient", temporary_directory, "csv")

  expect_equal(nrow(loaded), 2)
  expect_equal(loaded$patient_id, c("1", "2"))
})

test_that("load_data rejects a bare, unprefixed filename", {
  temporary_directory <- withr::local_tempdir()
  utils::write.csv(
    data.frame(patient_id = "1"),
    file.path(temporary_directory, "patient.csv"),
    row.names = FALSE
  )

  expect_error(
    load_data("patient", temporary_directory, "csv"),
    "clif_patient\\.csv"
  )
})

test_that("load_data reports a missing data directory separately", {
  missing_directory <- file.path(withr::local_tempdir(), "not-here")

  expect_error(
    load_data("patient", missing_directory, "csv"),
    "does not exist"
  )
})

test_that("BaseTable loads a clif_-prefixed file and errors clearly without one", {
  temporary_directory <- withr::local_tempdir()
  utils::write.csv(
    data.frame(patient_id = "1"),
    file.path(temporary_directory, "clif_patient.csv"),
    row.names = FALSE
  )

  table_object <- BaseTable$new(
    table_name = "patient",
    data_directory = temporary_directory,
    filetype = "csv",
    timezone = "UTC"
  )
  expect_equal(nrow(table_object$df), 1)

  empty_directory <- withr::local_tempdir()
  expect_error(
    BaseTable$new(
      table_name = "patient",
      data_directory = empty_directory,
      filetype = "csv"
    ),
    "No file found for table"
  )
})

# The symptom reported in issue #3: a correct data_directory produced five
# "not found, skipping" lines instead of loading anything.
test_that("ClifOrchestrator$initialize_tables loads clif_-prefixed files", {
  temporary_directory <- withr::local_tempdir()
  utils::write.csv(
    data.frame(patient_id = "1"),
    file.path(temporary_directory, "clif_patient.csv"),
    row.names = FALSE
  )

  orchestrator <- ClifOrchestrator$new(
    data_directory = temporary_directory,
    filetype = "csv",
    timezone = "UTC"
  )
  orchestrator$initialize_tables(tables = "patient")

  expect_false(is.null(orchestrator$patient))
  expect_equal(nrow(orchestrator$patient$df), 1)
})

test_that("ClifOrchestrator$initialize_tables errors on a missing data directory", {
  orchestrator <- ClifOrchestrator$new(
    data_directory = file.path(withr::local_tempdir(), "not-here"),
    filetype = "csv",
    timezone = "UTC"
  )

  expect_error(orchestrator$initialize_tables(), "Data directory not found")
})

test_that("initialize_tables defaults to patient alone", {
  temporary_directory <- withr::local_tempdir()
  for (table_name in c("patient", "vitals")) {
    utils::write.csv(
      data.frame(hospitalization_id = "1"),
      file.path(temporary_directory, paste0("clif_", table_name, ".csv")),
      row.names = FALSE
    )
  }

  orchestrator <- ClifOrchestrator$new(
    data_directory = temporary_directory,
    filetype = "csv",
    timezone = "UTC"
  )
  orchestrator$initialize_tables()

  expect_equal(orchestrator$get_loaded_tables(), "patient")
})

test_that("load_all_tables skips tables with no file and errors when none match", {
  temporary_directory <- withr::local_tempdir()
  utils::write.csv(
    data.frame(patient_id = "1"),
    file.path(temporary_directory, "clif_patient.csv"),
    row.names = FALSE
  )

  loaded <- load_all_tables(temporary_directory, c("patient", "vitals"), filetype = "csv")
  expect_named(loaded, "patient")

  empty_directory <- withr::local_tempdir()
  expect_error(
    load_all_tables(empty_directory, "patient", filetype = "csv"),
    "No CLIF tables found"
  )
})
