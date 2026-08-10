test_that("find_clif_table_file locates exact table_name.filetype", {
  tmp_dir <- withr::local_tempdir()
  file.create(file.path(tmp_dir, "patient.csv"))

  found <- find_clif_table_file(tmp_dir, "patient", "csv")

  expect_equal(found, file.path(tmp_dir, "patient.csv"))
})

test_that("find_clif_table_file locates clif_-prefixed files", {
  tmp_dir <- withr::local_tempdir()
  file.create(file.path(tmp_dir, "clif_vitals.parquet"))

  found <- find_clif_table_file(tmp_dir, "vitals", "parquet")

  expect_equal(found, file.path(tmp_dir, "clif_vitals.parquet"))
})

test_that("find_clif_table_file prefers exact match over clif_-prefixed", {
  tmp_dir <- withr::local_tempdir()
  file.create(file.path(tmp_dir, "labs.csv"))
  file.create(file.path(tmp_dir, "clif_labs.csv"))

  found <- find_clif_table_file(tmp_dir, "labs", "csv")

  expect_equal(found, file.path(tmp_dir, "labs.csv"))
})

test_that("find_clif_table_file matches case-insensitively as a fallback", {
  tmp_dir <- withr::local_tempdir()
  file.create(file.path(tmp_dir, "Patient.CSV"))

  found <- find_clif_table_file(tmp_dir, "patient", "csv")

  expect_equal(found, file.path(tmp_dir, "Patient.CSV"))
})

test_that("find_clif_table_file returns NA when no file matches", {
  tmp_dir <- withr::local_tempdir()

  found <- find_clif_table_file(tmp_dir, "adt", "parquet")

  expect_true(is.na(found))
})
