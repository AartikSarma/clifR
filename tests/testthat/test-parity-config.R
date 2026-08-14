# Config parity: get_config_or_params implements the same precedence rules as
# clifpy (explicit params > config file > auto-detect; params override file
# values; extra keys such as clif_version pass through). These are behavioural
# contracts rather than numeric outputs, so they are asserted directly rather
# than against a baseline artifact.

test_that("direct parameters take priority and skip the config file", {
  temporary_directory <- withr::local_tempdir()
  data_directory <- file.path(temporary_directory, "data")
  dir.create(data_directory)

  # A config file that would win if it were consulted; it must not be.
  writeLines(
    jsonlite::toJSON(list(
      data_directory = data_directory, filetype = "csv",
      timezone = "US/Central", clif_version = "2.1"
    ), auto_unbox = TRUE),
    file.path(temporary_directory, "config.json")
  )

  withr::local_dir(temporary_directory)
  resolved <- get_config_or_params(
    data_directory = data_directory,
    filetype = "parquet",
    timezone = "UTC"
  )

  expect_equal(resolved$filetype, "parquet")
  expect_equal(resolved$timezone, "UTC")
  # Auto-detect was skipped, so the file-only key is absent.
  expect_null(resolved$clif_version)
})

test_that("a config file supplies values and params override individual keys", {
  temporary_directory <- withr::local_tempdir()
  data_directory <- file.path(temporary_directory, "data")
  dir.create(data_directory)

  config_path <- file.path(temporary_directory, "site.yaml")
  writeLines(c(
    "site: EXAMPLE",
    sprintf("tables_path: %s", data_directory),
    "filetype: csv",
    "timezone: US/Central",
    "clif_version: \"3.0\""
  ), config_path)

  resolved <- get_config_or_params(config_path = config_path, filetype = "parquet")

  # tables_path mapped to data_directory; clif_version passed through; the
  # explicit filetype overrode the file's value; timezone came from the file.
  expect_equal(resolved$data_directory, data_directory)
  expect_equal(resolved$filetype, "parquet")
  expect_equal(resolved$timezone, "US/Central")
  expect_equal(resolved$clif_version, "3.0")
})

test_that("incomplete params without a config file raise a helpful error", {
  empty_directory <- withr::local_tempdir()
  withr::local_dir(empty_directory)

  expect_error(
    get_config_or_params(data_directory = "."),
    "Incomplete parameters"
  )
})

test_that("load_config validates required fields, filetype and directory", {
  temporary_directory <- withr::local_tempdir()

  missing_field_path <- file.path(temporary_directory, "missing.json")
  writeLines('{"data_directory": ".", "filetype": "csv"}', missing_field_path)
  expect_error(load_config(missing_field_path), "Missing required fields")

  bad_filetype_path <- file.path(temporary_directory, "badtype.json")
  writeLines(
    sprintf('{"data_directory": "%s", "filetype": "xlsx", "timezone": "UTC"}', temporary_directory),
    bad_filetype_path
  )
  expect_error(load_config(bad_filetype_path), "Unsupported filetype")
})

test_that("create_example_config round-trips through load_config", {
  temporary_directory <- withr::local_tempdir()

  yaml_path <- file.path(temporary_directory, "config.yaml")
  create_example_config(data_directory = temporary_directory, config_path = yaml_path)
  yaml_config <- load_config(yaml_path)
  expect_equal(yaml_config$data_directory, temporary_directory)
  expect_equal(yaml_config$clif_version, DEFAULT_CLIF_VERSION)

  json_path <- file.path(temporary_directory, "config.json")
  create_example_config(data_directory = temporary_directory, config_path = json_path)
  json_config <- load_config(json_path)
  expect_equal(json_config$filetype, "parquet")
})
