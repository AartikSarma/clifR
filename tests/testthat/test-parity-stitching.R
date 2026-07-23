# Encounter stitching parity: the mapping from hospitalization_id to encounter_block
# must be identical, since every downstream encounter-level analysis is grouped by it.

test_that("stitch_encounters reproduces clifpy's encounter mapping", {
  skip_if_no_parity_fixture("encounter_mapping")

  hospitalization <- parity_table("hospitalization")
  adt <- parity_table("adt")

  stitched <- stitch_encounters(hospitalization$df, adt$df, time_interval = 6)
  python_mapping <- read_parity_baseline("encounter_mapping")

  expect_named(stitched, c("hospitalization", "adt", "encounter_mapping"), ignore.order = TRUE)

  expect_parity(
    stitched$encounter_mapping,
    python_mapping,
    sort_columns = "hospitalization_id",
    tolerance = PARITY_TOLERANCE_EXACT,
    label = "encounter_mapping"
  )
})

test_that("encounter blocks partition hospitalizations without loss", {
  skip_if_no_parity_fixture("encounter_mapping")

  hospitalization <- parity_table("hospitalization")
  adt <- parity_table("adt")
  stitched <- stitch_encounters(hospitalization$df, adt$df, time_interval = 6)

  # Every hospitalization must land in exactly one block. A duplicated id would
  # silently multiply rows in any downstream join.
  expect_equal(
    nrow(stitched$encounter_mapping),
    dplyr::n_distinct(stitched$encounter_mapping$hospitalization_id)
  )
  expect_setequal(
    stitched$encounter_mapping$hospitalization_id,
    unique(hospitalization$df$hospitalization_id)
  )
})
