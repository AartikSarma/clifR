# MDRO flag parity: MDR/XDR/PDR/DTR flags for P. aeruginosa on the fixture
# microbiology tables. Flags and group columns are integer 0/1 and must match
# clifpy exactly; the per-agent susceptibility strings must match verbatim.

test_that("calculate_mdro_flags matches clifpy on the fixture cohort", {
  skip_if_no_parity_fixture("mdro_pseudomonas")

  python_flags <- read_parity_baseline("mdro_pseudomonas")

  culture <- parity_table("microbiology_culture")
  susceptibility <- parity_table("microbiology_susceptibility")

  r_flags <- calculate_mdro_flags(
    culture = culture,
    susceptibility = susceptibility,
    organism_name = "pseudomonas_aeruginosa"
  )

  # Column set and order should already agree; compare values exactly.
  expect_setequal(names(r_flags), names(python_flags))
  expect_parity(
    r_flags,
    python_flags,
    sort_columns = c("hospitalization_id", "organism_id"),
    tolerance = PARITY_TOLERANCE_EXACT,
    label = "mdro_pseudomonas"
  )
})

test_that("calculate_mdro_flags errors on an unknown organism", {
  skip_if_no_parity_fixture()

  culture <- parity_table("microbiology_culture")
  susceptibility <- parity_table("microbiology_susceptibility")

  expect_error(
    calculate_mdro_flags(culture, susceptibility, organism_name = "not_an_organism"),
    "not found in configuration"
  )
})
