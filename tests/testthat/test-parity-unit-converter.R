# Medication dose unit conversion parity. Doses drive the SOFA cardiovascular
# component and any vasopressor analysis, so converted values are compared at strict
# floating point tolerance and the per-unit conversion bookkeeping is compared too.

parity_preferred_units <- list(
  norepinephrine = "mcg/kg/min",
  epinephrine = "mcg/kg/min",
  phenylephrine = "mcg/kg/min",
  vasopressin = "u/min",
  dopamine = "mcg/kg/min",
  dobutamine = "mcg/kg/min",
  propofol = "mcg/kg/min",
  fentanyl = "mcg/hr"
)

test_that("convert_dose_units_by_med_category matches clifpy dose for dose", {
  skip_if_no_parity_fixture("dose_conversion")

  medications <- parity_table("medication_admin_continuous")
  vitals <- parity_table("vitals")

  conversion_result <- convert_dose_units_by_med_category(
    med_df = medications$df,
    vitals_df = vitals$df,
    preferred_units = parity_preferred_units,
    show_intermediate = TRUE,
    override = TRUE
  )

  python_converted <- read_parity_baseline("dose_conversion")

  expect_parity(
    conversion_result$converted,
    python_converted,
    sort_columns = c("hospitalization_id", "med_order_id", "admin_dttm"),
    tolerance = PARITY_TOLERANCE_STRICT,
    label = "dose_conversion"
  )
})

test_that("dose unit conversion counts match clifpy", {
  skip_if_no_parity_fixture("dose_conversion_counts")

  medications <- parity_table("medication_admin_continuous")
  vitals <- parity_table("vitals")

  conversion_result <- convert_dose_units_by_med_category(
    med_df = medications$df,
    vitals_df = vitals$df,
    preferred_units = parity_preferred_units,
    show_intermediate = TRUE,
    override = TRUE
  )

  python_counts <- read_parity_baseline("dose_conversion_counts")

  expect_parity(
    conversion_result$counts,
    python_counts,
    sort_columns = c("med_category", "med_dose_unit"),
    tolerance = PARITY_TOLERANCE_EXACT,
    label = "dose_conversion_counts"
  )
})

test_that("an unacceptable preferred unit is rejected unless overridden", {
  skip_if_no_parity_fixture()

  medications <- parity_table("medication_admin_continuous")
  vitals <- parity_table("vitals")

  # "furlongs/fortnight" is not in ALL_ACCEPTABLE_UNITS, so clifpy raises rather
  # than silently emitting unconverted doses. clifR must do the same.
  expect_error(
    convert_dose_units_by_med_category(
      med_df = medications$df,
      vitals_df = vitals$df,
      preferred_units = list(norepinephrine = "furlongs/fortnight"),
      override = FALSE
    )
  )
})
