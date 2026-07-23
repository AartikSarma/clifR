# clifR porting conventions

clifR is a port of [clifpy](https://github.com/Common-Longitudinal-ICU-data-Format/clifpy)
**v0.5.0**. The goal is behavioural parity: for the same input data, clifR and clifpy
must produce the same results, verified by the cross-language test suite.

A reference checkout of clifpy 0.5.0 lives in the scratchpad for this work; the
authoritative source is the upstream repository at tag/commit `6b9360d`.

## Parity is the requirement, not idiomatic R

Where clifpy uses DuckDB, **port the SQL, not the intent**. clifpy's wide-dataset
builder, unit converter, waterfall and ASE modules are DuckDB-SQL pipelines; running
the same SQL through R's `duckdb` package is the single most reliable way to get
identical numbers. Rewriting those pipelines in dplyr introduces differences in join
ordering, null handling and floating-point accumulation that are painful to chase down.

Where clifpy uses pandas/polars for genuinely elementwise work, dplyr is fine —
but preserve the *order of operations*, because rounding and fill semantics depend on it.

## Naming

- Exported function names match clifpy's Python names exactly: `create_wide_dataset`,
  `convert_wide_to_hourly`, `compute_sofa`, `calculate_cci`, `calculate_elix`,
  `stitch_encounters`, `apply_outlier_handling`, `process_resp_support_waterfall`.
- Class names match: `Patient`, `RespiratorySupport`, `MedicationAdminContinuous`.
- Method names on R6 classes match Python method names: `validate()`, `isvalid()`,
  `get_summary()`, `save_summary()`, `analyze_categorical_distributions()`.
- **Local variables use descriptive names** (`hospitalization_identifiers`, not `hids`),
  per the project style, even when the Python original is terse.

## Foundation already available

Defined in `R/schemas.R`, `R/util_io.R`, `R/table_registry.R`, `R/base_table.R`:

| Function | Purpose |
|---|---|
| `load_schema(table_name, clif_version)` | Parsed table schema, or `NULL` |
| `schema_path()`, `schema_dir()`, `resolve_schema_filename()` | Schema path resolution |
| `load_shared_config(name)` | `outlier_config.yaml`, `wide_tables_config.yaml`, `validation_rules.yaml` |
| `clif_extdata_path(...)` | Resources in `inst/extdata` (`mdro.yaml`, `comorbidity/*.yaml`) |
| `load_data(table_name, table_path, table_format_type, ...)` | DuckDB-backed table loader |
| `duckdb_connect()` | DuckDB connection with clifpy's session settings (`timezone = 'UTC'`) |
| `sql_quote_value(value)` | SQL string-literal escaping |
| `cast_id_cols_to_string(data)` | `*_id` columns to character |
| `convert_datetime_columns_to_site_tz(data, tz, verbose)` | `dttm` column timezone handling |
| `BaseTable` | R6 base class; subclasses override `private$run_table_specific_validations()` |
| `get_table_class(name)`, `CLIF_TABLE_NAMES` | Table registry |
| `%||%` | Null-coalescing operator |
| `describe_numeric_column(values)` | pandas-`describe()`-compatible numeric summary |

Constants: `DEFAULT_CLIF_VERSION` (`"2.1"`), `SUPPORTED_CLIF_VERSIONS` (`"2.1"`, `"3.0"`).

> **Schema source of truth**: the YAML resources under `inst/schemas/` and
> `inst/extdata/` must match the **released** clifpy version the baselines are
> generated against (pip `clifpy==0.5.0`), not an arbitrary git checkout. Git HEAD
> can drift from the release — e.g. HEAD marks `adt.room_id` required while 0.5.0
> does not — and any such drift shows up as a spurious validation parity failure.
> When upgrading clifpy, re-copy the schemas from the installed package
> (`tests/baseline_py/.venv/.../clifpy/schemas` and `.../clifpy/data`).

Schema and config resources are copied verbatim from clifpy into `inst/schemas/`
(`2.1/`, `3.0/`, `crosswalks/`, plus the three root configs) and `inst/extdata/`
(`mdro.yaml`, `comorbidity/cci.yaml`, `comorbidity/elixhauser.yaml`). **Do not
hand-write these values into R code** — read them from the YAML, as clifpy does.
A hardcoded R copy will silently drift from the Python one.

## Style

- tidyverse throughout; native pipe `|>`; `.data[[col]]` for tidy-eval column references.
- roxygen2 on every exported function, with `@param`, `@return`, and a `@examples`
  block (wrap file-reading examples in `\dontrun{}`).
- No `library()` calls in `R/`; use `pkg::fun()` and declare the dependency in DESCRIPTION.
- Errors and messages via `cli::cli_abort()` / `cli::cli_alert_*()`.
- Plots use viridis for continuous scales and Okabe-Ito for discrete ones.

## Return types

- Functions clifpy declares as returning a DataFrame return a **tibble**.
- Functions returning a dict return a **named list**.
- Functions returning a tuple return a **named list** with descriptively named elements
  (e.g. `list(hospitalization = ..., adt = ..., encounter_mapping = ...)`), and the
  docs must say what each element is.
- Preserve clifpy's **column names and column order** exactly. The parity tests compare
  column sets, and divergent names are the most common cause of a failing comparison.

## Numeric parity notes

- Percentiles: pandas' `describe()` uses linear interpolation, matching R's
  `quantile(type = 7)` default. Don't change the type.
- Integer-valued clinical scores (SOFA components, CCI, Elixhauser) must match **exactly**;
  there is no tolerance budget for them.
- Floating-point results are compared at `1e-12` unless the test says otherwise.
- Timestamps: clifpy stores timezone-aware datetimes per column; R's POSIXct carries a
  single `tzone` attribute per vector. Convert with `attr(x, "tzone") <- tz`, which
  relabels the display zone without shifting the underlying instant.

## Testing

Every ported module needs a matching pair:

1. A generator in `tests/baseline_py/generate_baselines.py` that runs the clifpy
   function on the shared fixture and writes a deterministic artifact into
   `tests/baseline/`.
2. A `tests/testthat/test-parity-<module>.R` that runs the clifR function on the same
   fixture and compares against that artifact.

The shared fixture is `tests/fixtures/cohort/` — 200 hospitalizations sliced from
`~/Research/synthetic_clif/synth_clif_10k` by `tests/baseline_py/build_cohort.py`.
It is CLIF **3.0** data, so pass `clif_version = "3.0"` when loading it.

Python tooling is managed with **uv** (`cd tests/baseline_py && uv run ...`). Never
use pip, poetry or conda.
