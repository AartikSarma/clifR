# clifR test suite

clifR is a port of [clifpy](https://github.com/Common-Longitudinal-ICU-data-Format/clifpy)
0.5.0. The suite's purpose is to prove the port is faithful: for the same input data,
clifR and clifpy must produce the same results.

## How parity testing works

```
    ~/Research/synthetic_clif/synth_clif_10k   (full 10k-patient CLIF 3.0 dataset)
                     |
                     |  build_cohort.py  — deterministic 200-hospitalization slice
                     v
          tests/fixtures/cohort/               (the shared input, version controlled)
                    / \
                   /   \
    clifpy 0.5.0  /     \  clifR
                 v       v
   tests/baseline/    in-memory result
        (artifacts)         |
                  \        /
                   v      v
              tests/testthat/test-parity-*.R   — compare
```

Both implementations read the **same fixture files**, so any difference in output is
attributable to the implementations rather than to differing inputs. The fixture and
the baseline artifacts are both committed, so the R tests run on a fresh clone with no
Python setup at all. Python is only needed to *regenerate* baselines.

## Running the R tests

```r
devtools::test()                                  # everything
testthat::test_file("tests/testthat/test-parity-sofa.R")   # one component
```

Tests skip rather than fail when the fixture or a baseline artifact is missing, so
`R CMD check` stays green in environments without them.

## Regenerating the Python baselines

Python tooling is managed with [uv](https://docs.astral.sh/uv/). The environment is
pinned in `tests/baseline_py/pyproject.toml` and `uv.lock`.

```bash
cd tests/baseline_py
uv sync                                      # install clifpy 0.5.0 and dependencies
uv run python generate_baselines.py          # all components
uv run python generate_baselines.py --list   # list component names
uv run python generate_baselines.py --only sofa cci   # regenerate a subset
```

Each run writes `tests/baseline/_baseline_manifest.json` recording the clifpy version,
the Python version, and the status of every component. Regenerate baselines only when
upgrading clifpy — a baseline change means the reference moved, and the diff should be
reviewed as carefully as a source change.

## Rebuilding the fixture cohort

```bash
cd tests/baseline_py
uv run python build_cohort.py --n 200
```

The cohort is the first N `hospitalization_id` values in lexicographic order, not a
random sample, so it is stable across machines and DuckDB versions. All 28 tables are
sliced consistently: hospitalization-keyed tables filter on the cohort's
hospitalizations, patient-keyed tables on the corresponding patients, and
`microbiology_susceptibility` on the organisms found in those patients' cultures.
`cohort_manifest.json` records the resulting row counts.

Changing `--n` invalidates every baseline; regenerate them together.

## Tolerances

| Result kind | Tolerance | Why |
|---|---|---|
| Clinical scores (SOFA components, CCI, Elixhauser) | exact | Integers on a fixed scale; a one-point difference is a real disagreement |
| IDs, encounter blocks, categorical values, counts | exact | Discrete and unambiguous |
| Converted doses, wide dataset values, lab and vital values | `1e-12` | Floating point accumulation order can differ slightly |
| Derived ratios (PaO2/FiO2), aggregate statistics | `1e-6` | Compounded floating point across several operations |

## Fixture characteristics

CLIF **3.0** data, timezone `US/Central`, 200 hospitalizations across 157 patients.
Load it with `clif_version = "3.0"`.

The synthetic data intentionally carries non-normalized category values (`"Black or
African American"` rather than `black_or_african_american`), so the validation tests
exercise real findings rather than a clean-data path.

## Known upstream issues

- `clifpy.utils.outlier_handler.get_outlier_summary()` raises `AttributeError` in
  0.5.0: it calls `validator.validate_numeric_ranges_from_config()`, which no longer
  exists in that module. Only the working `apply_outlier_handling()` path is
  baselined. clifR implements `get_outlier_summary()` correctly, so that function has
  no Python counterpart to compare against until the upstream bug is fixed.
- `clifpy.tables.Labs.get_lab_specimen_stats()` guards on a misspelled column name
  (`lab_speciment_category`) while grouping by the correct one. clifR implements the
  correct spelling; see the `@note` on that method.
- `compute_sofa()` requires vasopressor columns to already carry converted units
  (`norepinephrine_mcg_kg_min` and friends) and raises a DuckDB binder error
  otherwise, so dose conversion must run before the wide dataset is built.
