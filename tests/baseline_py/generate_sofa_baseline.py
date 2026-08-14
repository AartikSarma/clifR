"""Generate clifpy SOFA baselines for the clifR parity tests.

Writes, into ``tests/baseline/``:

* ``sofa_cohort.parquet``          - full-stay cohort windows (one row per hospitalization)
* ``sofa_cohort_48h.parquet``      - 48-hour windows from admission
* ``sofa_cohort_blocks.parquet``   - 48-hour windows plus an ``encounter_block`` grouping
* ``sofa_wide_full_input.parquet`` - the wide dataset the ``compute_sofa`` variants consume
* ``sofa_wide_*.parquet``          - ``clifpy.utils.sofa.compute_sofa`` results
* ``sofa_direct_*.parquet``        - ``clifpy.compute_sofa_polars`` results

Usage:
    cd tests/baseline_py && uv run python generate_sofa_baseline.py
"""

from __future__ import annotations

import os
import warnings

import pandas as pd
import polars as pl

warnings.filterwarnings("ignore")

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIRECTORY = os.path.join(HERE, "..", "fixtures", "cohort")
BASELINE_DIRECTORY = os.path.join(HERE, "..", "baseline")
OUTPUT_DIRECTORY = os.path.join(HERE, "output")

MEDICATION_CATEGORIES = ["norepinephrine", "epinephrine", "dopamine", "dobutamine"]


def build_cohorts() -> dict[str, pd.DataFrame]:
    hospitalization = pd.read_parquet(os.path.join(DATA_DIRECTORY, "clif_hospitalization.parquet"))
    full_stay = (
        hospitalization[["hospitalization_id", "admission_dttm", "discharge_dttm"]]
        .rename(columns={"admission_dttm": "start_dttm", "discharge_dttm": "end_dttm"})
        .sort_values("hospitalization_id")
        .reset_index(drop=True)
    )

    first_48_hours = full_stay.copy()
    first_48_hours["end_dttm"] = first_48_hours["start_dttm"] + pd.Timedelta(hours=48)

    with_blocks = first_48_hours.copy()
    # Deterministic synthetic grouping: pair consecutive hospitalizations into blocks.
    with_blocks["encounter_block"] = (with_blocks.index // 2) + 1

    return {"full": full_stay, "48h": first_48_hours, "blocks": with_blocks}


def generate_wide_baselines(cohorts: dict[str, pd.DataFrame]) -> None:
    from clifpy import ClifOrchestrator
    from clifpy.utils.sofa import REQUIRED_SOFA_CATEGORIES_BY_TABLE, compute_sofa

    orchestrator = ClifOrchestrator(
        data_directory=DATA_DIRECTORY,
        filetype="parquet",
        timezone="UTC",
        output_directory=OUTPUT_DIRECTORY,
    )
    # Load vitals in full first: the dose-unit conversion would otherwise leave a
    # weight-only vitals table cached, and the wide dataset would lose map/spo2.
    orchestrator.load_table("vitals")
    orchestrator.convert_dose_units_for_continuous_meds(
        preferred_units={category: "mcg/kg/min" for category in MEDICATION_CATEGORIES}
    )
    orchestrator.create_wide_dataset(
        tables_to_load=list(REQUIRED_SOFA_CATEGORIES_BY_TABLE.keys()),
        category_filters=REQUIRED_SOFA_CATEGORIES_BY_TABLE,
        return_dataframe=True,
        show_progress=False,
    )
    wide_df = orchestrator.wide_df
    # Deliberately a different artifact from generate_baselines.py's
    # ``sofa_wide_input``: that one is built without pre-loading vitals, which
    # leaves map and spo2 entirely null, so the cardiovascular component and the
    # SpO2 imputation path would go untested.
    wide_df.to_parquet(
        os.path.join(BASELINE_DIRECTORY, "sofa_wide_full_input.parquet"), index=False
    )

    variants = {
        "default": dict(),
        "nofill": dict(fill_na_scores_with_zero=False),
        "keep_outliers": dict(remove_outliers=False),
    }
    for name, keyword_arguments in variants.items():
        scores = compute_sofa(wide_df, id_name="hospitalization_id", **keyword_arguments)
        scores.to_parquet(
            os.path.join(BASELINE_DIRECTORY, f"sofa_wide_{name}.parquet"), index=False
        )

    # Time-window filtering: compute_sofa's cohort_df uses start_time/end_time.
    window_cohort = cohorts["48h"].rename(
        columns={"start_dttm": "start_time", "end_dttm": "end_time"}
    )
    scores = compute_sofa(wide_df, cohort_df=window_cohort, id_name="hospitalization_id")
    scores.to_parquet(os.path.join(BASELINE_DIRECTORY, "sofa_wide_cohort48h.parquet"), index=False)


def generate_direct_baselines(cohorts: dict[str, pd.DataFrame]) -> None:
    from clifpy import compute_sofa_polars

    for name, cohort in cohorts.items():
        cohort.to_parquet(
            os.path.join(BASELINE_DIRECTORY, f"sofa_cohort_{name}.parquet"), index=False
        )

    runs = {
        "full": dict(cohort="full"),
        "48h": dict(cohort="48h"),
        "nofill": dict(cohort="48h", fill_na_scores_with_zero=False, remove_outliers=False),
        "blocks": dict(cohort="blocks", id_name="encounter_block"),
    }
    for name, arguments in runs.items():
        cohort = pl.from_pandas(cohorts[arguments.pop("cohort")])
        scores = compute_sofa_polars(
            DATA_DIRECTORY, cohort, filetype="parquet", timezone="UTC", **arguments
        )
        scores.write_parquet(os.path.join(BASELINE_DIRECTORY, f"sofa_direct_{name}.parquet"))


def main() -> None:
    os.makedirs(BASELINE_DIRECTORY, exist_ok=True)
    os.makedirs(OUTPUT_DIRECTORY, exist_ok=True)
    cohorts = build_cohorts()
    generate_direct_baselines(cohorts)
    generate_wide_baselines(cohorts)
    print("SOFA baselines written to", os.path.abspath(BASELINE_DIRECTORY))


if __name__ == "__main__":
    main()
