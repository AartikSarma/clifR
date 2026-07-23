"""Generate the clifpy CDC Adult Sepsis Event (ASE) baseline for clifR parity.

Runs ``clifpy.utils.ase.compute_ase`` on the shared cohort fixture and writes a
deterministic parquet artifact into ``tests/baseline/`` (plus a companion schema
JSON) so ``tests/testthat/test-parity-ase.R`` can compare row-for-row.

Usage:
    cd tests/baseline_py && uv run python generate_ase_baseline.py
"""

from __future__ import annotations

import json
import os
import warnings

import pandas as pd

warnings.filterwarnings("ignore")

HERE = os.path.dirname(os.path.abspath(__file__))
DATA_DIRECTORY = os.path.join(HERE, "..", "fixtures", "cohort")
BASELINE_DIRECTORY = os.path.join(HERE, "..", "baseline")

# Sort key: one row per (hospitalization_id, bc_id). bc_id is <NA> for the
# no-blood-culture rows, so it is coerced to a sortable string first.
SORT_COLUMNS = ["hospitalization_id", "bc_id"]


def write_frame(frame: pd.DataFrame, artifact_name: str) -> str:
    """Write a DataFrame as parquet with deterministic row and column order."""
    frame = frame.copy()

    # Stable row order. bc_id / episode_id are nullable ints; sort on a string
    # rendering so NA sorts consistently (as the empty string) across engines.
    sort_frame = frame.copy()
    for column in SORT_COLUMNS:
        sort_frame[column + "__sort"] = (
            sort_frame[column].astype("string").fillna("")
        )
    order = sort_frame.sort_values(
        [c + "__sort" for c in SORT_COLUMNS], kind="mergesort"
    ).index
    frame = frame.loc[order].reset_index(drop=True)

    frame = frame[sorted(frame.columns)]

    output_path = os.path.join(BASELINE_DIRECTORY, f"{artifact_name}.parquet")
    frame.to_parquet(output_path, index=False)

    schema_path = os.path.join(BASELINE_DIRECTORY, f"{artifact_name}_schema.json")
    with open(schema_path, "w") as schema_file:
        json.dump(
            {
                "artifact": artifact_name,
                "n_rows": int(len(frame)),
                "n_columns": int(len(frame.columns)),
                "columns": list(frame.columns),
                "dtypes": {
                    column: str(dtype) for column, dtype in frame.dtypes.items()
                },
            },
            schema_file,
            indent=2,
            sort_keys=True,
        )
    return output_path


def main() -> None:
    from clifpy.utils.ase import compute_ase

    os.makedirs(BASELINE_DIRECTORY, exist_ok=True)

    ase = compute_ase(
        hospitalization_ids=None,
        data_directory=DATA_DIRECTORY,
        filetype="parquet",
        timezone="UTC",
        apply_rit=True,
        rit_only_hospital_onset=True,
        include_lactate=False,
        verbose=True,
    )

    path = write_frame(ase, "ase")
    n_sepsis = int((ase["sepsis"] == 1).sum())
    print(f"ASE baseline written to {os.path.abspath(path)}")
    print(f"  rows: {len(ase)}  sepsis==1: {n_sepsis}")


if __name__ == "__main__":
    main()
