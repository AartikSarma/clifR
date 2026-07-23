"""Build a deterministic CLIF cohort subset for cross-language parity testing.

Samples a fixed set of hospitalization_ids from a full CLIF data directory and
writes every table filtered to that cohort. Both clifpy and clifR run against
the resulting fixture directory, so any difference in results is attributable to
the implementations rather than to differing inputs.

Usage:
    uv run python build_cohort.py --source <dir> --dest <dir> --n 200
"""

from __future__ import annotations

import argparse
import glob
import json
import os

import duckdb

# Tables keyed by hospitalization_id are filtered directly. Tables keyed only by
# patient_id are filtered via the patient_ids of the sampled hospitalizations.
# Tables keyed by organism_id are filtered via microbiology_culture.
PATIENT_KEYED_TABLES = {"patient", "code_status"}
ORGANISM_KEYED_TABLES = {"microbiology_susceptibility"}


def table_name_from_path(parquet_path: str) -> str:
    basename = os.path.basename(parquet_path)
    stem = basename[: -len(".parquet")]
    return stem[len("clif_") :] if stem.startswith("clif_") else stem


def build_cohort(source_directory: str, dest_directory: str, n_hospitalizations: int, seed: int) -> dict:
    os.makedirs(dest_directory, exist_ok=True)
    connection = duckdb.connect()
    connection.execute(f"SELECT setseed({seed / 100.0})")

    source_files = sorted(glob.glob(os.path.join(source_directory, "*.parquet")))
    available_tables = {table_name_from_path(path): path for path in source_files}

    if "hospitalization" not in available_tables:
        raise FileNotFoundError(f"No hospitalization table found in {source_directory}")

    # Deterministic cohort: order by hospitalization_id and take the first N.
    # Ordering (not random sampling) keeps the cohort stable across duckdb versions.
    hospitalization_path = available_tables["hospitalization"]
    connection.execute(
        f"""
        CREATE TABLE cohort_hospitalizations AS
        SELECT * FROM read_parquet('{hospitalization_path}')
        WHERE hospitalization_id IN (
            SELECT hospitalization_id
            FROM read_parquet('{hospitalization_path}')
            ORDER BY hospitalization_id
            LIMIT {n_hospitalizations}
        )
        """
    )
    cohort_hospitalization_count = connection.execute(
        "SELECT COUNT(*) FROM cohort_hospitalizations"
    ).fetchone()[0]

    connection.execute(
        """
        CREATE TABLE cohort_patients AS
        SELECT DISTINCT patient_id FROM cohort_hospitalizations
        """
    )

    if "microbiology_culture" in available_tables:
        culture_path = available_tables["microbiology_culture"]
        connection.execute(
            f"""
            CREATE TABLE cohort_organisms AS
            SELECT DISTINCT organism_id
            FROM read_parquet('{culture_path}')
            WHERE hospitalization_id IN (SELECT hospitalization_id FROM cohort_hospitalizations)
              AND organism_id IS NOT NULL
            """
        )

    written_tables = {}
    skipped_tables = {}

    for table_name, source_path in available_tables.items():
        column_names = [
            row[0]
            for row in connection.execute(
                f"DESCRIBE SELECT * FROM read_parquet('{source_path}') LIMIT 0"
            ).fetchall()
        ]
        dest_path = os.path.join(dest_directory, f"clif_{table_name}.parquet")

        if table_name == "hospitalization":
            select_sql = "SELECT * FROM cohort_hospitalizations"
        elif "hospitalization_id" in column_names and table_name not in PATIENT_KEYED_TABLES:
            select_sql = f"""
                SELECT * FROM read_parquet('{source_path}')
                WHERE hospitalization_id IN (SELECT hospitalization_id FROM cohort_hospitalizations)
            """
        elif "patient_id" in column_names:
            select_sql = f"""
                SELECT * FROM read_parquet('{source_path}')
                WHERE patient_id IN (SELECT patient_id FROM cohort_patients)
            """
        elif table_name in ORGANISM_KEYED_TABLES and "organism_id" in column_names:
            select_sql = f"""
                SELECT * FROM read_parquet('{source_path}')
                WHERE organism_id IN (SELECT organism_id FROM cohort_organisms)
            """
        else:
            skipped_tables[table_name] = "no recognized cohort key column"
            continue

        connection.execute(
            f"COPY ({select_sql}) TO '{dest_path}' (FORMAT PARQUET, COMPRESSION ZSTD)"
        )
        written_tables[table_name] = connection.execute(
            f"SELECT COUNT(*) FROM read_parquet('{dest_path}')"
        ).fetchone()[0]

    manifest = {
        "source_directory": os.path.abspath(source_directory),
        "n_hospitalizations_requested": n_hospitalizations,
        "n_hospitalizations_written": cohort_hospitalization_count,
        "selection": "first N hospitalization_ids in lexicographic order",
        "row_counts": dict(sorted(written_tables.items())),
        "skipped_tables": skipped_tables,
    }
    with open(os.path.join(dest_directory, "cohort_manifest.json"), "w") as manifest_file:
        json.dump(manifest, manifest_file, indent=2)

    connection.close()
    return manifest


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source",
        default=os.path.expanduser("~/Research/synthetic_clif/synth_clif_10k"),
        help="Directory of full-size clif_*.parquet files",
    )
    parser.add_argument(
        "--dest",
        default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "fixtures", "cohort"),
        help="Directory to write the cohort subset into",
    )
    parser.add_argument("--n", type=int, default=200, help="Number of hospitalizations to include")
    parser.add_argument("--seed", type=int, default=42, help="Seed for reproducibility")
    arguments = parser.parse_args()

    manifest = build_cohort(arguments.source, os.path.abspath(arguments.dest), arguments.n, arguments.seed)
    print(json.dumps(manifest, indent=2))


if __name__ == "__main__":
    main()
