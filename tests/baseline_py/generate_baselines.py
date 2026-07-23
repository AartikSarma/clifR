"""Generate clifpy reference outputs for cross-language parity testing.

Runs clifpy against the shared fixture cohort and writes one deterministic artifact
per component into ``tests/baseline/``. The R test suite reruns the equivalent clifR
function on the same fixture and compares against these files, so any difference is
attributable to the implementations rather than to differing inputs.

Every artifact is written with a stable row order and stable column order. Anything
that is not reproducible run to run (timestamps, absolute paths, memory figures) is
excluded, so re-running this script on unchanged inputs produces byte-identical files.

Usage:
    uv run python generate_baselines.py                 # all components
    uv run python generate_baselines.py --only sofa cci # a subset
    uv run python generate_baselines.py --list          # show component names
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import traceback

import pandas as pd

import clifpy
from clifpy.clif_orchestrator import ClifOrchestrator
from clifpy.utils import comorbidity, sofa, stitching_encounters, unit_converter
from clifpy.utils import outlier_handler, waterfall, crosswalk, wide_dataset
from clifpy.utils import query as query_utils
from clifpy.utils.mdro_flags import calculate_mdro_flags

HERE = os.path.dirname(os.path.abspath(__file__))
FIXTURE_DIRECTORY = os.path.abspath(os.path.join(HERE, "..", "fixtures", "cohort"))
BASELINE_DIRECTORY = os.path.abspath(os.path.join(HERE, "..", "baseline"))

# The fixture is CLIF 3.0 data; the site timezone is fixed so datetime handling is
# exercised rather than defaulting to UTC everywhere.
CLIF_VERSION = "3.0"
SITE_TIMEZONE = "US/Central"
FILETYPE = "parquet"

# Registry of baseline generators, populated by the @component decorator.
COMPONENT_REGISTRY: dict[str, callable] = {}


def component(name: str):
    """Register a function as a named baseline component."""

    def decorator(function):
        COMPONENT_REGISTRY[name] = function
        return function

    return decorator


# --------------------------------------------------------------------------------
# Serialization helpers
# --------------------------------------------------------------------------------

def to_pandas(frame):
    """Coerce whatever clifpy returned into a pandas DataFrame.

    clifpy's public functions variously return pandas DataFrames, polars DataFrames
    and raw DuckDB relations, so every artifact goes through this first.
    """
    if isinstance(frame, pd.DataFrame):
        return frame
    for conversion_method in ("fetchdf", "to_df", "df", "to_pandas"):
        method = getattr(frame, conversion_method, None)
        if callable(method):
            try:
                converted = method()
            except Exception:  # noqa: BLE001 - try the next conversion strategy
                continue
            if isinstance(converted, pd.DataFrame):
                return converted
    raise TypeError(f"Cannot convert {type(frame)!r} to a pandas DataFrame")


def write_frame(frame, artifact_name: str, sort_columns: list[str] | None = None) -> str:
    """Write a DataFrame as parquet with deterministic row and column order."""
    frame = to_pandas(frame).copy()

    if sort_columns:
        present_sort_columns = [column for column in sort_columns if column in frame.columns]
        if present_sort_columns:
            frame = frame.sort_values(present_sort_columns, kind="mergesort")

    frame = frame.reset_index(drop=True)
    frame = frame[sorted(frame.columns)]

    output_path = os.path.join(BASELINE_DIRECTORY, f"{artifact_name}.parquet")
    frame.to_parquet(output_path, index=False)

    # A companion JSON records the schema so the R side can assert on column names
    # and dtypes without having to infer them from the parquet file.
    schema_path = os.path.join(BASELINE_DIRECTORY, f"{artifact_name}_schema.json")
    with open(schema_path, "w") as schema_file:
        json.dump(
            {
                "artifact": artifact_name,
                "n_rows": int(len(frame)),
                "n_columns": int(len(frame.columns)),
                "columns": list(frame.columns),
                "dtypes": {column: str(dtype) for column, dtype in frame.dtypes.items()},
            },
            schema_file,
            indent=2,
            sort_keys=True,
        )
    return output_path


def write_json(payload, artifact_name: str) -> str:
    """Write a JSON artifact with sorted keys so diffs stay stable."""
    output_path = os.path.join(BASELINE_DIRECTORY, f"{artifact_name}.json")
    with open(output_path, "w") as output_file:
        json.dump(payload, output_file, indent=2, sort_keys=True, default=str)
    return output_path


def build_orchestrator(tables: list[str] | None = None) -> ClifOrchestrator:
    orchestrator = ClifOrchestrator(
        data_directory=FIXTURE_DIRECTORY,
        filetype=FILETYPE,
        timezone=SITE_TIMEZONE,
        output_directory=os.path.join(HERE, "output"),
    )
    if tables:
        orchestrator.initialize(tables=tables)
    return orchestrator


def load_table(table_name: str):
    """Load a single clifpy table object from the fixture at the fixture's CLIF version."""
    table_class = ClifOrchestrator.__init__.__globals__["TABLE_CLASSES"][table_name]
    return table_class.from_file(
        data_directory=FIXTURE_DIRECTORY,
        filetype=FILETYPE,
        timezone=SITE_TIMEZONE,
        output_directory=os.path.join(HERE, "output"),
        clif_version=CLIF_VERSION,
    )


# --------------------------------------------------------------------------------
# Components
# --------------------------------------------------------------------------------

@component("validation")
def generate_validation_baselines() -> list[str]:
    """Per-table validation errors, the output BaseTable.validate() produces."""
    written = []
    table_names = list(ClifOrchestrator.__init__.__globals__["TABLE_CLASSES"].keys())

    validation_summary = {}
    for table_name in table_names:
        try:
            table_object = load_table(table_name)
        except FileNotFoundError:
            validation_summary[table_name] = {"status": "table_absent"}
            continue

        table_object.validate()
        errors = table_object.errors or []

        # Error records carry nested detail dicts; keep them, but sort by a stable key
        # so the ordering does not depend on dict iteration order.
        normalized_errors = sorted(
            (
                {
                    "type": error.get("type"),
                    "description": error.get("description"),
                    "category": error.get("category"),
                    "severity": error.get("severity"),
                    "details": error.get("details"),
                }
                for error in errors
            ),
            key=lambda record: (
                str(record.get("type")),
                str(record.get("description")),
                json.dumps(record.get("details"), sort_keys=True, default=str),
            ),
        )

        validation_summary[table_name] = {
            "status": "loaded",
            "n_rows": int(len(table_object.df)),
            "n_columns": int(len(table_object.df.columns)),
            "columns": sorted(table_object.df.columns.tolist()),
            "n_errors": len(normalized_errors),
            "is_valid": bool(table_object.isvalid()),
            "errors": normalized_errors,
        }

    written.append(write_json(validation_summary, "validation_by_table"))
    return written


@component("table_summaries")
def generate_table_summary_baselines() -> list[str]:
    """BaseTable.get_summary() output for every table."""
    summaries = {}
    table_names = list(ClifOrchestrator.__init__.__globals__["TABLE_CLASSES"].keys())

    for table_name in table_names:
        try:
            table_object = load_table(table_name)
        except FileNotFoundError:
            continue

        summary = table_object.get_summary()
        # memory_usage_mb reflects the pandas representation and has no meaningful R
        # counterpart, so it is dropped rather than compared.
        summary.pop("memory_usage_mb", None)
        summaries[table_name] = summary

    return [write_json(summaries, "table_summaries")]


@component("stitching")
def generate_stitching_baseline() -> list[str]:
    hospitalization = load_table("hospitalization")
    adt = load_table("adt")

    _, _, encounter_mapping = stitching_encounters.stitch_encounters(
        hospitalization.df, adt.df, time_interval=6
    )
    return [write_frame(encounter_mapping, "encounter_mapping", ["hospitalization_id"])]


@component("cci")
def generate_cci_baseline() -> list[str]:
    hospital_diagnosis = load_table("hospital_diagnosis")
    cci_scores = comorbidity.calculate_cci(hospital_diagnosis, hierarchy=True)
    return [write_frame(cci_scores, "cci_scores", ["hospitalization_id"])]


@component("elix")
def generate_elix_baseline() -> list[str]:
    hospital_diagnosis = load_table("hospital_diagnosis")
    elix_scores = comorbidity.calculate_elix(hospital_diagnosis, hierarchy=True)
    return [write_frame(elix_scores, "elix_scores", ["hospitalization_id"])]


@component("unit_conversion")
def generate_unit_conversion_baseline() -> list[str]:
    medications = load_table("medication_admin_continuous")
    vitals = load_table("vitals")

    preferred_units = {
        "norepinephrine": "mcg/kg/min",
        "epinephrine": "mcg/kg/min",
        "phenylephrine": "mcg/kg/min",
        "vasopressin": "u/min",
        "dopamine": "mcg/kg/min",
        "dobutamine": "mcg/kg/min",
        "propofol": "mcg/kg/min",
        "fentanyl": "mcg/hr",
    }

    converted, counts = unit_converter.convert_dose_units_by_med_category(
        medications.df,
        vitals_df=vitals.df,
        preferred_units=preferred_units,
        show_intermediate=True,
        override=True,
    )

    return [
        write_frame(converted, "dose_conversion", ["hospitalization_id", "med_order_id", "admin_dttm"]),
        write_frame(counts, "dose_conversion_counts", None),
    ]


@component("wide_dataset")
def generate_wide_dataset_baseline() -> list[str]:
    orchestrator = build_orchestrator()
    category_filters = {
        "vitals": ["map", "spo2", "heart_rate", "weight_kg"],
        "labs": ["creatinine", "platelet_count", "po2_arterial", "bilirubin_total"],
        "patient_assessments": ["gcs_total"],
    }
    wide_frame = orchestrator.create_wide_dataset(
        tables_to_load=list(category_filters.keys()),
        category_filters=category_filters,
        show_progress=False,
    )
    if wide_frame is None:
        wide_frame = orchestrator.wide_df

    return [write_frame(wide_frame, "wide_dataset", ["hospitalization_id", "event_time"])]


@component("hourly")
def generate_hourly_baseline() -> list[str]:
    orchestrator = build_orchestrator()
    category_filters = {
        "vitals": ["map", "spo2", "heart_rate"],
        "labs": ["creatinine", "platelet_count"],
    }
    orchestrator.create_wide_dataset(
        tables_to_load=list(category_filters.keys()),
        category_filters=category_filters,
        show_progress=False,
    )

    aggregation_config = {
        "max": ["heart_rate", "creatinine"],
        "min": ["map", "spo2"],
        "mean": ["heart_rate"],
    }
    hourly_frame = orchestrator.convert_wide_to_hourly(
        aggregation_config=aggregation_config,
        id_name="hospitalization_id",
        hourly_window=1,
    )
    return [write_frame(hourly_frame, "hourly_dataset", ["hospitalization_id", "window_number"])]


@component("sofa")
def generate_sofa_baseline() -> list[str]:
    orchestrator = build_orchestrator()

    # compute_sofa requires the vasopressor columns to already carry converted units
    # (norepinephrine_mcg_kg_min and friends), so the dose conversion must run before
    # the wide dataset is built. Without this clifpy raises a DuckDB binder error.
    orchestrator.convert_dose_units_for_continuous_meds(
        preferred_units={
            "norepinephrine": "mcg/kg/min",
            "epinephrine": "mcg/kg/min",
            "dopamine": "mcg/kg/min",
            "dobutamine": "mcg/kg/min",
        },
        override=True,
    )

    sofa_scores = orchestrator.compute_sofa_scores(id_name="hospitalization_id")

    written = [write_frame(sofa_scores, "sofa_scores", ["hospitalization_id"])]

    # The wide dataset SOFA was computed from is written too, so the R test can
    # isolate a SOFA arithmetic failure from a wide-dataset construction failure.
    if orchestrator.wide_df_sofa is not None:
        written.append(
            write_frame(
                orchestrator.wide_df_sofa,
                "sofa_wide_input",
                ["hospitalization_id", "event_time"],
            )
        )
    return written


@component("waterfall")
def generate_waterfall_baseline() -> list[str]:
    respiratory_support = load_table("respiratory_support")

    resp_frame = respiratory_support.df.copy()
    for column_name in [c for c in resp_frame.columns if "dttm" in c]:
        resp_frame[column_name] = pd.to_datetime(resp_frame[column_name], utc=True)

    waterfall_frame = waterfall.process_resp_support_waterfall(resp_frame, verbose=False)
    return [
        write_frame(waterfall_frame, "resp_support_waterfall", ["hospitalization_id", "recorded_dttm"])
    ]


@component("outliers")
def generate_outlier_baseline() -> list[str]:
    # Note: clifpy 0.5.0's outlier_handler.get_outlier_summary() is broken upstream —
    # it calls validator.validate_numeric_ranges_from_config(), which no longer exists
    # in that module, so it raises AttributeError for every table. Only the working
    # apply_outlier_handling() path is baselined here. clifR implements
    # get_outlier_summary() correctly, so that function has no Python counterpart to
    # compare against until the upstream bug is fixed.
    written = []
    for table_name in ["vitals", "labs", "respiratory_support"]:
        table_object = load_table(table_name)

        # Applying the handler nullifies out-of-range values in place; recording the
        # per-column non-null counts afterwards pins down exactly which cells changed.
        outlier_handler.apply_outlier_handling(table_object)
        non_null_counts = {
            column: int(table_object.df[column].notna().sum())
            for column in sorted(table_object.df.columns)
        }
        written.append(write_json(non_null_counts, f"outlier_applied_nonnull_{table_name}"))
    return written


@component("crosswalk")
def generate_crosswalk_baseline() -> list[str]:
    """Category value normalization and the 2.1 -> 3.0 crosswalk report."""
    sample_values = [
        "Black or African American", "Non-Hispanic", "Other", "Unknown or NA",
        "Sign Language", "Haitian Creole", "ED", "ICU", "Ward & Stepdown",
        "IMV", "High Flow NC", "vasopressin/norepinephrine", "",
    ]
    # Emitted as a list of records rather than an input-keyed map: an empty-string
    # key is not addressable in an R named list, which would make the empty-input
    # case untestable on the R side.
    normalized = [
        {"input": value, "output": crosswalk.normalize_category_value(value)}
        for value in sample_values
    ]

    written = [write_json(normalized, "crosswalk_normalize_category_value")]

    reports = {}
    for table_name in ["patient", "adt", "vitals", "labs"]:
        try:
            table_object = load_table(table_name)
        except FileNotFoundError:
            continue
        _, report = crosswalk.crosswalk_table_2_1_to_3_0(table_object.df, table_name)
        reports[table_name] = report

    written.append(write_json(reports, "crosswalk_reports"))
    return written


@component("mdro")
def generate_mdro_baseline() -> list[str]:
    """MDRO flags for P. aeruginosa on the fixture microbiology tables."""
    culture = load_table("microbiology_culture")
    susceptibility = load_table("microbiology_susceptibility")

    flags = calculate_mdro_flags(
        culture=culture,
        susceptibility=susceptibility,
        organism_name="pseudomonas_aeruginosa",
    )
    return [write_frame(flags, "mdro_pseudomonas", sort_columns=["hospitalization_id", "organism_id"])]


@component("query")
def generate_query_baseline() -> list[str]:
    """Extremal-value lookups against the fixture vitals table."""
    vitals = load_table("vitals")
    vitals_df = vitals.df

    # Build one 36-hour window per hospitalization, anchored at its first vital,
    # for a deterministic slice of the cohort.
    window_bounds = (
        vitals_df.groupby("hospitalization_id")["recorded_dttm"].min().reset_index()
        .sort_values("hospitalization_id", kind="mergesort")
        .head(25)
    )
    ids_w_dttm = pd.DataFrame({
        "hospitalization_id": window_bounds["hospitalization_id"].to_numpy(),
        "start_dttm": window_bounds["recorded_dttm"].to_numpy(),
        "end_dttm": window_bounds["recorded_dttm"].to_numpy() + pd.Timedelta(hours=36),
    })

    query_dict = {
        "spo2": ["max", "min"],
        "heart_rate": ["max", "min", "latest"],
        "weight_kg": ["latest"],
        "temp_c": ["latest"],
    }

    # Bypass the config-file load: hand the function the already-loaded vitals.
    original_load_config = query_utils.load_config
    original_load_data = query_utils.load_data
    query_utils.load_config = lambda _path: {"tables_path": FIXTURE_DIRECTORY, "filetype": FILETYPE}
    query_utils.load_data = lambda *a, **k: vitals_df
    try:
        pivoted = query_utils.lookup_extremal_values_in_long_table(
            ids_w_dttm, query_dict, "vitals"
        )
    finally:
        query_utils.load_config = original_load_config
        query_utils.load_data = original_load_data

    # Persist the windows so the R side queries exactly the same slices.
    written = [write_frame(
        ids_w_dttm, "query_windows",
        sort_columns=["hospitalization_id", "start_dttm", "end_dttm"],
    )]
    written.append(write_frame(
        pivoted, "query_extremal_values",
        sort_columns=["hospitalization_id", "start_dttm", "end_dttm"],
    ))
    return written


# --------------------------------------------------------------------------------
# Entry point
# --------------------------------------------------------------------------------

def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--only", nargs="*", help="Only run these components")
    parser.add_argument("--list", action="store_true", help="List available components and exit")
    arguments = parser.parse_args()

    if arguments.list:
        for component_name in sorted(COMPONENT_REGISTRY):
            print(component_name)
        return 0

    os.makedirs(BASELINE_DIRECTORY, exist_ok=True)

    selected_components = arguments.only or sorted(COMPONENT_REGISTRY)
    unknown_components = [name for name in selected_components if name not in COMPONENT_REGISTRY]
    if unknown_components:
        print(f"Unknown components: {', '.join(unknown_components)}", file=sys.stderr)
        print(f"Available: {', '.join(sorted(COMPONENT_REGISTRY))}", file=sys.stderr)
        return 2

    run_report = {
        "clifpy_version": clifpy.__version__,
        "python_version": sys.version.split()[0],
        "fixture_directory": FIXTURE_DIRECTORY,
        "clif_version": CLIF_VERSION,
        "timezone": SITE_TIMEZONE,
        "components": {},
    }

    exit_code = 0
    for component_name in selected_components:
        print(f"==> {component_name}", flush=True)
        try:
            written_paths = COMPONENT_REGISTRY[component_name]()
            run_report["components"][component_name] = {
                "status": "ok",
                "artifacts": [os.path.basename(path) for path in written_paths],
            }
            for path in written_paths:
                print(f"    wrote {os.path.basename(path)}")
        except Exception as error:  # noqa: BLE001 - report and continue to next component
            exit_code = 1
            run_report["components"][component_name] = {
                "status": "failed",
                "error": f"{type(error).__name__}: {error}",
            }
            print(f"    FAILED: {type(error).__name__}: {error}", file=sys.stderr)
            traceback.print_exc(limit=3)

    write_json(run_report, "_baseline_manifest")
    print(f"\nManifest written to {BASELINE_DIRECTORY}/_baseline_manifest.json")
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
