"""Generate clifpy DQA-report baselines for the clifR parity tests.

Exercises ``clifpy.utils.report_generator`` on the shared fixture cohort and
writes the *exactly-comparable* outputs into ``tests/baseline/`` so the R port
can assert byte-for-byte parity (modulo the run timestamp, which is not
reproducible and is stripped on both sides).

The PDF report has no R equivalent (clifR emits HTML instead), so no PDF
baseline is written. Only the text report, the consolidated CSV, and the
``collect_dqa_issues`` / ``compute_table_stats`` structures are baselined.

For each present fixture table this builds ``validation_data`` exactly as a
report caller would: ``run_full_dqa(df, schema, table_name)`` augmented with
``table_stats`` (from ``compute_table_stats``) and ``total_rows``. Each
``validation_data`` dict is persisted as ``report_dqa_json/{table}_dqa.json``,
which is precisely the input ``collect_table_results`` /
``generate_consolidated_csv`` / ``generate_combined_report`` consume.

Usage:
    cd tests/baseline_py && uv run python generate_report_baseline.py
"""

from __future__ import annotations

import json
import os
import re
import sys
import warnings

# clifpy's validator derives some message orderings from Python sets, so pinning
# the hash seed removes one source of run-to-run churn in the persisted
# validation_data (and thus the CSV/text baselines). A residual, engine-level
# nondeterminism remains — its polars/duckdb aggregations order equal-count
# values arbitrarily among ties — so the committed artifacts may still differ
# slightly on regeneration. This does NOT affect the R parity: clifR consumes the
# persisted validation_data verbatim, so its report output always matches the
# same generation run's baselines.
if os.environ.get("PYTHONHASHSEED") != "0":
    os.environ["PYTHONHASHSEED"] = "0"
    os.execv(sys.executable, [sys.executable, *sys.argv])

warnings.filterwarnings("ignore")

from clifpy.schemas import load_schema
from clifpy.utils.validator import run_full_dqa
from clifpy.utils import report_generator as rg

HERE = os.path.dirname(os.path.abspath(__file__))
FIXTURE_DIRECTORY = os.path.abspath(os.path.join(HERE, "..", "fixtures", "cohort"))
BASELINE_DIRECTORY = os.path.abspath(os.path.join(HERE, "..", "baseline"))
OUTPUT_DIRECTORY = os.path.join(HERE, "output")

CLIF_VERSION = "3.0"
SITE_TIMEZONE = "US/Central"
FILETYPE = "parquet"

# The canonical multi-table order the combined report iterates. Tables not in
# the fixture render through the "absent" path, exercising that branch too.
TABLE_NAMES = list(rg.TABLE_DISPLAY_NAMES.keys())

# Line the report stamps with datetime.now(); replaced with a fixed token so the
# baseline artifact is reproducible and the R side can strip the same line.
TIMESTAMP_TOKEN = "Generated: <TIMESTAMP>"


def load_table(table_name: str):
    """Load one clifpy table object from the fixture at the fixture CLIF version."""
    from clifpy.clif_orchestrator import ClifOrchestrator

    table_class = ClifOrchestrator.__init__.__globals__["TABLE_CLASSES"][table_name]
    return table_class.from_file(
        data_directory=FIXTURE_DIRECTORY,
        filetype=FILETYPE,
        timezone=SITE_TIMEZONE,
        output_directory=OUTPUT_DIRECTORY,
        clif_version=CLIF_VERSION,
    )


def build_validation_data(table_name: str):
    """Return (validation_data, df, schema) for a present table, or None if absent."""
    try:
        table_object = load_table(table_name)
    except FileNotFoundError:
        return None
    df = table_object.df
    schema = table_object.schema or load_schema(table_name, clif_version=CLIF_VERSION)

    validation_data = run_full_dqa(df, schema=schema, table_name=table_name,
                                   clif_version=CLIF_VERSION)
    table_stats = rg.compute_table_stats(df, schema)
    validation_data["table_stats"] = table_stats
    validation_data["total_rows"] = int(len(df))
    return validation_data, df, schema


def strip_timestamp(text: str) -> str:
    """Replace the non-reproducible 'Generated:' line with a fixed token."""
    return re.sub(r"Generated: [0-9].*", TIMESTAMP_TOKEN, text)


def issue_for_json(issue: dict) -> dict:
    """Project an enriched issue to the comparable fields (order-independent)."""
    return {
        "category": issue.get("category"),
        "check_type": issue.get("check_type"),
        "severity": issue.get("severity"),
        "rule_code": issue.get("rule_code", ""),
        "rule_description": issue.get("rule_description", ""),
        "column_field": issue.get("column_field", "NA"),
        "finding": issue.get("finding", issue.get("message", "")),
        "message": issue.get("message", ""),
        "atomic_count": issue.get("atomic_count", 1),
    }


def main() -> None:
    os.makedirs(BASELINE_DIRECTORY, exist_ok=True)
    os.makedirs(OUTPUT_DIRECTORY, exist_ok=True)
    json_dir = os.path.join(BASELINE_DIRECTORY, "report_dqa_json")
    os.makedirs(json_dir, exist_ok=True)

    present_tables: list[str] = []
    collect_issues_baseline: dict = {}
    table_stats_baseline: dict = {}
    text_reports_baseline: dict = {}

    for table_name in TABLE_NAMES:
        built = build_validation_data(table_name)
        if built is None:
            continue
        validation_data, df, schema = built
        present_tables.append(table_name)

        # Persist validation_data exactly as a report caller would, so
        # collect_table_results can reload it on the R side. Keys are NOT
        # sorted: collect_dqa_issues iterates the check dict in insertion
        # order, so preserving run_full_dqa's natural order keeps the R-side
        # report identical to the Python one.
        with open(os.path.join(json_dir, f"{table_name}_dqa.json"), "w",
                  encoding="utf-8") as handle:
            json.dump(validation_data, handle, indent=2, sort_keys=False, default=str)

        # collect_dqa_issues structure
        category_scores, all_issues = rg.collect_dqa_issues(validation_data)
        collect_issues_baseline[table_name] = {
            "category_scores": {k: list(v) for k, v in category_scores.items()},
            "error_count": sum(i.get("atomic_count", 1) for i in all_issues
                               if i["severity"] == "error"),
            "warning_count": sum(i.get("atomic_count", 1) for i in all_issues
                                 if i["severity"] == "warning"),
            "issues": [issue_for_json(i) for i in all_issues],
        }

        # compute_table_stats structure
        table_stats_baseline[table_name] = rg.compute_table_stats(df, schema)

        # Text report (timestamp stripped)
        text_path = os.path.join(OUTPUT_DIRECTORY, f"{table_name}_report.txt")
        rg.generate_text_report(validation_data, table_name, text_path,
                                site_name="Test Site")
        with open(text_path, encoding="utf-8") as handle:
            text_reports_baseline[table_name] = strip_timestamp(handle.read())

    # Consolidated CSV across the full canonical table list (all present here).
    table_results, feedback_map = rg.collect_table_results(json_dir, TABLE_NAMES)
    csv_path = os.path.join(BASELINE_DIRECTORY, "report_consolidated_validation.csv")
    rg.generate_consolidated_csv(table_results, csv_path, TABLE_NAMES,
                                 feedback_map=feedback_map)

    # Absent-table path: a json_dir missing one table renders that table
    # through build_absent_table_dqa_result (the "Table not present" CSV row).
    import shutil
    partial_dir = os.path.join(BASELINE_DIRECTORY, "report_dqa_json_partial")
    if os.path.isdir(partial_dir):
        shutil.rmtree(partial_dir)
    os.makedirs(partial_dir, exist_ok=True)
    partial_names = ["patient", "adt", "position"]
    for name in ("patient", "adt"):  # 'position' deliberately omitted -> absent
        shutil.copyfile(os.path.join(json_dir, f"{name}_dqa.json"),
                        os.path.join(partial_dir, f"{name}_dqa.json"))
    partial_results, partial_fb = rg.collect_table_results(partial_dir, partial_names)
    partial_csv = os.path.join(BASELINE_DIRECTORY, "report_consolidated_partial.csv")
    rg.generate_consolidated_csv(partial_results, partial_csv, partial_names,
                                 feedback_map=partial_fb)

    with open(os.path.join(BASELINE_DIRECTORY, "report_collect_dqa_issues.json"),
              "w", encoding="utf-8") as handle:
        json.dump(collect_issues_baseline, handle, indent=2, sort_keys=True, default=str)
    with open(os.path.join(BASELINE_DIRECTORY, "report_table_stats.json"),
              "w", encoding="utf-8") as handle:
        json.dump(table_stats_baseline, handle, indent=2, sort_keys=True, default=str)
    with open(os.path.join(BASELINE_DIRECTORY, "report_text_reports.json"),
              "w", encoding="utf-8") as handle:
        json.dump(text_reports_baseline, handle, indent=2, sort_keys=True, default=str)
    with open(os.path.join(BASELINE_DIRECTORY, "report_table_names.json"),
              "w", encoding="utf-8") as handle:
        json.dump({"all": TABLE_NAMES, "present": present_tables,
                   "partial": partial_names}, handle, indent=2, sort_keys=True)

    print(f"Report baselines written to {BASELINE_DIRECTORY}")
    print(f"  present tables: {present_tables}")


if __name__ == "__main__":
    main()
