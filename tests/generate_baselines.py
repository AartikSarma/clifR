#!/usr/bin/env python3
"""
Generate Baseline Results from Python clifpy

This script loads the SAME synthetic data files created by the R script
and generates expected outputs using the Python clifpy library.
These baseline outputs are then compared to R outputs for validation.

Requirements:
    pip install clifpy

Usage:
    python tests/generate_baselines.py
"""

import sys
from pathlib import Path
import pandas as pd
import json

# Try to import clifpy
try:
    from clifpy import ClifOrchestrator
    from clifpy.tables import Patient, Hospitalization, Adt, Vitals, Labs
except ImportError:
    print("ERROR: clifpy not installed")
    print("Install with: pip install clifpy")
    sys.exit(1)

# Paths
SYNTHETIC_DATA_DIR = Path("tests/fixtures/synthetic_data")
BASELINE_DIR = Path("tests/baseline")

# Create baseline directory
BASELINE_DIR.mkdir(parents=True, exist_ok=True)

print("=" * 70)
print("Generating Python Baseline Results from clifpy")
print("=" * 70)
print(f"Input data: {SYNTHETIC_DATA_DIR}")
print(f"Output baselines: {BASELINE_DIR}")
print()

# ============================================================================
# 1. VALIDATION BASELINES
# ============================================================================

print("1. Generating validation baselines...")

# Load tables individually and validate
try:
    patient = Patient.from_file(
        data_directory=str(SYNTHETIC_DATA_DIR),
        filetype="csv",
        timezone="America/New_York"
    )
    patient_valid = patient.validate()

    # Save validation results
    validation_results = {
        "table_name": "patient",
        "n_rows": len(patient.df),
        "n_cols": len(patient.df.columns),
        "is_valid": patient_valid,
        "errors": patient.errors if hasattr(patient, 'errors') else {},
        "warnings": patient.warnings if hasattr(patient, 'warnings') else {}
    }

    with open(BASELINE_DIR / "patient_validation_python.json", "w") as f:
        json.dump(validation_results, f, indent=2, default=str)

    print(f"   Patient table: {len(patient.df)} rows, valid={patient_valid}")

except Exception as e:
    print(f"   WARNING: Could not validate patient table: {e}")

# Repeat for other tables...
for table_name, table_class in [
    ("hospitalization", Hospitalization),
    ("adt", Adt),
    ("vitals", Vitals),
    ("labs", Labs)
]:
    try:
        table = table_class.from_file(
            data_directory=str(SYNTHETIC_DATA_DIR),
            filetype="csv",
            timezone="America/New_York"
        )
        is_valid = table.validate()

        validation_results = {
            "table_name": table_name,
            "n_rows": len(table.df),
            "n_cols": len(table.df.columns),
            "is_valid": is_valid,
            "errors": table.errors if hasattr(table, 'errors') else {},
            "warnings": table.warnings if hasattr(table, 'warnings') else {}
        }

        with open(BASELINE_DIR / f"{table_name}_validation_python.json", "w") as f:
            json.dump(validation_results, f, indent=2, default=str)

        print(f"   {table_name} table: {len(table.df)} rows, valid={is_valid}")

    except Exception as e:
        print(f"   WARNING: Could not validate {table_name} table: {e}")

print()

# ============================================================================
# 2. SUMMARY STATISTICS BASELINES
# ============================================================================

print("2. Generating summary statistics baselines...")

# Hospitalization summary stats
try:
    hosp = Hospitalization.from_file(
        data_directory=str(SYNTHETIC_DATA_DIR),
        filetype="csv",
        timezone="America/New_York"
    )

    # Calculate length of stay
    hosp.df['length_of_stay_days'] = (
        (hosp.df['discharge_dttm'] - hosp.df['admission_dttm']).dt.total_seconds() / 86400
    )

    summary_stats = {
        "n_hospitalizations": len(hosp.df),
        "los_mean": float(hosp.df['length_of_stay_days'].mean()),
        "los_median": float(hosp.df['length_of_stay_days'].median()),
        "los_std": float(hosp.df['length_of_stay_days'].std()),
        "age_mean": float(hosp.df['age_at_admission'].mean()),
        "age_median": float(hosp.df['age_at_admission'].median()),
        "n_expired": int((hosp.df['discharge_category'] == 'Expired').sum()),
        "mortality_rate": float((hosp.df['discharge_category'] == 'Expired').sum() / len(hosp.df))
    }

    with open(BASELINE_DIR / "hospitalization_summary_python.json", "w") as f:
        json.dump(summary_stats, f, indent=2)

    print(f"   Hospitalization summary: N={summary_stats['n_hospitalizations']}, "
          f"LOS={summary_stats['los_mean']:.1f} days")

except Exception as e:
    print(f"   WARNING: Could not generate hospitalization summary: {e}")

# Vitals summary by category
try:
    vitals = Vitals.from_file(
        data_directory=str(SYNTHETIC_DATA_DIR),
        filetype="csv",
        timezone="America/New_York"
    )

    vital_summaries = {}
    for vital_cat in vitals.df['vital_category'].unique():
        vital_subset = vitals.df[vitals.df['vital_category'] == vital_cat]
        vital_summaries[vital_cat] = {
            "n": int(len(vital_subset)),
            "mean": float(vital_subset['vital_value'].mean()),
            "median": float(vital_subset['vital_value'].median()),
            "std": float(vital_subset['vital_value'].std()),
            "min": float(vital_subset['vital_value'].min()),
            "max": float(vital_subset['vital_value'].max())
        }

    with open(BASELINE_DIR / "vitals_summary_python.json", "w") as f:
        json.dump(vital_summaries, f, indent=2)

    print(f"   Vitals summary: {len(vital_summaries)} categories")

except Exception as e:
    print(f"   WARNING: Could not generate vitals summary: {e}")

# Labs summary by category
try:
    labs = Labs.from_file(
        data_directory=str(SYNTHETIC_DATA_DIR),
        filetype="csv",
        timezone="America/New_York"
    )

    lab_summaries = {}
    for lab_cat in labs.df['lab_category'].unique():
        lab_subset = labs.df[labs.df['lab_category'] == lab_cat]
        lab_summaries[lab_cat] = {
            "n": int(len(lab_subset)),
            "mean": float(lab_subset['lab_value'].mean()),
            "median": float(lab_subset['lab_value'].median()),
            "std": float(lab_subset['lab_value'].std()),
            "min": float(lab_subset['lab_value'].min()),
            "max": float(lab_subset['lab_value'].max())
        }

    with open(BASELINE_DIR / "labs_summary_python.json", "w") as f:
        json.dump(lab_summaries, f, indent=2)

    print(f"   Labs summary: {len(lab_summaries)} categories")

except Exception as e:
    print(f"   WARNING: Could not generate labs summary: {e}")

print()

# ============================================================================
# 3. ORCHESTRATOR BASELINES (if available)
# ============================================================================

print("3. Attempting to generate orchestrator baselines...")

try:
    # Load all data with orchestrator
    co = ClifOrchestrator(
        data_directory=str(SYNTHETIC_DATA_DIR),
        filetype="csv",
        timezone="America/New_York"
    )

    print("   ClifOrchestrator loaded successfully")

    # TODO: Add SOFA scores, wide datasets, etc. when those functions are implemented
    # co.compute_sofa_scores().to_csv(BASELINE_DIR / "sofa_scores_python.csv", index=False)
    # co.create_wide_dataset().to_parquet(BASELINE_DIR / "wide_dataset_python.parquet")

except Exception as e:
    print(f"   NOTE: ClifOrchestrator not yet implemented or data incomplete: {e}")

print()

# ============================================================================
# SUMMARY
# ============================================================================

print("=" * 70)
print("Baseline Generation Complete")
print("=" * 70)
print(f"Baseline files saved to: {BASELINE_DIR}")
print()
print("Baseline files created:")
baseline_files = list(BASELINE_DIR.glob("*_python.*"))
for f in sorted(baseline_files):
    size_kb = f.stat().st_size / 1024
    print(f"  - {f.name} ({size_kb:.1f} KB)")
print()
print("These baseline files can now be compared to R (clifR) outputs")
print("to ensure cross-language compatibility.")
print()
