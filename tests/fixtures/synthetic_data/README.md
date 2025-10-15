# Synthetic Test Data

This directory contains synthetic CLIF-compliant data for testing the clifR package.

## Generating Synthetic Data

The synthetic data CSV files are **not included in the git repository** due to their large size (~135 MB total). To generate the test data:

```r
# From the clifR project root
source("tests/fixtures/generate_synthetic_data.R")
```

This will create:
- `patient.csv` (1,000 patients, ~95 KB)
- `hospitalization.csv` (1,500 hospitalizations, ~164 KB)
- `adt.csv` (3,750 ADT events, ~276 KB)
- `vitals.csv` (1,412,358 measurements, ~94 MB)
- `labs.csv` (290,345 results, ~38 MB)

## Data Characteristics

- **Random Seed**: 42 (for reproducibility)
- **Patients**: 1,000 synthetic patients
- **Hospitalizations**: ~1.5 per patient (1,500 total)
- **Time Range**: 2023-2025
- **ADT Events**: ~2.5 per hospitalization
- **Vitals**: Hourly measurements across 6 vital signs
- **Labs**: Daily measurements across 11 lab categories

## Cross-Language Testing

These data files are used by **both** R (clifR) and Python (clifpy) for cross-validation testing:

1. R generates the CSV files (this script)
2. Python clifpy reads the **same** CSV files
3. Outputs are compared to ensure compatibility

Symlinks with `clif_` prefix are created for compatibility with clifpy's naming convention.

## Data Quality

The synthetic data intentionally includes:
- ✅ Valid CLIF-compliant records
- ⚠️ Missing values (e.g., 80% missing death dates)
- ⚠️ Some categorical values not in schema (for validation testing)
- ⚠️ Realistic value ranges with some outliers

This helps test the package's validation and error-handling capabilities.
