# Synthetic Test Data for Cross-Language Validation

## Overview

This directory contains synthetic CLIF-compliant data used for testing and cross-language validation between R (clifR) and Python (clifpy).

## Workflow

### Step 1: Generate Synthetic Data (Once)

```r
source("tests/fixtures/generate_synthetic_data.R")
```

This creates CLIF-compliant CSV files in `tests/fixtures/synthetic_data/`:
- `patient.csv` - 10 patients with demographics
- `hospitalization.csv` - ~15 hospitalizations
- `adt.csv` - ADT events for each hospitalization
- `vitals.csv` - Hourly/4-hourly vital signs
- `labs.csv` - Daily lab results

**Important**: This data is generated with `set.seed(42)` for reproducibility.

### Step 2: Generate Python Baselines

```bash
cd /path/to/clifR
python tests/generate_baselines.py
```

This script:
1. Loads the SAME synthetic data files created in Step 1
2. Runs clifpy operations on them
3. Saves results to `tests/baseline/*_python.*`

Output files:
- `*_validation_python.json` - Validation results
- `*_summary_python.json` - Summary statistics
- `*_python.csv` / `*.parquet` - Calculated outputs (SOFA scores, wide datasets, etc.)

### Step 3: Run R Tests and Compare

```r
# In R
devtools::test()

# Or run specific comparison tests
testthat::test_file("tests/testthat/test-cross-validation.R")
```

R tests will:
1. Load the same synthetic data
2. Run clifR operations
3. Compare R outputs to Python baselines
4. Report any differences exceeding tolerance

## Regenerating Data

If you need to regenerate the synthetic data:

```r
# Generate new synthetic data
source("tests/fixtures/generate_synthetic_data.R")

# Regenerate Python baselines
system("python tests/generate_baselines.py")

# Run tests
devtools::test()
```

**Note**: The same random seed (42) ensures reproducibility, but changing the seed or data generation logic will require regenerating both data and baselines.

## Data Characteristics

### Size
- **Patients**: 10
- **Hospitalizations**: ~15 (1-2 per patient)
- **ADT Events**: ~30-45
- **Vitals**: ~5,000-10,000 measurements
- **Labs**: ~150-200 results

### Coverage
- ✅ Multiple admission types (ED, direct, transfer)
- ✅ Both ICU and floor patients
- ✅ Various discharge dispositions
- ✅ Complete time series data
- ✅ Realistic clinical values within CLIF ranges
- ✅ Some missing data patterns
- ✅ 20% mortality rate

### Time Period
- Admissions: January 1, 2024 - June 30, 2024
- Timezone: America/New_York

## Validation

The synthetic data is designed to:
1. ✅ Pass CLIF 2.0 schema validation
2. ✅ Include edge cases (missing values, boundary values)
3. ✅ Support clinical calculations (SOFA scores, etc.)
4. ✅ Enable encounter stitching tests
5. ✅ Test unit conversion functions

## Files

```
tests/fixtures/
├── README.md (this file)
├── generate_synthetic_data.R (generates data)
└── synthetic_data/ (generated files)
    ├── patient.csv
    ├── hospitalization.csv
    ├── adt.csv
    ├── vitals.csv
    └── labs.csv

tests/
├── generate_baselines.py (Python baseline generator)
└── baseline/ (Python baseline outputs)
    ├── *_validation_python.json
    ├── *_summary_python.json
    └── (future: SOFA scores, wide datasets, etc.)
```

## Cross-Validation Strategy

### Exact Match (tolerance = 0)
- Row counts
- Column names
- Categorical values
- Integer calculations (e.g., ages)

### Strict Numeric (tolerance = 1e-12)
- Summary statistics (mean, median, std)
- Calculated values (length of stay, etc.)
- Vital sign and lab value comparisons

### Moderate Numeric (tolerance = 1e-6)
- Clinical scores with floating point operations
- Unit conversions
- Aggregated metrics

See `CLAUDE.md` for complete tolerance guidelines.
