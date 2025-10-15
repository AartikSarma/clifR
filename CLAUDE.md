# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**clifR** is an R port of the Python library [clifpy](https://github.com/Common-Longitudinal-ICU-data-Format/clifpy), designed to standardize and analyze critical care (ICU) data using the Common Longitudinal ICU data Format (CLIF) version 2.0.

**Primary Goal**: Transform heterogeneous ICU data into standardized, analysis-ready datasets with built-in validation, clinical calculations, and high-performance data processing.

**Source Library**: https://github.com/Common-Longitudinal-ICU-data-Format/clifpy
**CLIF Specification**: https://clif-icu.com/data-dictionary
**Python Library Docs**: https://common-longitudinal-icu-data-format.github.io/clifpy/

## Core Architecture

### Main Components

1. **ClifOrchestrator** (R6 class)
   - Central orchestration layer managing all data operations
   - Handles data loading, validation, and transformation
   - Supports timezone-aware timestamp processing
   - Manages multiple clinical data tables simultaneously
   - Key methods to implement:
     - `initialize()`: Load and configure data tables
     - `load_table()`: Dynamically load specific clinical tables
     - `validate_all()`: Run validation across all tables
     - `create_wide_dataset()`: Transform to time-series format
     - `convert_wide_to_hourly()`: Aggregate into hourly windows
     - `compute_sofa_scores()`: Calculate Sequential Organ Failure Assessment scores

2. **Table Classes** (20+ clinical data types)
   - All inherit from `BaseTable` class
   - Each represents a specific CLIF table type:
     - `Patient`: Demographics and patient identifiers
     - `Adt`: Admission/Discharge/Transfer events
     - `Hospitalization`: Hospital stay information
     - `HospitalDiagnosis`: Diagnosis codes
     - `Labs`: Laboratory results
     - `Vitals`: Vital signs measurements
     - `RespiratorySupport`: Ventilation and respiratory data
     - `MedicationAdminContinuous`: Continuous medication infusions
     - `MedicationAdminIntermittent`: Intermittent medication doses
     - `CodeStatus`: Code status changes
     - `CrrtTherapy`: Continuous renal replacement therapy
     - `EcmoMcs`: ECMO and mechanical circulatory support
     - `MicrobiologyCulture`: Culture results
     - `MicrobiologyNonculture`: Non-culture microbiology
     - `MicrobiologySusceptibility`: Antimicrobial susceptibility
     - `PatientAssessments`: Clinical assessments
     - `PatientProcedures`: Procedures performed
     - `Position`: Patient positioning data

3. **BaseTable Class** (foundation for all tables)
   - Provides common functionality for all table types
   - Core features to implement:
     - Data loading from CSV/Parquet files
     - Schema validation against YAML schemas
     - Missing data analysis
     - Duplicate detection
     - Timezone validation
     - Categorical value checking
     - Outlier detection
     - Summary statistics generation
     - Export capabilities (CSV, JSON, visualizations)

4. **Utility Modules** (in `R/utils/`)
   - `validator.R`: Schema validation, data type checking
   - `unit_converter.R`: Medical unit conversions
   - `sofa.R`: SOFA score calculation algorithms
   - `comorbidity.R`: Charlson Comorbidity Index calculation
   - `stitching_encounters.R`: Link related hospital stays
   - `wide_dataset.R`: Transform narrow to wide format
   - `config.R`: Configuration file handling
   - `io.R`: Data loading/saving operations
   - `logging_config.R`: Logging setup
   - `outlier_handler.R`: Outlier detection and handling
   - `waterfall.R`: Respiratory support waterfall processing

### Data Flow

```
Data Files (CSV/Parquet)
    ↓
ClifOrchestrator.initialize()
    ↓
Load individual Table objects
    ↓
Schema Validation (YAML schemas)
    ↓
Data Cleaning & Transformation
    ↓
Wide Dataset Creation (time-series format)
    ↓
Clinical Calculations (SOFA, CCI, etc.)
    ↓
Analysis-Ready Output
```

## Development Setup

### Package Management

This project uses `renv` for dependency management. The environment is already initialized.

**Restore dependencies**:
```r
renv::restore()
```

**Add new packages**:
```r
install.packages("package_name")
renv::snapshot()
```

### Key R Dependencies (to be added)

- **Data manipulation**: `dplyr`, `tidyr`, `data.table`, `arrow` (for Parquet)
- **Data validation**: `validate`, `pointblank`
- **Database**: `duckdb`, `DBI`
- **Date/time**: `lubridate`
- **Configuration**: `yaml`, `config`
- **Object-oriented**: `R6`
- **Testing**: `testthat`, `mockery`
- **Documentation**: `roxygen2`, `pkgdown`
- **Performance**: `Rcpp` (if needed)

### Python Library Dependencies Reference

For understanding what to port, the Python library uses:
- `pandas`, `polars` → `dplyr`/`data.table`
- `duckdb` → `duckdb` (same in R)
- `pyarrow` → `arrow`
- `pytz` → `lubridate`
- `pyyaml` → `yaml`
- `pytest` → `testthat`

## Common Development Tasks

### Building the Package

```r
# Load package for development
devtools::load_all()

# Build package
devtools::build()

# Install package locally
devtools::install()
```

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-orchestrator.R")

# Run tests with coverage
covr::package_coverage()
```

### Documentation

```r
# Generate documentation from roxygen2 comments
devtools::document()

# Build pkgdown site
pkgdown::build_site()

# Preview documentation
?ClifOrchestrator
```

### Checking Package

```r
# Run R CMD check
devtools::check()

# Check with additional tests
devtools::check(cran = TRUE)
```

## Schema Files

All CLIF table schemas are defined in YAML files (to be ported from Python library):
- Located in `inst/schemas/` directory
- Each table has corresponding `*_schema.yaml` file
- Schemas define:
  - Required columns
  - Data types
  - Categorical value constraints
  - Numeric ranges
  - Validation rules

**Special config files**:
- `outlier_config.yaml`: Outlier detection thresholds
- `wide_tables_config.yaml`: Wide dataset transformation rules

## Class System Design

Use **R6** for object-oriented design to closely mirror Python's class structure:

```r
library(R6)

BaseTable <- R6Class("BaseTable",
  public = list(
    df = NULL,
    schema = NULL,
    initialize = function(data_path, schema_path, ...) { },
    validate = function() { },
    summarize = function() { }
  )
)

Patient <- R6Class("Patient",
  inherit = BaseTable,
  public = list(
    # Patient-specific methods
  )
)
```

## Implementation Status

### ✅ Completed Features

**Core Infrastructure**:
- ✅ Package structure (DESCRIPTION, NAMESPACE)
- ✅ All 20 YAML schemas ported from Python
- ✅ BaseTable R6 class foundation
- ✅ ClifOrchestrator main orchestration class
- ✅ Data I/O utilities (CSV/Parquet support)
- ✅ Configuration management
- ✅ Logging infrastructure

**Table Classes** (5 of 20):
- ✅ Patient
- ✅ Hospitalization
- ✅ ADT (Admission/Discharge/Transfer)
- ✅ Vitals
- ✅ Labs

**Validation & Quality**:
- ✅ Schema validation against YAML definitions
- ✅ Data type checking
- ✅ Categorical value validation
- ✅ Missing data analysis
- ✅ Duplicate detection
- ✅ Timezone validation

**Advanced Features**:
- ✅ Unit conversion (medications, temperature, pressure, labs)
- ✅ Wide dataset transformation (narrow to wide format)
- ✅ Time-series aggregation (hourly, 4-hour, daily)
- ✅ Missing value imputation (forward/backward fill)
- ✅ SOFA score calculation (all 6 components)
- ✅ Charlson Comorbidity Index (ICD-9 and ICD-10)
- ✅ Encounter stitching (link related hospitalizations)

**Testing Infrastructure**:
- ✅ Synthetic data generator (with reproducible seed)
- ✅ Python baseline generator for cross-validation
- ✅ Cross-validation test helpers
- ✅ Automated comparison tests

**ClifOrchestrator Methods**:
- ✅ `initialize_tables()`: Load multiple tables
- ✅ `validate_all()`: Comprehensive validation
- ✅ `create_wide_dataset()`: Wide format transformation
- ✅ `calculate_sofa_scores()`: SOFA calculation
- ✅ `calculate_charlson_scores()`: CCI calculation
- ✅ `convert_medication_doses()`: Unit conversion
- ✅ `generate_analysis_report()`: Comprehensive analysis
- ✅ `summary()`: Summary statistics

### ⏳ Pending Features

**Additional Table Classes** (15 remaining):
- ⏳ RespiratorySupport
- ⏳ MedicationAdminContinuous
- ⏳ MedicationAdminIntermittent
- ⏳ HospitalDiagnosis
- ⏳ CodeStatus
- ⏳ CrrtTherapy
- ⏳ EcmoMcs
- ⏳ MicrobiologyCulture
- ⏳ MicrobiologyNonculture
- ⏳ MicrobiologySusceptibility
- ⏳ PatientAssessments
- ⏳ PatientProcedures
- ⏳ Position
- ⏳ (and others as needed)

**Additional Features**:
- ⏳ Outlier detection and handling
- ⏳ Respiratory support waterfall
- ⏳ Additional clinical scores (APACHE, qSOFA)
- ⏳ Visualization functions
- ⏳ Export to analysis-ready formats
- ⏳ Performance optimization for large datasets
- ⏳ Full cross-language validation against Python baselines

## Key Features Implementation Details

### 1. Schema Validation ✅ COMPLETE
- Load YAML schemas
- Validate column presence, data types, categorical values
- Check for missing data, duplicates
- Generate validation reports

### 2. Wide Dataset Creation ✅ COMPLETE
- Transform narrow clinical data to wide time-series format
- Pivot multiple measurement types into columns
- Handle temporal alignment
- Support hourly aggregation

### 3. Clinical Calculations ✅ COMPLETE

**SOFA Score** (Sequential Organ Failure Assessment):
- Cardiovascular, respiratory, hepatic, coagulation, renal, neurological components
- Requires vitals, labs, respiratory support data
- Calculated at each time point
- **Implemented in**: `R/utils/sofa.R`

**Charlson Comorbidity Index**:
- Calculate from ICD diagnosis codes (ICD-9 and ICD-10)
- Weight different comorbidities
- Generate summary scores
- Age adjustment support
- **Implemented in**: `R/utils/comorbidity.R`

### 4. Encounter Stitching ✅ COMPLETE
- Link related hospital stays
- Use configurable time interval (default: hours between encounters)
- Create encounter mapping for longitudinal analysis
- **Implemented in**: `R/utils/stitching_encounters.R`

### 5. Unit Conversion ✅ COMPLETE
- Convert between medical units (e.g., mg/dL ↔ mmol/L)
- Handle temperature scales (Celsius/Fahrenheit)
- Pressure conversions (mmHg, cmH2O)
- Medication dose conversions (mcg/kg/min ↔ mg/hr)
- **Implemented in**: `R/utils/unit_converter.R`

### 6. Outlier Handling ⏳ PENDING
- Statistical outlier detection
- Configurable thresholds per measurement type
- Options to remove, flag, or impute outliers

## Tidyverse Preference

Per user preferences, use tidyverse approaches where appropriate:
- `dplyr` for data manipulation
- `tidyr` for reshaping
- `purrr` for functional programming
- Pipe operator `%>%` or native `|>` for chaining

However, consider `data.table` for performance-critical operations with large datasets.

## Testing Approach

Structure tests to mirror Python library's test organization:

```
tests/testthat/
├── test-orchestrator.R
├── test-patient.R
├── test-adt.R
├── test-labs.R
├── test-vitals.R
├── test-validator.R
├── test-sofa.R
├── test-wide-dataset.R
└── fixtures/
    └── sample_data/
```

Use fixtures for test data, potentially porting from Python library's test fixtures.

## Cross-Language Testing & Validation

A critical component of this project is ensuring that the R port produces results identical (within acceptable tolerances) to the Python implementation. This requires systematic cross-validation.

### Synthetic Data Strategy

**Generate CLIF-compliant synthetic datasets** for testing:

1. **Synthetic Data Generator** (`tests/fixtures/generate_synthetic_data.R`)
   - Create realistic but synthetic ICU data for all CLIF tables
   - Ensure data follows CLIF 2.0 specification
   - Include edge cases (missing values, outliers, timezone variations)
   - Generate multiple patient trajectories with:
     - Various hospitalization patterns
     - Complete vital signs time series
     - Laboratory results at different intervals
     - Medication administrations (continuous and intermittent)
     - Respiratory support changes
     - Diagnosis codes for comorbidity testing

2. **Python Baseline Generator** (`tests/fixtures/generate_synthetic_data.py`)
   - Create identical synthetic data using Python
   - Or use R-generated data to create Python baseline results
   - Generate expected outputs from clifpy for comparison

3. **Data Characteristics**
   - **Size**: Start small (10 patients, 100 time points), scale up for performance testing
   - **Complexity**: Include patients with multiple hospitalizations for encounter stitching
   - **Completeness**: Mix of complete and sparse data to test handling of missing values
   - **Units**: Multiple unit representations to test conversion functions

### Cross-Validation Approach

**For each component, perform systematic comparison**:

#### 1. Validation Functions
```r
# Test: Schema validation produces same error messages
python_errors <- read_json("tests/baseline/validation_errors_python.json")
r_errors <- validate_patient_schema(test_data)
compare_validation_results(r_errors, python_errors)
```

**Expected**: Identical error counts and types for:
- Missing required columns
- Invalid data types
- Out-of-range values
- Invalid categorical values

#### 2. Unit Conversion
```r
# Test: Medication dose conversions match Python exactly
test_cases <- tribble(
  ~medication, ~dose, ~from_unit, ~to_unit, ~expected_python,
  "norepinephrine", 0.1, "mcg/kg/min", "mg/hr", 0.42,  # for 70kg patient
  "propofol", 50, "mcg/kg/min", "mg/hr", 210,
  # ... more cases
)

for (i in seq_len(nrow(test_cases))) {
  r_result <- convert_medication_dose(...)
  expect_equal(r_result, test_cases$expected_python[i], tolerance = 1e-10)
}
```

**Tolerance**: Exact match for unit conversions (no floating point tolerance)

#### 3. SOFA Score Calculation
```r
# Test: SOFA scores match Python implementation
python_sofa <- read_csv("tests/baseline/sofa_scores_python.csv")
r_sofa <- compute_sofa_scores(orchestrator, id_name = "hospitalization_id")

# Compare component scores
compare_numeric_results(
  r_sofa$cardiovascular_score,
  python_sofa$cardiovascular_score,
  tolerance = 0  # SOFA scores are integers, must match exactly
)
```

**Expected**: Exact match for all six SOFA components:
- Cardiovascular (vasopressor dosing)
- Respiratory (PaO2/FiO2 ratio)
- Hepatic (bilirubin)
- Coagulation (platelets)
- Renal (creatinine, urine output)
- Neurological (GCS)

#### 4. Wide Dataset Transformation
```r
# Test: Wide dataset structure and values match
python_wide <- read_parquet("tests/baseline/wide_dataset_python.parquet")
r_wide <- create_wide_dataset(orchestrator)

# Compare dimensions
expect_equal(nrow(r_wide), nrow(python_wide))
expect_equal(ncol(r_wide), ncol(python_wide))

# Compare specific values with tolerance for floating point
compare_numeric_columns(r_wide, python_wide, tolerance = 1e-12)
```

**Tolerance**: `1e-12` for numeric values (accounting for floating point arithmetic differences)

#### 5. Encounter Stitching
```r
# Test: Encounter mappings identical
python_mapping <- read_csv("tests/baseline/encounter_mapping_python.csv")
r_mapping <- stitch_encounters(hospitalizations, time_interval = 24)

# Exact match required for encounter IDs
expect_identical(r_mapping$original_id, python_mapping$original_id)
expect_identical(r_mapping$stitched_id, python_mapping$stitched_id)
```

**Expected**: Exact match on encounter groupings

#### 6. Charlson Comorbidity Index
```r
# Test: CCI scores match for same diagnosis codes
python_cci <- read_csv("tests/baseline/cci_scores_python.csv")
r_cci <- calculate_cci(hospital_diagnosis)

compare_numeric_results(r_cci$cci_score, python_cci$cci_score, tolerance = 0)
```

**Expected**: Exact match (integer scores)

### Comparison Testing Infrastructure

**Create helper functions for systematic comparison**:

```r
# tests/testthat/helper-comparison.R

compare_validation_results <- function(r_result, python_result) {
  # Compare error counts by type
  # Compare specific error messages
  # Return detailed diff if mismatch
}

compare_numeric_results <- function(r_values, python_values, tolerance = 1e-12) {
  # Element-wise comparison
  # Report statistics: max diff, mean diff, correlation
  # Flag any values exceeding tolerance
}

compare_dataframes <- function(r_df, python_df,
                                key_cols = NULL,
                                tolerance = 1e-12,
                                ignore_row_order = TRUE) {
  # Compare structure (dimensions, column names, types)
  # Compare values (with tolerance for numeric)
  # Generate detailed diff report
}

generate_comparison_report <- function(component_name, comparison_results) {
  # Create markdown report with:
  #   - Pass/fail status
  #   - Summary statistics
  #   - Detailed discrepancies
  #   - Visualizations of differences
}
```

### Tolerance Guidelines

Different components require different comparison tolerances:

1. **Exact Match (tolerance = 0)**:
   - Integer scores (SOFA, CCI)
   - Categorical values
   - IDs and mappings
   - Unit conversion factors (should be exact ratios)
   - Count statistics

2. **Strict Numeric (tolerance = 1e-12)**:
   - Wide dataset values
   - Laboratory values
   - Vital signs
   - Medication doses after conversion

3. **Moderate Numeric (tolerance = 1e-6)**:
   - Calculated ratios (e.g., PaO2/FiO2)
   - Statistical summaries
   - Aggregated values

4. **Intentional Differences**:
   - Document any intentional differences in implementation
   - Examples might include:
     - R-specific optimizations
     - Alternative algorithms with equivalent results
     - Different handling of edge cases (if justified)

### Continuous Validation Workflow

**Integrate cross-validation into development process**:

1. **Before implementing R function**:
   - Generate Python baseline results using clifpy
   - Save to `tests/baseline/[component]_python.*`
   - Document Python version and clifpy version used

2. **During R implementation**:
   - Run comparison tests frequently
   - Use tests to debug discrepancies
   - Iterate until tolerance met

3. **After R implementation**:
   - Run full comparison suite
   - Generate comparison report
   - Document any remaining differences

4. **Regression prevention**:
   - Keep all baseline files in version control
   - Re-run comparisons in CI/CD if available
   - Update baselines only when Python library updates

### Baseline Generation Script

Create `tests/generate_baselines.py`:

```python
# Generate baseline results from clifpy for comparison

from clifpy import ClifOrchestrator
import pandas as pd

# Load synthetic data
co = ClifOrchestrator(data_directory="tests/fixtures/synthetic_data")

# Generate baselines for all components
co.validate_all()
# Save validation errors

co.compute_sofa_scores().to_csv("tests/baseline/sofa_scores_python.csv")

co.create_wide_dataset().to_parquet("tests/baseline/wide_dataset_python.parquet")

# ... more baseline generations
```

### Documentation of Validation Results

Maintain `tests/VALIDATION_RESULTS.md`:

```markdown
# Cross-Language Validation Results

## Last Updated
2024-01-15

## Python Baseline
- clifpy version: 0.2.2
- Python version: 3.9

## Component Status

| Component | Status | Max Difference | Notes |
|-----------|--------|----------------|-------|
| Schema Validation | ✅ Pass | Exact match | - |
| Unit Conversion | ✅ Pass | Exact match | - |
| SOFA Scores | ✅ Pass | Exact match | - |
| Wide Dataset | ✅ Pass | 3.2e-13 | Within tolerance |
| Encounter Stitching | ✅ Pass | Exact match | - |
| CCI Calculation | ✅ Pass | Exact match | - |

## Known Differences
None currently.
```

This systematic approach ensures that the R port maintains fidelity to the Python implementation while documenting any necessary deviations.

## Version Control

- Use Git for version control
- **Do not create duplicative files** when testing alternatives
- Use Git branches/commits for experimentation
- Follow conventional commit messages

## Important Notes

1. **No Placeholder Data**: If unable to implement functionality, stop and ask for guidance rather than using simulated/placeholder data.

2. **CLIF Specification Compliance**: All implementations must comply with CLIF 2.0 specification at https://clif-icu.com/data-dictionary

3. **Timezone Handling**: Critical for clinical data - all timestamps must support timezone-aware operations

4. **Performance**: ICU datasets can be large (millions of rows). Optimize for performance using:
   - DuckDB for SQL operations
   - `data.table` for large transformations
   - `arrow` for Parquet I/O

5. **Validation Rigor**: Validation is critical for clinical data integrity. Implement comprehensive checks matching Python library's validation suite.

6. **Descriptive Naming**: Always use descriptive variable names (per user preference).

## Reference Files in Python Library

Key files to reference when porting:
- `clifpy/clif_orchestrator.py`: Main orchestrator implementation
- `clifpy/tables/base_table.py`: Base class for all tables
- `clifpy/utils/validator.py`: Validation logic
- `clifpy/utils/sofa.py`: SOFA score calculation
- `clifpy/utils/wide_dataset.py`: Wide dataset transformation
- `clifpy/schemas/*.yaml`: All schema definitions

## Package Structure

```
clifR/
├── R/
│   ├── clif_orchestrator.R
│   ├── base_table.R
│   ├── tables/
│   │   ├── patient.R
│   │   ├── adt.R
│   │   ├── labs.R
│   │   └── ... (other tables)
│   └── utils/
│       ├── validator.R
│       ├── sofa.R
│       ├── unit_converter.R
│       └── ... (other utilities)
├── inst/
│   └── schemas/
│       ├── patient_schema.yaml
│       └── ... (other schemas)
├── tests/
│   └── testthat/
│       └── ... (test files)
├── man/ (generated documentation)
├── DESCRIPTION
├── NAMESPACE
└── README.md
```
