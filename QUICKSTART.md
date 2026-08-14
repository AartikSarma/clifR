# clifR Quick Start Guide

## 🚀 Getting Started

### Step 1: Install R Dependencies

The package uses `renv` for dependency management. Install all required packages:

```r
# Open R in the clifR directory
source("install_dependencies.R")
```

This will install:
- tidyverse (dplyr, tidyr, purrr, readr, stringr, lubridate)
- R6
- yaml, jsonlite
- arrow (Parquet support)
- duckdb, DBI
- cli, glue, rlang
- devtools, roxygen2, testthat
- And more...

**Time estimate**: 5-10 minutes

### Step 2: Generate Synthetic Test Data

```r
source("tests/fixtures/generate_synthetic_data.R")
```

This creates test data in `tests/fixtures/synthetic_data/`:
- patient.csv (10 patients)
- hospitalization.csv (~15 hospitalizations)
- adt.csv (~40 ADT events)
- vitals.csv (~8,000 measurements)
- labs.csv (~165 lab results)

**Time estimate**: < 1 minute

### Step 3: Test the Package

```r
# Run comprehensive pipeline test
source("test_pipeline.R")
```

This will:
1. ✓ Load the package
2. ✓ Test individual table classes
3. ✓ Test data validation
4. ✓ Test ClifOrchestrator
5. ✓ Test encounter stitching
6. ✓ Generate summary statistics

**Expected output**: All tests should pass

---

## 📊 Using the Package

### Load Package for Development

```r
devtools::load_all()
```

### Basic Usage: Individual Tables

```r
# Load a single table
patient <- Patient$new(
  data_directory = "tests/fixtures/synthetic_data",
  filetype = "csv",
  timezone = "America/New_York"
)

# Validate data
validation_results <- patient$validate()

# Get summary
patient$summarize()

# Access data
head(patient$df)

# Get demographics
patient$get_demographics_summary()
```

### Advanced Usage: Orchestrator

```r
# Create orchestrator
orchestrator <- ClifOrchestrator$new(
  data_directory = "tests/fixtures/synthetic_data",
  filetype = "csv",
  timezone = "America/New_York",
  stitch_encounter = TRUE,      # Link related encounters
  stitch_time_interval = 24     # Within 24 hours
)

# Load tables. Note: initialize_tables() with no arguments loads ONLY
# patient (clifpy's default), so name every table you need.
orchestrator$initialize_tables(
  tables = c("patient", "hospitalization", "adt", "vitals", "labs")
)

# Access individual tables
patient_data <- orchestrator$patient$df
vitals_data <- orchestrator$vitals$df

# Get summary of all tables
orchestrator$summary()

# Validate all tables
orchestrator$validate_all()

# Export validation reports
orchestrator$export_validation_reports(output_dir = "reports")
```

### Working with Specific Tables

```r
# Hospitalization analysis
hosp <- orchestrator$hospitalization

# Calculate length of stay
los <- hosp$calculate_length_of_stay(units = "days")
summary(los$length_of_stay)

# Get mortality rate
mortality_rate <- hosp$get_mortality_rate()

# Get summary statistics
summary_stats <- hosp$get_summary_stats()
```

```r
# Vitals analysis
vitals <- orchestrator$vitals

# Filter by vital type
temp_data <- vitals$filter_by_category("temp_c")
hr_data <- vitals$filter_by_category("heart_rate")

# Get summary for specific vital
vitals$get_vital_summary("temp_c")
vitals$get_vital_summary("spo2")

# Calculate MAP from SBP/DBP
map_data <- vitals$calculate_map()
```

```r
# Labs analysis
labs <- orchestrator$labs

# Filter by lab category
sodium_data <- labs$filter_by_category("sodium")
creatinine_data <- labs$filter_by_category("creatinine")

# Get lab summary
labs$get_lab_summary("creatinine")
```

---

## 🧪 Cross-Language Validation

### Step 4: Install Python clifpy (Optional)

For cross-language validation:

```bash
pip install clifpy
```

### Step 5: Generate Python Baselines

```bash
python tests/generate_baselines.py
```

This creates baseline outputs in `tests/baseline/`:
- `*_validation_python.json` - Validation results
- `*_summary_python.json` - Summary statistics

### Step 6: Run Cross-Validation Tests

```r
devtools::test()

# Or run specific test file
testthat::test_file("tests/testthat/test-cross-validation.R")
```

Tests compare R outputs to Python baselines with strict tolerances:
- Exact match for counts, IDs, categories
- 1e-12 tolerance for numeric values
- Automatic diff reporting

---

## 📁 Package Structure

```
clifR/
├── R/                          # R source code
│   ├── clif_orchestrator.R     # Main orchestrator class
│   ├── base_table.R            # Base class for all tables
│   ├── tables/                 # Individual table classes
│   │   ├── patient.R
│   │   ├── hospitalization.R
│   │   ├── adt.R
│   │   ├── labs.R
│   │   └── vitals.R
│   └── utils/                  # Utility functions
│       ├── config.R
│       ├── io.R
│       ├── validator.R
│       ├── logging_config.R
│       └── stitching_encounters.R
├── inst/schemas/               # CLIF 2.0 YAML schemas (20 files)
├── tests/
│   ├── fixtures/               # Synthetic test data
│   │   ├── generate_synthetic_data.R
│   │   └── synthetic_data/     # Generated CSV files
│   ├── testthat/               # R tests
│   │   ├── helper-comparison.R
│   │   └── test-cross-validation.R
│   ├── baseline/               # Python baseline outputs
│   └── generate_baselines.py  # Python baseline generator
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exported functions
├── README.md                   # Package overview
├── CLAUDE.md                   # Development guide
├── PROGRESS.md                 # Detailed progress tracking
├── QUICKSTART.md               # This file
└── test_pipeline.R             # Comprehensive test script
```

---

## 🔧 Development Workflow

### Make Changes

```r
# Edit R files in R/

# Reload package
devtools::load_all()

# Test changes
devtools::test()

# Check package
devtools::check()
```

### Update Documentation

```r
# Regenerate documentation from roxygen2 comments
devtools::document()

# Build website (if pkgdown is set up)
pkgdown::build_site()
```

### Add New Features

1. Create R file in appropriate directory
2. Add roxygen2 documentation
3. Export function/class in NAMESPACE
4. Add tests in `tests/testthat/`
5. Update PROGRESS.md

---

## ✅ What's Working

**Core Functionality (100%)**:
- ✅ Package structure
- ✅ All 20 YAML schemas
- ✅ Data loading (CSV/Parquet)
- ✅ Comprehensive validation
- ✅ 18 table classes implemented:
  - Core: Patient, Hospitalization, ADT
  - Clinical Data: Vitals, Labs, HospitalDiagnosis
  - Medications: MedicationAdminContinuous, MedicationAdminIntermittent
  - Respiratory: RespiratorySupport
  - Advanced Support: CodeStatus, CrrtTherapy, EcmoMcs
  - Microbiology: MicrobiologyCulture, MicrobiologyNonculture, MicrobiologySusceptibility
  - Clinical Documentation: PatientAssessments, PatientProcedures, Position
- ✅ ClifOrchestrator
- ✅ Encounter stitching
- ✅ Cross-language test infrastructure

**Testing Infrastructure (100%)**:
- ✅ Synthetic data generator
- ✅ Python baseline generator
- ✅ Cross-validation helpers
- ✅ Automated comparison tests

---

## ✅ Advanced Features (NEW!)

**Implemented Advanced Features**:
- ✅ Unit converter (medication doses, temperature, pressure, labs)
- ✅ SOFA score calculation (Sequential Organ Failure Assessment)
- ✅ Charlson Comorbidity Index (CCI)
- ✅ Wide dataset transformation (narrow to wide format)
- ✅ Time-series aggregation and missing value imputation

**Advanced Usage Examples**:

```r
# Create wide dataset for time-series analysis
wide_data <- orchestrator$create_wide_dataset(
  time_resolution = "hour"  # or "4hour", "day"
)

# Calculate SOFA scores
sofa_results <- orchestrator$calculate_sofa_scores()

# View SOFA summary
sofa_summary <- attr(sofa_results, "summary")
print(sofa_summary)

# Calculate Charlson Comorbidity Index (requires diagnosis data)
# diagnosis_data should have columns: hospitalization_id, icd_code
cci_results <- orchestrator$calculate_charlson_scores(
  diagnosis_data = your_diagnosis_df,
  icd_code_col = "icd_code",
  age_col = "age_at_admission",
  default_icd_version = "10"
)

# Generate comprehensive analysis report
report <- orchestrator$generate_analysis_report(
  include_sofa = TRUE,
  include_charlson = TRUE,
  diagnosis_data = your_diagnosis_df,
  output_file = "analysis_report.rds"
)
```

## 🚧 Future Enhancements

**Potential Additional Features**:
- ⏳ Additional clinical scores (APACHE, qSOFA, SAPS, etc.)
- ⏳ Visualization functions (ggplot2 wrappers for common ICU visualizations)
- ⏳ Export to analysis-ready formats (SAS, Stata, etc.)
- ⏳ Respiratory support waterfall analysis
- ⏳ Automated quality reports
- ⏳ Time-series imputation strategies
- ⏳ Integration with electronic health record (EHR) systems

Core functionality is complete! All 18 CLIF 2.0 table classes are implemented with comprehensive methods for clinical analysis.

---

## 🆘 Troubleshooting

### Package won't load

```r
# Reinstall dependencies
source("install_dependencies.R")

# Check for errors
devtools::check()
```

### Synthetic data won't generate

```r
# Make sure tidyverse is installed
install.packages("tidyverse")
install.packages("lubridate")

# Try again
source("tests/fixtures/generate_synthetic_data.R")
```

### Tests fail

```r
# Make sure synthetic data exists
list.files("tests/fixtures/synthetic_data")

# Make sure package is loaded
devtools::load_all()

# Run tests with verbose output
devtools::test()
```

---

## 📚 Additional Resources

- **CLIF Specification**: https://clif-icu.com/data-dictionary
- **Python clifpy**: https://github.com/Common-Longitudinal-ICU-data-Format/clifpy
- **clifpy Docs**: https://common-longitudinal-icu-data-format.github.io/clifpy/

---

## 🎯 Next Steps

After basic testing works:

1. **Generate Python baselines** for cross-validation
2. **Implement additional features** (SOFA, wide datasets, etc.)
3. **Add more table classes** (respiratory support, medications, etc.)
4. **Create vignettes** for common use cases
5. **Optimize performance** for large datasets
6. **Prepare for CRAN** submission (if desired)

---

**Package Status**: ✅ **READY FOR TESTING**

The core functionality is complete and ready to use!
