# clifR

<!-- badges: start -->
<!-- badges: end -->

**clifR** is an R port of the Python library [clifpy](https://github.com/Common-Longitudinal-ICU-data-Format/clifpy) for standardizing and analyzing critical care (ICU) data using the Common Longitudinal ICU data Format (CLIF) version 2.0.

## Overview

Transform heterogeneous ICU data into standardized, analysis-ready datasets with built-in validation, clinical calculations, and high-performance data processing.

## Features

- **CLIF 2.0 Tables**: 9 core table classes implemented (Patient, Hospitalization, ADT, Vitals, Labs, Diagnoses, Medications, Respiratory Support)
- **Schema Validation**: Automatic validation against CLIF specification
- **Clinical Calculations**:
  - SOFA (Sequential Organ Failure Assessment) scores
  - Charlson Comorbidity Index (CCI) from ICD-9/ICD-10 codes
  - P/F ratio calculation
  - Ventilator settings analysis
- **Medication Analysis**:
  - Vasopressor and sedation tracking
  - Antibiotic administration timing
  - Dose unit conversion
- **Respiratory Support**: Ventilator modes, settings, and compliance metrics
- **Unit Conversion**: Smart conversion between medical units (doses, temperature, pressure, labs)
- **Encounter Stitching**: Link related hospital stays
- **Wide Dataset Creation**: Transform narrow clinical data to time-series format
- **High Performance**: Leverages DuckDB and arrow for efficient processing
- **Timezone-Aware**: Proper handling of timestamps across timezones

## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("AartikSarma/clifR")
```

## Quick Start

```r
library(clifR)

# Load and validate CLIF data
orchestrator <- ClifOrchestrator$new(
  data_directory = "path/to/clif/data",
  timezone = "US/Eastern"
)

# Validate all tables
orchestrator$validate_all()

# Access data
vitals <- orchestrator$vitals$df
labs <- orchestrator$labs$df

# Create wide dataset
wide_data <- orchestrator$create_wide_dataset()

# Calculate SOFA scores
sofa_scores <- orchestrator$calculate_sofa_scores()
```

## CLIF Specification

This package implements the [CLIF 2.0 specification](https://clif-icu.com/data-dictionary) for standardized ICU data.

## Supported Tables

- Patient demographics
- Hospitalization records
- ADT (Admission/Discharge/Transfer) events
- Vital signs
- Laboratory results
- Respiratory support
- Medications (continuous and intermittent)
- Microbiology (culture, non-culture, susceptibility)
- Diagnoses, procedures, assessments
- And more...

## Documentation

- **CLIF Specification**: https://clif-icu.com/data-dictionary
- **Python Library (clifpy)**: https://github.com/Common-Longitudinal-ICU-data-Format/clifpy
- **Package Documentation**: [Coming soon]

## Development Status

🚧 **This package is under active development.** Core functionality is being ported from the Python library with systematic cross-validation.

## Contributing

This is an R port of clifpy. Contributions are welcome! Please ensure:
- R implementations match Python outputs (see cross-validation tests)
- Code follows tidyverse style guide
- All functions have roxygen2 documentation
- Tests pass with `devtools::check()`

## Related Projects

- **clifpy**: Python implementation ([GitHub](https://github.com/Common-Longitudinal-ICU-data-Format/clifpy))
- **CLIF Consortium**: https://clif-icu.com/

## License

Apache License 2.0

## Citation

If you use this package in your research, please cite the CLIF Consortium and the original clifpy library.
