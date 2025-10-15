# Install all required dependencies for clifR package development
# Run this script once to set up your development environment

packages_required <- c(
  # Core tidyverse
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "stringr",

  # Object-oriented programming
  "R6",

  # Date/time handling
  "lubridate",

  # Configuration and data formats
  "yaml",
  "jsonlite",

  # Data processing
  "arrow",          # Parquet support
  "duckdb",         # High-performance database
  "DBI",            # Database interface
  "data.table",     # For performance-critical operations

  # Utilities
  "rlang",
  "cli",
  "glue",

  # Development tools
  "devtools",
  "roxygen2",
  "pkgdown",

  # Testing
  "testthat",
  "mockery",
  "covr",
  "withr",

  # Validation
  "pointblank",
  "validate",

  # Documentation
  "knitr",
  "rmarkdown",

  # Visualization (for validation reports)
  "ggplot2"
)

cat("Installing required packages for clifR development...\n\n")

# Install packages
for (pkg in packages_required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg)
  } else {
    cat(pkg, "already installed.\n")
  }
}

cat("\nAll required packages installed!\n")
cat("\nNow updating renv lockfile...\n")

# Update renv snapshot
renv::snapshot(prompt = FALSE)

cat("\nDevelopment environment ready!\n")
cat("You can now load the package with: devtools::load_all()\n")
