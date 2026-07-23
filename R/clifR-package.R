#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom R6 R6Class
#' @import rlang
#' @importFrom rlang .data
#' @importFrom stats setNames quantile sd median
#' @importFrom utils object.size head packageVersion str
#' @importFrom parallel detectCores
## usethis namespace: end
NULL

# Suppress R CMD check NOTEs about the dot placeholder used in dplyr chains and the
# tidy-eval column names referenced throughout the table classes.
utils::globalVariables(".")
