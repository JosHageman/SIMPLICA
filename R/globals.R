#' Global Variables Declaration
#'
#' This file declares global variables used in the SIMPLICA package to avoid
#' R CMD check notes about "no visible binding for global variable".
#' These variables are typically used within ggplot2 aesthetics and data manipulation
#' functions where they refer to column names in data frames created during execution.
#'
#' @name globals
NULL

utils::globalVariables(c(
  "Column", "Row", "Value", "SC", "Label",        # Used in plot.simplica for ggplot2 aesthetics
  "Iteration", "Fitness", "Replicate", "Fraction", "MetricLabel", # Used in other plotting functions
  "ComponentID", "Col"
))
