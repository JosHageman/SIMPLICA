#' Normalize and validate data at a public entry point
#'
#' Converts a matrix or data frame to a numeric matrix with non-empty axes.
#' Allows NA, rejects NaN and infinity, and requires at least one observed value.
#' Returns the validated matrix or stops with an input error.
#' @noRd
.simplicaValidateDataMatrix <- function(df) {
  if (!(is.matrix(df) || is.data.frame(df))) {
    stop("df must be a matrix or data.frame.")
  }
  mat <- as.matrix(df)
  if (!is.numeric(mat)) {
    stop("df must be numeric after conversion to a matrix.")
  }
  if (nrow(mat) < 1 || ncol(mat) < 1) {
    stop("df must have at least one row and one column.")
  }
  if (any(is.nan(mat)) || any(!is.na(mat) & !is.finite(mat))) {
    stop("df must contain only finite numeric values or NA.")
  }
  if (!any(is.finite(mat))) {
    stop("df must contain at least one finite observed value.")
  }
  mat
}

#' Validate a custom pattern's parameter count
#'
#' Returns a single finite non-negative whole-number value as an integer.
#' Uses pattern to identify invalid complexity values in the error message.
#' @noRd
.simplicaValidatePatternComplexity <- function(value, pattern) {
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
      value < 0 || value != floor(value)) {
    stop("Invalid pattern complexity for '", pattern, "'.")
  }
  as.integer(value)
}

#' Check built-in and custom pattern definitions before fitting
#'
#' Checks built-in names, named custom functions and their complexity entries;
#' requires unique active names and at least one pattern. Fixed complexities are
#' checked here; complexity functions are checked when their results are used.
#' Returns invisible NULL on success and stops on invalid definitions.
#' @noRd
.simplicaValidatePatterns <- function(patterns, customPatterns,
                                      customPatternComplexity) {
  if (!is.character(patterns) || anyNA(patterns) || any(!nzchar(patterns))) {
    stop("patterns must be a character vector of non-empty built-in pattern names.")
  }
  invalidPatterns <- setdiff(patterns, defaultPatterns())
  if (length(invalidPatterns)) {
    stop("Invalid built-in pattern(s): ", paste(invalidPatterns, collapse = ", "))
  }
  if (anyDuplicated(patterns)) {
    stop("Built-in pattern names must be unique.")
  }
  if (!is.list(customPatterns)) {
    stop("customPatterns must be a list.")
  }
  customNames <- names(customPatterns)
  if (length(customPatterns)) {
    if (is.null(customNames) || anyNA(customNames) || any(!nzchar(customNames))) {
      stop("customPatterns must have non-empty, non-missing names.")
    }
    if (anyDuplicated(customNames)) {
      stop("Custom pattern names must be unique.")
    }
    if (!all(vapply(customPatterns, is.function, logical(1)))) {
      stop("Every custom pattern must be a function.")
    }
    duplicatedActive <- intersect(patterns, customNames)
    if (length(duplicatedActive)) {
      stop("Built-in and custom pattern names must be unique.")
    }
    complexityNames <- names(customPatternComplexity)
    missingComplexity <- if (is.null(complexityNames)) {
      customNames
    } else {
      setdiff(customNames, complexityNames)
    }
    if (length(missingComplexity)) {
      stop("No pattern complexity supplied for: ",
           paste(missingComplexity, collapse = ", "))
    }
    for (pattern in customNames) {
      definition <- customPatternComplexity[[pattern]]
      if (!is.function(definition)) {
        .simplicaValidatePatternComplexity(definition, pattern)
      }
    }
  }
  if (!length(patterns) && !length(customPatterns)) {
    stop("At least one built-in or custom pattern must be supplied.")
  }
  invisible(NULL)
}

#' Validate optional background parameters as a pair
#'
#' Accepts two NULL values for automatic estimation, or a finite scalar signal
#' and positive finite scalar scale. Returns invisible NULL or stops on error.
#' @noRd
.simplicaValidateBackground <- function(backgroundSignal, backgroundScale) {
  if (is.null(backgroundSignal) && is.null(backgroundScale)) {
    return(invisible(NULL))
  }
  if (is.null(backgroundSignal) || is.null(backgroundScale)) {
    stop("backgroundSignal and backgroundScale must either both be NULL or both be supplied.")
  }
  if (!is.numeric(backgroundSignal) || length(backgroundSignal) != 1L ||
      !is.finite(backgroundSignal)) {
    stop("backgroundSignal must be one finite numeric value.")
  }
  if (!is.numeric(backgroundScale) || length(backgroundScale) != 1L ||
      !is.finite(backgroundScale) || backgroundScale <= 0) {
    stop("backgroundScale must be one finite positive numeric value.")
  }
  invisible(NULL)
}

#' Validate a minimum component row or column count
#'
#' Requires a whole-number value from three through the corresponding data
#' dimension maximum. Returns an integer; name identifies the argument in errors.
#' @noRd
.simplicaValidateMinimumDimension <- function(value, name, maximum) {
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
      value < 3 || value != floor(value)) {
    stop(name, " must be one integer of at least 3.")
  }
  if (value > maximum) {
    stop(name, " must not exceed the corresponding df dimension.")
  }
  as.integer(value)
}

#' Validate the shared cross-validation settings
#'
#' Checks a positive whole repeat count, a hold-out fraction strictly between
#' zero and one, a non-negative RMSE margin and a finite whole seed.
#' Returns nRepeats as an integer or stops on invalid settings.
#' @noRd
.simplicaValidateCVControl <- function(nRepeats, testFraction,
                                       parsimonyMargin, cvSeed) {
  if (length(nRepeats) != 1 || !is.finite(nRepeats) || nRepeats < 1 ||
      nRepeats != floor(nRepeats)) stop("nRepeats must be a positive integer.")
  if (length(testFraction) != 1 || !is.finite(testFraction) ||
      testFraction <= 0 || testFraction >= 1) {
    stop("testFraction must be strictly between zero and one.")
  }
  if (length(parsimonyMargin) != 1 || !is.finite(parsimonyMargin) ||
      parsimonyMargin < 0) stop("parsimonyMargin must be non-negative.")
  if (length(cvSeed) != 1 || !is.finite(cvSeed) || cvSeed != floor(cvSeed))
    stop("cvSeed must be one finite integer.")
  as.integer(nRepeats)
}

#' Validate row or column indices for component cross-validation
#'
#' Returns integer indices within one through maximum, preserving order and
#' duplicates. Accepts an empty vector; name identifies invalid input in errors.
#' @noRd
.simplicaValidateIndices <- function(indices, name, maximum) {
  if (!length(indices)) return(integer())
  if (!is.numeric(indices) || any(!is.finite(indices)) ||
      any(indices < 1 | indices != floor(indices) | indices > maximum)) {
    stop(name, " must contain finite positive integer indices within df.")
  }
  as.integer(indices)
}
