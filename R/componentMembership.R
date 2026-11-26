# Internal helpers for the bit-mask representation of component membership.

#' Validate the number of components at an API boundary
#'
#' Returns nComponents as an integer, or stops unless it is a single whole
#' number from 1 to 31, the supported range of membership bit positions.
#' @noRd
.simplicaValidateComponentCount <- function(nComponents) {
  if (length(nComponents) != 1 || !is.numeric(nComponents) ||
      is.na(nComponents) || !is.finite(nComponents) ||
      nComponents != floor(nComponents) ||
      nComponents < 1 || nComponents > 31) {
    stop("'nComponents' must be one integer between 1 and 31.")
  }

  as.integer(nComponents)
}

# Internal callers supply a component count validated at the public boundary.
#' Create the individual component bits for membership encoding
#'
#' Returns powers of two in component order for a validated nComponents.
#' Each bit marks membership of a row or column in that component.
#' @noRd
.simplicaComponentMasks <- function(nComponents) {
  bitwShiftL(1L, seq_len(nComponents) - 1L)
}

#' Compute the largest membership code for a validated component count
#'
#' Returns the code with all nComponents bits set, used as the GA upper bound
#' and the largest allowed value when validating a membership string.
#' @noRd
.simplicaMaximumMembership <- function(nComponents) {
  2^nComponents - 1
}

# Validate membership values only; nComponents has already been validated.
#' Check that membership codes use only the supported component bits
#'
#' Checks string for finite whole numbers from zero through the maximum code
#' for a validated nComponents. Returns invisible TRUE or stops on invalid codes;
#' string length is checked separately by callers.
#' @noRd
.simplicaValidateMembership <- function(string, nComponents) {
  upper <- .simplicaMaximumMembership(nComponents)

  if (!is.numeric(string) || anyNA(string) ||
      any(!is.finite(string)) || any(string != round(string)) ||
      any(string < 0) || any(string > upper)) {
    stop(sprintf(
      "Component membership must contain whole numbers between 0 and %.0f.",
      upper
    ))
  }

  invisible(TRUE)
}

#' Test membership in one component
#'
#' Returns a logical vector indicating whether each membership code contains
#' the bit for the one-based component index. Inputs are already validated.
#' @noRd
.simplicaHasComponent <- function(membership, component) {
  mask <- bitwShiftL(1L, component - 1L)
  bitwAnd(as.integer(membership), mask) != 0L
}

#' Decode the row and column indices of one component
#'
#' Splits a membership string into nRows row codes followed by nCols column
#' codes. Returns a list with rows and cols containing the matching indices;
#' checks the string length and assumes its codes and component are valid.
#' @noRd
.simplicaComponentIndices <- function(string, nRows, nCols, component) {
  if (length(string) != nRows + nCols) {
    stop("Membership string length must equal nRows + nCols.")
  }

  idxRows <- string[seq_len(nRows)]
  idxCols <- string[nRows + seq_len(nCols)]

  list(
    rows = which(.simplicaHasComponent(idxRows, component)),
    cols = which(.simplicaHasComponent(idxCols, component))
  )
}
