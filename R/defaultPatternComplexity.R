#' Default conditional pattern complexities for fitness
#'
#' Gives the parameter count for each built-in pattern at a given block size.
#' SIMPLICA uses these counts in the fitness penalty.
#'
#' @return A named list. Each element is a function accepting `nRows`,
#'   `nCols`, and `nObserved`, and returning one non-negative number.
#'
#' @details Each custom pattern supplied to `simplica()` must have a
#'   corresponding complexity entry. Entries may also be fixed non-negative
#'   numeric values.
#'
#' @examples
#' complexity <- defaultPatternComplexity()
#' vapply(complexity, function(count) count(6, 10, 60), numeric(1))
#'
#' @export
defaultPatternComplexity <- function() {
  list(
    # One intercept for all observed cells.
    constant = function(nRows, nCols, nObserved) 1,
    # One fitted mean per row.
    row = function(nRows, nCols, nObserved) nRows,
    # One fitted mean per column.
    column = function(nRows, nCols, nObserved) nCols,
    # Intercept and zero-sum row/column main effects.
    additive = function(nRows, nCols, nObserved) nRows + nCols - 1,
    # Intercept and a rank-one interaction with centering and scale constraints.
    multiplicative = function(nRows, nCols, nObserved) nRows + nCols - 2,
    # Additive main effects plus the constrained rank-one interaction.
    additive_multiplicative = function(nRows, nCols, nObserved) {
      2 * nRows + 2 * nCols - 4
    }
  )
}
