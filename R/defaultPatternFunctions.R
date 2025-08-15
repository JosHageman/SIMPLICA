#' Default pattern generators for SIMPLICA
#'
#' Returns a named list of default matrix approximation functions used to score component patterns.
#' Each function must take a numeric matrix (i.e., a component: subset of rows and columns)
#' and return a matrix of the same dimensions that approximates the original matrix
#' according to a specific structural pattern (e.g., constant, additive, etc.).
#'
#' This list can be passed to \code{fitness2()} via the \code{patternFunctions} argument.
#' Users can extend or override the default patterns by modifying the returned list.
#'
#' ## Requirements for pattern functions
#' Custom pattern functions must:
#' \itemize{
#'   \item Take a numeric matrix as input.
#'   \item Return a numeric matrix of the same dimensions.
#'   \item Be compatible with \code{sum(abs(...))} and \code{sum((...)^2)} operations for fitness scoring.
#' }
#'
#' @return A named list of functions, each representing a matrix approximation method.
#'
#' @examples
#' # Retrieve default pattern functions
#' patterns <- defaultPatternFunctions()
#'
#' # Add a custom pattern based on diagonal structure
#' diagonalPattern <- function(m) {
#'   diagVal <- mean(diag(as.matrix(m)))
#'   matrix(diagVal, nrow = nrow(m), ncol = ncol(m))
#' }
#' 
#' # Extend the list with your own pattern
#' patterns$diagonal <- diagonalPattern
#'
#' @export
defaultPatternFunctions <- function() {
  return(list(
    constant = function(m) matrix(mean(m), nrow = nrow(m), ncol = ncol(m)),
    additive = additiveMatrix,
    multiplicative = multiplicativeMatrix
  ))
}
