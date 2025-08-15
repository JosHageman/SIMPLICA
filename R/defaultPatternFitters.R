#' Default Pattern Fitters for SIMPLICA
#'
#' Returns a list of default pattern fitting functions used in SIMPLICA.
#' These fitters estimate different types of patterns (constant, additive, multiplicative)
#' from matrix data using specified training cells.
#'
#' @return A named list containing pattern fitting functions:
#' \itemize{
#'   \item constant: Fits a constant value (mean of training data)
#'   \item additive: Fits an additive pattern using additiveMatrixFitter
#'   \item multiplicative: Fits a multiplicative pattern using multiplicativeMatrixFitter
#' }
#'
#' @details Each fitter function takes two arguments:
#' \itemize{
#'   \item mat: Numeric matrix containing the data to fit
#'   \item trainMask: Logical matrix indicating which cells to use for training
#' }
#' All fitters return a fitted matrix of the same dimensions as the input.
#'
#' @examples
#' # Retrieve default pattern fitters
#' fitters <- defaultPatternFitters()
#'
#' # Add a custom diagonal pattern fitter
#' diagonalFitter <- function(mat, trainMask) {
#'   # Extract diagonal values from training data
#'   minDim <- min(nrow(mat), ncol(mat))
#'   diagIndices <- cbind(1:minDim, 1:minDim)
#'   # Only use diagonal elements that are in the training mask
#'   validDiag <- trainMask[diagIndices]
#'   if (any(validDiag)) {
#'     diagVal <- mean(mat[diagIndices][validDiag])
#'   } else {
#'     diagVal <- mean(mat[trainMask])  # fallback to overall mean
#'   }
#'   matrix(diagVal, nrow = nrow(mat), ncol = ncol(mat))
#' }
#' 
#' # Extend the list with your own pattern
#' fitters$diagonal <- diagonalFitter
#'
#' @export
defaultPatternFitters <- function() {
  return(list(
    # Constant pattern: fits overall mean from training data
    constant = function(mat, trainMask) {
      mu <- mean(mat[trainMask])
      matrix(mu, nrow = nrow(mat), ncol = ncol(mat))
    },
    # Additive pattern: fits row and column main effects
    additive = additiveMatrixFitter,
    # Multiplicative pattern: fits bilinear interaction effects
    multiplicative = multiplicativeMatrixFitter
  ))
}
