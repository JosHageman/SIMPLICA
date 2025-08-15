#' Multiplicative Pattern Fitter
#'
#' Fits a multiplicative pattern to matrix data using training data specified by a mask.
#' The pattern is based on the outer product of centered row and column effects, creating
#' a bilinear surface that can capture multiplicative interactions between rows and columns.
#'
#' @param mat Numeric matrix containing the data to fit
#' @param trainMask Logical matrix of same dimensions as mat, indicating which cells to use for training
#'
#' @return Numeric matrix of same dimensions as mat containing the fitted multiplicative pattern
#'
#' @details The function creates a multiplicative pattern by:
#' \itemize{
#'   \item Computing the mean of training data as baseline
#'   \item Creating centered row effects (0 to nRows-1, mean-centered)
#'   \item Creating centered column effects (0 to nCols-1, mean-centered) 
#'   \item Taking outer product to form bilinear pattern
#'   \item Fitting scaling coefficient using least squares on training data
#'   \item Returning baseline plus scaled pattern
#' }
#'
#' @export
multiplicativeMatrixFitter <- function(mat, trainMask) {
  # Get matrix dimensions and compute baseline mean from training data
  nR <- nrow(mat)
  nC <- ncol(mat)
  mu <- mean(mat[trainMask])

  # Create centered row and column effects for multiplicative pattern
  rowEffect <- (0:(nR - 1)) - mean(0:(nR - 1))
  colEffect <- (0:(nC - 1)) - mean(0:(nC - 1))
  pattern <- outer(rowEffect, colEffect)

  # Fit scaling coefficient using least squares on training data
  resid <- mat - mu
  num <- sum(resid[trainMask] * pattern[trainMask])
  den <- sum(pattern[trainMask]^2)
  a <- if (den > 0) num / den else 0

  # Return baseline plus scaled multiplicative pattern
  return(mu + a * pattern)
}