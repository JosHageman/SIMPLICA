
#' Additive Pattern Fitter
#'
#' Fits an additive pattern to matrix data using training data specified by a mask.
#' The pattern is based on row and column main effects, creating an additive model
#' where each cell value is the sum of a row effect and column effect minus the overall mean.
#'
#' @param mat Numeric matrix containing the data to fit
#' @param trainMask Logical matrix of same dimensions as mat, indicating which cells to use for training
#'
#' @return Numeric matrix of same dimensions as mat containing the fitted additive pattern
#'
#' @details The function creates an additive pattern by:
#' \itemize{
#'   \item Masking non-training cells as NA to compute statistics only on training data
#'   \item Computing overall mean of training data
#'   \item Computing row means and column means from training data
#'   \item Replacing non-finite means with overall mean as fallback
#'   \item Creating additive pattern as outer sum of row and column effects minus overall mean
#' }
#'
#' @export
additiveMatrixFitter <- function(mat, trainMask) {
  # Get matrix dimensions and create training-only matrix
  nR <- nrow(mat)
  nC <- ncol(mat)
  mTrain <- matrix(NA_real_, nR, nC)
  mTrain[trainMask] <- mat[trainMask]

  # Compute overall mean and row/column means from training data
  mu <- mean(mTrain, na.rm = TRUE)
  rMeans <- rowMeans(mTrain, na.rm = TRUE)
  cMeans <- colMeans(mTrain, na.rm = TRUE)

  # Replace non-finite means with overall mean as fallback
  rMeans[!is.finite(rMeans)] <- mu
  cMeans[!is.finite(cMeans)] <- mu

  # Return additive pattern: row effects + column effects - overall mean
  return(outer(rMeans, cMeans, "+") - mu)
}