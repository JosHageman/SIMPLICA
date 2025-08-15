#' Generate an additive approximation of a data matrix
#'
#' Constructs a matrix with an additive structure based on the row means,
#' column means, and the grand mean of the original matrix.
#'
#' The result is an approximation \eqn{A} of the original matrix, where:
#' \deqn{A[i,j] = rowMean[i] + colMean[j] - grandMean}
#' This model captures main additive effects of rows and columns, commonly
#' used in exploratory data analysis or baseline modeling.
#'
#' @param mat A numeric matrix or data frame with numeric entries.
#'
#' @return A numeric matrix of the same dimension as \code{mat}, approximating it
#' using an additive model.
#'
#' @examples
#' m <- matrix(c(1, 2, 3, 4), nrow = 2)
#' additiveMatrix(m)
#'
#' @export
additiveMatrix <- function(mat) {

  colMeansVec <- colMeans(mat)
  rowMeansVec <- rowMeans(mat)
  grandMean <- mean(mat)

  approxMatrix <- outer(rowMeansVec, colMeansVec, "+") - grandMean

  return(approxMatrix)
}
