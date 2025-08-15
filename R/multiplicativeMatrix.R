#' Generate an multiplicative approximation of a data matrix
#'
#' Approximates a data matrix using a low-rank multiplicative model based on
#' a fixed outer product of centered row and column effects. 
#'
#' The model assumes:
#' \deqn{M[i,j] = mu + a * rowEffect[i] * colEffect[j]}
#' where \code{mu} is the overall mean of the input matrix, and
#' \code{rowEffect} and \code{colEffect} are centered integer sequences.
#'
#' @param mat A numeric matrix with values to approximate.
#'
#' @return A numeric matrix of the same size as \code{mat}, containing the fitted values.
#'
#' @export
multiplicativeMatrix <- function(mat) {

  mat <- as.matrix(mat)

  nrow <- nrow(mat)
  ncol <- ncol(mat)

  mu <- mean(mat)

  rowEffect <- (0:(nrow - 1)) - mean(0:(nrow - 1))
  colEffect <- (0:(ncol - 1)) - mean(0:(ncol - 1))

  pattern <- outer(rowEffect, colEffect)
  residual <- matrix(mat - mu, nrow = nrow, ncol = ncol)

  a <- sum(residual * pattern) / sum(pattern^2)
  fitted <- mu + a * pattern

  return(fitted)
}
