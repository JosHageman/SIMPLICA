#' Compute best pattern-based fitness for a single Simplivariate Component
#'
#' @param mat A numeric matrix (the component)
#' @param dfMean Overall mean of the full data matrix
#' @param patternFunctions Named list of functions for structure types
#' @param penalty Named numeric vector of penalties per pattern type
#'
#' @return Numeric fitness value (higher is better)
#'
#' @examples
#' m <- matrix(rnorm(100, mean = 10), nrow = 10)
#' f <- fitnessForOneComponent(m, mean(m), defaultPatternFunctions(),
#'                c(constant = 0, additive = 1.0, multiplicative = 0))
#' @export
fitnessForOneComponent <- function(mat, dfMean, patternFunctions, penalty) {
  bestScore <- -Inf
  bestPattern <- NA

  for (pattern in names(patternFunctions)) {
    # Pattern-based approximation
    approx <- patternFunctions[[pattern]](mat)

    # Residual sum of squares
    rss <- sum((mat - approx)^2)

    # Total variation relative to global mean
    totalVar <- sum((mat - dfMean)^2)

    # Penalized fitness score
    score <- totalVar - rss - penalty[[pattern]] * rss

    if (score > bestScore) {
      bestScore <- score
      bestPattern <- pattern
    }
  }

  return(list(fitness = bestScore, pattern = bestPattern))
}
