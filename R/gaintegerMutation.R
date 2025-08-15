#' Integer mutation operator for genetic algorithms
#'
#' Applies mutation to a selected parent vector by replacing each gene with a random value
#' (within bounds) with a given mutation probability. Used in integer-encoded GAs.
#'
#' @param object A GA object containing at least the slots \code{@population},
#' \code{@upper}, \code{@lower}, and \code{@pmutation}.
#' @param parent An integer index indicating which individual in the population to mutate.
#' @param ... Further arguments (unused, included for compatibility).
#'
#' @return A numeric vector representing the mutated individual.
#'
#' @export
gaintegerMutation <- function(object, parent, ...) {

  ups <- object@upper
  lows <- object@lower
  mutate <- as.vector(object@population[parent, ])
  num <- length(mutate)

  w <- stats::runif(num) < object@pmutation

  for (i in 1:num) {
    if (w[i]) {
      mutate[i] <- sample(lows[i]:ups[i], size = 1, replace = TRUE)
    }
  }

  return(mutate)
}
