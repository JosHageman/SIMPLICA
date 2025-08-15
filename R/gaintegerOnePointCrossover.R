#' One-point crossover operator for integer-encoded genetic algorithms
#'
#' Performs one-point crossover on two parent individuals. A single crossover point is selected,
#' and all genes before (and including) that point are exchanged between the parents.
#'
#' @param object A GA object with a \code{@population} slot (a matrix).
#' @param parents A 2-row matrix of values indexing the parents from the current population.
#' @param ... Further arguments (unused, included for compatibility).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{children}{A 2-row matrix of the resulting offspring.}
#'   \item{fitness}{A numeric vector of \code{NA} values to be replaced by fitness evaluation.}
#' }
#'
#' @export
gaintegerOnePointCrossover <- function(object, parents, ...) {  

  parents <- object@population[parents, , drop = FALSE]

  n2 <- ncol(parents)
  children <- parents

  crossOverPoint <- sample.int(n2, size = 1)

  swap <- 1:crossOverPoint

  children[1, swap] <- parents[2, swap]
  children[2, swap] <- parents[1, swap]

  out <- list(children = children, fitness = rep(NA, 2))
  return(out)
}
