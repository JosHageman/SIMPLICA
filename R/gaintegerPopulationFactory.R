#' GA Integer Population Factory
#'
#' Creates a factory function for generating initial populations for genetic algorithms
#' with integer chromosomes, where a specified fraction of variables are set to zero.
#'
#' @param zeroFraction Numeric value between 0 and 1 specifying the fraction of variables
#'   to set to zero in each individual
#' @param verbose Logical indicating whether to print information about zero fraction
#'
#' @return A function that takes a GA object and returns an initial population matrix
#'
#' @importFrom stats rbinom
#' @export
gaintegerPopulationFactory <- function(zeroFraction, verbose = FALSE) {
  
  force(zeroFraction)

  return(function(object) {
    ups <- object@upper
    population <- matrix(as.integer(NA), nrow = object@popSize, ncol = length(ups))

    for (i in seq_len(object@popSize)) {
      for (j in seq_along(ups)) {
        population[i, j] <- sample.int(ups[j], size = 1, replace = TRUE)
      }
    }

    for (i in seq_len(object@popSize)) {
      numZeros <- rbinom(1, size = length(ups), prob = zeroFraction)
      theZeros <- sample.int(length(ups), size = numZeros)
      population[i, theZeros] <- 0
    }

    if (verbose) {
      cat(sprintf("  Zero percentage in starting population: %.1f%%\n",
                  mean(population == 0) * 100))
    }

    return(population)
  })
}
