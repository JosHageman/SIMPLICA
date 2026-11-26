# Initialize each gene independently; no viability repair is performed.
.gaintegerPopulationFactory <- function(nGenes, zeroFraction) {
  force(nGenes)
  force(zeroFraction)
  return(function(object) {
    return(matrix(rbinom(object@popSize * nGenes, 1, 1 - zeroFraction), object@popSize, nGenes))
  })
}

# GA applies pMutation per string; this callback flips 1..maxMutatedGenes bits.
.gaintegerMutationFactory <- function(nGenes, maxMutatedGenes) {
  maxFlips <- min(nGenes, maxMutatedGenes)
  force(nGenes)
  force(maxFlips)
  return(function(object, parent) {
    string <- object@population[parent, ]
    genes <- sample.int(nGenes, sample.int(maxFlips, 1))
    string[genes] <- 1 - string[genes]
    return(string)
  })
}

#' Create an n-point crossover callback for membership chromosomes
#'
#' Validates maxCrossoverPoints once and returns a callback taking a GA object and two
#' parent indices. Samples 1 to maxCrossoverPoints cuts uniformly per crossover.
#' Exchanges alternating segments at sampled gene boundaries,
#' preserving each gene's membership code. Returns children and fitness values;
#' unchanged parent chromosomes reuse known fitness, others receive NA.
#' The caller must provide enough genes for the requested crossover points.
#' @noRd
.gaintegerNPointCrossoverFactory <- function(maxCrossoverPoints = 3) {

  if (length(maxCrossoverPoints) != 1 || !is.numeric(maxCrossoverPoints) ||
      !is.finite(maxCrossoverPoints) || maxCrossoverPoints < 1 || maxCrossoverPoints != floor(maxCrossoverPoints)) {
    stop("maxCrossoverPoints must be one positive integer.")
  }
  maxCrossoverPoints <- as.integer(maxCrossoverPoints)
  force(maxCrossoverPoints)

  # Produce two children and retain cached fitness when they match either parent.
  function(object, parents, ...) {
    parentPopulation <- object@population[parents, , drop = FALSE]

    if (identical(parentPopulation[1, ], parentPopulation[2, ])) {
      return(list(
        children = parentPopulation,
        fitness = object@fitness[parents]
      ))
    }

    nGenes <- ncol(parentPopulation)

    nPoints <- sample.int(maxCrossoverPoints, 1)
    crossoverPoints <- sort(
      sample.int(nGenes - 1L, size = nPoints, replace = FALSE)
    )
    boundaries <- c(0L, crossoverPoints, nGenes)

    children <- parentPopulation

    for (segment in seq.int(2L, length(boundaries) - 1L, by = 2L)) {
      genes <- seq.int(
        boundaries[segment] + 1L,
        boundaries[segment + 1L]
      )

      children[1, genes] <- parentPopulation[2, genes]
      children[2, genes] <- parentPopulation[1, genes]
    }

    fitness <- c(NA_real_, NA_real_)
    parentFitness <- object@fitness[parents]

    if (identical(children[1, ], parentPopulation[1, ])) {
      fitness <- parentFitness
    } else if (identical(children[1, ], parentPopulation[2, ])) {
      fitness <- rev(parentFitness)
    }

    list(
      children = children,
      fitness = fitness
    )
  }
}

#' Create a periodic GA progress callback
#'
#' Captures the reporting interval and returns a callback taking a GA object.
#' At multiples of interval it prints the generation and best current fitness;
#' the callback returns invisible NULL and does not modify the GA object.
#' @noRd
.monitorFactory <- function(interval = 100) {
  
  force(interval)

  # Report the current generation only when its reporting interval is reached.
  return(function(object) {
    if (object@iter %% interval == 0) {
      cat("Generation:", object@iter,
          "Best fitness:", round(max(object@fitness), 2), "\n")
    }
    
    return(invisible(NULL))
  })
}

# Selection excludes lethal (-Inf) individuals. Elitism is applied after fitness
# evaluation so GA's built-in elitism cannot reintroduce lethal chromosomes.
# No chromosomes are repaired or newly initialized here.
.rankSelectionFactory <- function(selectionPressure, elitism) {
  elitePopulation <- NULL
  eliteFitness <- numeric()
  selection <- function(object) {
    valid <- which(is.finite(object@fitness))
    if (!length(valid)) stop("No valid chromosomes remain in the GA population.")
    ordered <- valid[order(object@fitness[valid], decreasing = TRUE)]
    uniqueRows <- !duplicated(object@population[ordered, , drop = FALSE], MARGIN = 1)
    elites <- head(ordered[uniqueRows], elitism)
    elitePopulation <<- object@population[elites, , drop = FALSE]
    eliteFitness <<- object@fitness[elites]
    weights <- seq(selectionPressure, 2 - selectionPressure, length.out = length(ordered))
    selected <- ordered[sample.int(length(ordered), object@popSize, replace = TRUE, prob = weights)]
    return(list(population = object@population[selected, , drop = FALSE], fitness = object@fitness[selected]))
  }
  postFitness <- function(object, ...) {
    # Find absent elites once, without repeated chromosome-sized comparisons.
    if (length(eliteFitness)) {
      duplicate <- duplicated(rbind(object@population, elitePopulation), MARGIN = 1)
      missing <- which(!tail(duplicate, length(eliteFitness)))
      worst <- order(object@fitness)
      for (j in seq_along(missing)) {
        i <- missing[j]
        target <- worst[j]
        if (eliteFitness[i] <= object@fitness[target]) break
        object@population[target, ] <- elitePopulation[i, ]
        object@fitness[target] <- eliteFitness[i]
      }
    }
    return(object)
  }
  return(list(selection = selection, postFitness = postFitness))
}
