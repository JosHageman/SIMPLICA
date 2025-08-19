#' SIMPLICA: Simultaneous Identification of Simplivariate Components
#'
#' This function implements the SIMPLICA algorithm for identifying Simplivariate Components
#' in data matrices using genetic algorithms. It searches for submatrices that follow
#' specific patterns (constant, additive, multiplicative or user-defined ones) and extracts
#' them iteratively.
#'
#' @param df A numeric data matrix to analyze
#' @param maxIter Maximum number of generations for the genetic algorithm (default: 2000)
#' @param popSize Population size for the genetic algorithm (default: 200)
#' @param pCrossover Crossover probability for genetic algorithm (default: 0.5)
#' @param pMutation Mutation probability for genetic algorithm (default: 0.025)
#' @param zeroFraction Fraction of population initialized with zeros (default: 0.9)
#' @param elitism Number of best individuals preserved between generations (default: 20)
#' @param numSimComp Number of Simplivariate Components simultaneously optimized (default: 5)
#' @param verbose Logical, whether to print SIMPLICA progress information (default: FALSE)
#' @param mySeeds Vector of random seeds for replicate runs (default: 1:5)
#' @param interval Interval for monitoring GA progress (default: 100)
#' @param penalty Named vector of penalty values for each pattern type (default: c(constant = 0, additive = 1, multiplicative = 0))
#' @param patternFunctions List of pattern functions used for fitness evaluation (default: defaultPatternFunctions())
#' @param doSimplicaCV Logical, run cross-validated relabeling with simplicaCV() after GA (default: TRUE)
#' @param cvControl Optional list to tune simplicaCV; fields passed to simplicaCV via do.call.
#'   Defaults if omitted:
#'   \itemize{
#'     \item patternFitters = defaultPatternFitters()
#'     \item preferenceOrder = names(patternFunctions)
#'     \item nRepeats = 40
#'     \item testFraction = 0.2
#'     \item minCellsForModels = 25
#'     \item parsimonyMargin = 0.05
#'     \item requireFitters = TRUE
#'     \item updateObject = TRUE
#'     \item verbose = verbose
#'   }
#'
#' @return A list with:
#' \itemize{
#'   \item best: simplica object (includes original GA result; if doSimplicaCV=TRUE, also componentPatternsUpdated and componentAudit)
#'   \item raw: list of GA objects over seeds
#' }
#'
#' @examples
#' \dontrun{
#' data("simplicaToy")
#' fit <- simplica(df = simplicaToy$data, verbose = TRUE)
#' plotComponentResult(df = simplicaToy$data,
#'                     string            = fit$best$string,
#'                     componentPatterns = fit$best$componentPatternsUpdated,
#'                     componentScores   = fit$best$componentScores,
#'                     showLabels        = FALSE,
#'                     title             = "SIMPLICA on simplicaToy",
#'                     scoreCutoff       = 10000)
#' }
#'
#' @importFrom stats setNames
#' @export
simplica <- function(df,
                     maxIter = 2000,
                     popSize = 200,
                     pCrossover = 0.5,
                     pMutation = 0.025,
                     zeroFraction = 0.9,
                     elitism = 20,
                     numSimComp = 5,
                     verbose = FALSE,
                     mySeeds = 1:5,
                     interval = 100,
                     penalty = c(constant = 0, additive = 1, multiplicative = 0),
                     patternFunctions = defaultPatternFunctions(),
                     doSimplicaCV = TRUE,
                     cvControl = NULL) {

  if (!requireNamespace("GA", quietly = TRUE)) stop("Please install 'GA'.")

  if (!(is.matrix(df) || is.data.frame(df))) {
    stop("df must be a numeric matrix or data.frame.")
  }
  df <- as.matrix(df)
  if (!is.numeric(df)) stop("df must be numeric.")

  nRows <- nrow(df)
  nCols <- ncol(df)
  dfMean <- mean(df)

  if (verbose) {
    cat("Starting SIMPLICA:\n")
    cat(sprintf("  Extracting %d simplivariate Components from a %d x %d matrix\n",
                numSimComp, nRows, nCols))
  }

  rawGAResults <- lapply(seq_along(mySeeds), function(seedIndex) {
    if (verbose) {
      cat(sprintf("  Run %d of %d. (seed: %d)\n",
                  seedIndex, length(mySeeds), mySeeds[seedIndex]))
    }
    
    GA::ga(
      type        = "real-valued",
      fitness     = fitness,
      df          = df,
      dfMean      = dfMean,
      penalty     = penalty,
      patternFunctions = patternFunctions,
      zeroFraction = zeroFraction,
      lower       = rep(0, nRows + nCols),
      upper       = rep(numSimComp, nRows + nCols),
      population  = gaintegerPopulationFactory(zeroFraction, verbose = verbose),
      selection   = GA::ga_lrSelection,
      crossover   = gaintegerOnePointCrossover,
      mutation    = gaintegerMutation,
      parallel    = FALSE,
      seed        = mySeeds[seedIndex],
      monitor     = if (verbose) SCAMonitorFactory(interval) else FALSE,
      pmutation   = pMutation,
      pcrossover  = pCrossover,
      elitism     = elitism,
      popSize     = popSize,
      maxiter     = maxIter
    )
  })

  evalValues <- vapply(rawGAResults, function(x) x@fitnessValue, numeric(1))
  if (verbose) {
    cat(sprintf("  Fitness over %d seeds: %s\n",
                length(mySeeds), paste(round(evalValues, 2), collapse = ", ")))
  }

  sol <- rawGAResults[[which.max(evalValues)]]
  bestString <- as.numeric(sol@solution[1, ])

  bestResult <- fitness(bestString,
                        df = df,
                        dfMean = dfMean,
                        penalty = penalty,
                        patternFunctions = patternFunctions,
                        returnPatterns = TRUE)

  bestResult$string <- bestString
  bestResult$nRows <- nRows
  bestResult$nCols <- nCols
  class(bestResult) <- "simplica"

  if (isTRUE(doSimplicaCV)) {
    cvDefaults <- list(
      foundObject       = bestResult,
      df                = df,
      patternFunctions  = patternFunctions,
      patternFitters    = defaultPatternFitters(),
      preferenceOrder   = names(patternFunctions),
      nRepeats          = 40L,
      testFraction      = 0.2,
      minCellsForModels = 25L,
      parsimonyMargin   = 0.05,
      requireFitters    = TRUE,
      updateObject      = TRUE,
      verbose           = verbose
    )
    if (is.null(cvControl)) cvControl <- list()
    cvArgs <- utils::modifyList(cvDefaults, cvControl)
    if (verbose) cat("  Running CV...\n")
    bestResult <- do.call(simplicaCV, cvArgs)  # returns a simplica object if updateObject=TRUE
  }

  list(best = bestResult, raw = rawGAResults)
}
