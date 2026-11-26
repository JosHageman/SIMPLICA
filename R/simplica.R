#' Fit SIMPLICA components
#'
#' Finds one component at a time. For each component, the genetic algorithm
#' searches for rows, columns and a pattern, using each seed in `mySeeds`.
#' SIMPLICA keeps the highest-scoring candidate. It refits the pattern and
#' subtracts the fitted values minus the background location within that block
#' before searching again. The background stays fixed throughout the fit.
#' The search stops at `numSimComp` or when the best score is zero or negative.
#' Optional cross-validation suggests patterns for interpretation after the search.
#'
#' @inheritParams simplicaCV
#' @param df Numeric matrix or data frame. Missing values are allowed.
#' @param numSimComp Maximum number of components (1 to 31).
#' @param maxIter,run,popSize,pCrossover,elitism,selectionPressure,verbose,interval GA controls. `maxIter` caps generations and `run` stops a start after that many generations without improvement.
#'   `popSize` is the population size, `pCrossover` is the crossover probability, and `elitism` caps the number of distinct elite candidates carried forward.
#'   `selectionPressure` controls rank-based selection (1 gives equal weights).
#'   With `verbose = TRUE`, progress is printed every `interval` generations.
#' @param pMutation Probability of mutating each string.
#' @param maxMutatedGenes Maximum distinct bit flips; sample uniformly from 1 to this maximum (default 5).
#' @param maxCrossoverPoints Maximum cuts, sampled uniformly per crossover (default 20); capped at string length minus one.
#' @param zeroFraction Probability of initializing a gene to zero (default 0.75).
#' @param mySeeds Seeds used anew for each component (default 1:5).
#' @param minimumRows,minimumColumns Minimum selected dimensions (default 3).
#' @param backgroundSignal,backgroundScale Background location and positive scale. Supply both to override estimation.
#' @param doSimplicaCV Whether to report cross-validated pattern suggestions after the search.
#' @param cvControl Optional tuning arguments for simplicaCV. SIMPLICA
#'   defaults to ten splits and requires seven shared valid splits. When
#'   `nRepeats` is supplied, all splits are required unless `minValidRepeats` is supplied too.
#' @return A list with `best` (a fitted SIMPLICA object) and `raw` (GA results
#'   grouped by search step and seed). Use `summaryComponents(best)` to view the
#'   retained components. The fit contains `componentFits`, the final `residuals`,
#'   `bestSeeds`, per-step `seedFitness` and a `stopReason`. Unused slots have zero
#'   scores. The total fitness is the sum of the retained component scores.
#' 
#' @section Typical analysis:
#' The following code reproduces the SIMPLICA analysis presented in
#' the package vignette. The analysis takes several minutes to complete.
#'
#' \preformatted{
#' data("simplicaVignette")
#' x <- simplicaVignette$data
#' analysis <- do.call(simplica, c(list(df = x), simplicaVignette$info$gaSettings))
#' fit <- analysis$best
#' summaryComponents(fit)
#' }
#'
#' @importFrom stats rbinom setNames
#' @importFrom utils head tail
#' @export
simplica <- function(df,
                     numSimComp,
                     maxIter = 5000,
                     run = 500,
                     popSize = 200,
                     pCrossover = 0.90,
                     pMutation = 0.20,
                     maxMutatedGenes = 5,
                     maxCrossoverPoints = 20,
                     elitism = 80,
                     verbose = FALSE,
                     mySeeds = 1:5,
                     interval = 100,
                     patterns = defaultPatterns(),
                     customPatterns = list(),
                     customPatternComplexity = list(),
                     minimumRows = 3,
                     minimumColumns = 3,
                     doSimplicaCV = TRUE,
                     cvControl = NULL,
                     backgroundSignal = NULL,
                     backgroundScale = NULL,
                     selectionPressure = 1.01,
                     zeroFraction = 0.75) {

  df <- .simplicaValidateDataMatrix(df)
  if (length(maxIter) != 1 || !is.numeric(maxIter) ||
      !is.finite(maxIter) || maxIter < 1 || maxIter != floor(maxIter)) {
    stop("maxIter must be one positive integer.")
  }
  maxIter <- as.integer(maxIter)
  if (length(run) != 1 || !is.numeric(run) ||
      !is.finite(run) || run < 1 || run != floor(run)) {
    stop("run must be one positive integer.")
  }
  run <- as.integer(run)
  if (length(popSize) != 1 || !is.numeric(popSize) ||
      !is.finite(popSize) || popSize < 2 || popSize != floor(popSize)) {
    stop("popSize must be one integer of at least 2.")
  }
  popSize <- as.integer(popSize)
  if (length(selectionPressure) != 1 || !is.numeric(selectionPressure) ||
      !is.finite(selectionPressure) || selectionPressure < 1 || selectionPressure > 2) {
    stop("selectionPressure must be one finite number between 1 and 2.")
  }
  if (length(pCrossover) != 1 || !is.numeric(pCrossover) ||
      !is.finite(pCrossover) || pCrossover < 0 || pCrossover > 1) {
    stop("pCrossover must be one finite number between 0 and 1.")
  }
  if (length(pMutation) != 1 || !is.numeric(pMutation) ||
      !is.finite(pMutation) || pMutation < 0 || pMutation > 1) {
    stop("pMutation must be one finite number between 0 and 1.")
  }
  if (length(zeroFraction) != 1 || !is.numeric(zeroFraction) ||
      !is.finite(zeroFraction) || zeroFraction < 0 || zeroFraction > 1) {
    stop("zeroFraction must be one finite number between 0 and 1.")
  }
  if (length(elitism) != 1 || !is.numeric(elitism) ||
      !is.finite(elitism) || elitism < 0 || elitism > popSize ||
      elitism != floor(elitism)) {
    stop("elitism must be one non-negative integer not exceeding popSize.")
  }
  elitism <- as.integer(elitism)
  if (length(verbose) != 1 || !is.logical(verbose) || is.na(verbose)) {
    stop("verbose must be TRUE or FALSE.")
  }
  if (length(doSimplicaCV) != 1 || !is.logical(doSimplicaCV) ||
      is.na(doSimplicaCV)) {
    stop("doSimplicaCV must be TRUE or FALSE.")
  }
  if (!is.numeric(mySeeds) || length(mySeeds) < 1 ||
      any(!is.finite(mySeeds)) || any(mySeeds != floor(mySeeds))) {
    stop("mySeeds must contain one or more finite integer values.")
  }
  mySeeds <- as.integer(mySeeds)
  if (length(interval) != 1 || !is.numeric(interval) ||
      !is.finite(interval) || interval < 1 || interval != floor(interval)) {
    stop("interval must be one positive integer.")
  }
  interval <- as.integer(interval)

  if (length(maxMutatedGenes) != 1 || !is.numeric(maxMutatedGenes) ||
      !is.finite(maxMutatedGenes) || maxMutatedGenes < 1 ||
      maxMutatedGenes != as.integer(maxMutatedGenes)) {
    stop("maxMutatedGenes must be one positive integer.")
  }
  maxMutatedGenes <- as.integer(maxMutatedGenes)
  numSimComp <- .simplicaValidateComponentCount(numSimComp)
  .simplicaValidatePatterns(
    patterns, customPatterns, customPatternComplexity)
  nRows <- nrow(df)
  nCols <- ncol(df)
  minimumRows <- .simplicaValidateMinimumDimension(
    minimumRows, "minimumRows", nRows)

  minimumColumns <- .simplicaValidateMinimumDimension(
    minimumColumns, "minimumColumns", nCols)

  .simplicaValidateBackground(backgroundSignal, backgroundScale)
  background <- if (is.null(backgroundSignal)) {
    .estimateBackground(df)
  } else {
    list(
      signal = backgroundSignal,
      scale = backgroundScale,
      proportion = NA_real_,
      method = "user"
    )
  }
  dfMean <- background$signal
  dfScale <- background$scale
  nGenes <- nRows + nCols

  if (length(maxCrossoverPoints) != 1 || !is.numeric(maxCrossoverPoints) ||
      !is.finite(maxCrossoverPoints) || maxCrossoverPoints < 1 ||
      maxCrossoverPoints != floor(maxCrossoverPoints)) {
    stop("maxCrossoverPoints must be one positive integer.")
  }
  maxCrossoverPoints <- as.integer(maxCrossoverPoints)

  maxCrossoverPoints <- min(maxCrossoverPoints, nGenes - 1)
  initialPopulation <- .gaintegerPopulationFactory(nGenes, zeroFraction)
  internalMutation <- .gaintegerMutationFactory(nGenes, maxMutatedGenes)
  internalCrossover <- .gaintegerNPointCrossoverFactory(maxCrossoverPoints)
  residuals <- df
  cvTuning <- if (doSimplicaCV) .cvSettings(cvControl, patterns, customPatterns, verbose) else NULL
  componentResults <- list()
  rawGAResults <- list()
  seedFitness <- list()
  bestSeeds <- integer()
  stopReason <- "maximum components reached"

  for (component in seq_len(numSimComp)) {
    if (verbose) cat(sprintf("Component %d of at most %d\n", component, numSimComp))
    fitnessControl <- .prepareFitnessControl(residuals, dfMean, dfScale,
      patterns, customPatterns, customPatternComplexity, minimumRows, minimumColumns)
    rawRuns <- lapply(mySeeds, function(seed) {
      if (verbose) cat(sprintf("  Seed %d\n", seed))
      operators <- .rankSelectionFactory(selectionPressure, elitism)
      return(GA::ga(type = "binary", nBits = nGenes, fitness = .fitnessCore,
        control = fitnessControl, population = initialPopulation,
        selection = operators$selection, postFitness = operators$postFitness,
        crossover = internalCrossover, mutation = internalMutation,
        parallel = FALSE, seed = seed, monitor = if (verbose) .monitorFactory(interval) else FALSE,
        pmutation = pMutation, pcrossover = pCrossover, elitism = 0,
        popSize = popSize, maxiter = maxIter, run = run))
    })
    names(rawRuns) <- as.character(mySeeds)
    rawGAResults[[component]] <- rawRuns
    values <- vapply(rawRuns, function(result) as.numeric(result@fitnessValue), numeric(1))
    seedFitness[[component]] <- setNames(values, mySeeds)
    bestSeedIndex <- which.max(values)
    if (values[bestSeedIndex] <= 0) {
      stopReason <- "best component fitness is zero or negative"
      break
    }
    sol <- rawRuns[[bestSeedIndex]]
    diagnosticControl <- fitnessControl
    diagnosticControl$validateCustom <- TRUE
    found <- .fitnessCore(as.numeric(sol@solution[1, ]), diagnosticControl, TRUE)
    found$string <- as.numeric(sol@solution[1, ])
    found$nRows <- nRows
    found$nCols <- nCols
    found$nComponents <- 1
    found$nActiveComponents <- 1
    found$membershipEncoding <- "bitmask"
    found$background <- background
    class(found) <- "simplica"
    found$componentFits <- .simplicaRefitSelectedComponents(found, residuals, customPatterns)
    componentFit <- found$componentFits[[1]]
    if (is.null(componentFit$fitted)) stop("Selected component could not be fitted for deflation.")
    rows <- componentFit$rows
    columns <- componentFit$columns
    residuals[rows, columns] <- .deflateBlock(residuals[rows, columns, drop = FALSE], componentFit$fitted, dfMean)
    componentResults[[component]] <- found
    bestSeeds[component] <- mySeeds[bestSeedIndex]
  }

  # Keep the existing bit-mask result interface; GA chromosomes themselves are binary.
  bestResult <- list(fitness = 0, componentPatterns = rep(NA_character_, numSimComp),
    componentScores = setNames(numeric(numSimComp), seq_len(numSimComp)),
    componentDiagnostics = vector("list", numSimComp), string = numeric(nGenes),
    nRows = nRows, nCols = nCols, nComponents = numSimComp,
    nActiveComponents = length(componentResults), membershipEncoding = "bitmask",
    background = background, componentFits = vector("list", numSimComp))
  masks <- .simplicaComponentMasks(numSimComp)
  for (component in seq_along(componentResults)) {
    found <- componentResults[[component]]
    bestResult$string <- bestResult$string + found$string * masks[component]
    bestResult$componentPatterns[component] <- found$componentPatterns
    bestResult$componentScores[component] <- found$componentScores
    bestResult$componentDiagnostics[[component]] <- found$componentDiagnostics[[1]]
    componentFit <- found$componentFits[[1]]
    componentFit$componentId <- component
    bestResult$componentFits[[component]] <- componentFit
  }
  bestResult$fitness <- sum(bestResult$componentScores)
  bestResult$components <- componentResults
  bestResult$residuals <- residuals
  bestResult$bestSeeds <- bestSeeds
  bestResult$seedFitness <- seedFitness
  bestResult$stopReason <- stopReason
  class(bestResult) <- "simplica"
  if (doSimplicaCV) {
    if (verbose) cat("Running post-search CV...\n")
    bestResult <- do.call(simplicaCV, c(list(foundObject = bestResult, df = df, patterns = patterns,
      customPatterns = customPatterns, customPatternComplexity = customPatternComplexity), cvTuning))
  }
  return(list(best = bestResult, raw = rawGAResults))
}

.cvSettings <- function(cvControl, patterns, customPatterns, verbose) {
  if (is.null(cvControl)) cvControl <- list()
  if (!is.list(cvControl)) stop("cvControl must be a list or NULL.")
  if (length(cvControl) > 0 &&
      (is.null(names(cvControl)) || anyNA(names(cvControl)) ||
       any(!nzchar(names(cvControl))))) {
    stop("cvControl entries must have non-empty names.")
  }
  allowedCVControl <- c(
    "preferenceOrder",
    "nRepeats",
    "minValidRepeats",
    "testFraction",
    "parsimonyMargin",
    "cvSeed",
    "verbose"
  )
  unsupported <- setdiff(names(cvControl), allowedCVControl)
  if (length(unsupported) > 0) {
    stop(
      "Unsupported cvControl field(s): ",
      paste(unsupported, collapse = ", ")
    )
  }
  cvTuning <- list(
    preferenceOrder = c(patterns, names(customPatterns)),
    nRepeats = 10,
    minValidRepeats = if (is.null(cvControl$nRepeats)) 7 else cvControl$nRepeats,
    testFraction = 0.2,
    parsimonyMargin = 0.02,
    cvSeed = 1,
    verbose = verbose
  )
  return(utils::modifyList(cvTuning, cvControl))
}

.deflateBlock <- function(block, fitted, backgroundSignal) {
  return(block - (fitted - backgroundSignal))
}
