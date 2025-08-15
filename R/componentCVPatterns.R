#' Cross-Validation of Simplivariate Component Patterns
#'
#' Performs pure cross-validation over specified patterns with mandatory fitters.
#' This function evaluates different pattern fitting models using cross-validation
#' to determine the best model for a given data subset.
#'
#' @param df A matrix or data frame containing the data
#' @param rows Row indices to subset from df
#' @param cols Column indices to subset from df
#' @param patternFunctions A named list of pattern functions to evaluate
#' @param patternFitters A named list of fitter functions corresponding to each pattern
#' @param preferenceOrder Character vector specifying the preference order of patterns (default: names of patternFunctions)
#' @param nRepeats Integer, number of cross-validation repeats (default: 40)
#' @param testFraction Numeric, fraction of data to use for testing in each CV fold (default: 0.2)
#' @param minCellsForModels Integer, minimum number of cells required for reliable CV (default: 25)
#' @param parsimonyMargin Numeric, margin for parsimony selection as fraction (default: 0.05)
#' @param requireFitters Logical, whether to require fitters for all patterns (default: TRUE)
#' @param verbose Logical, whether to print progress messages (default: FALSE)
#'
#' @return A list containing:
#' \item{decision}{Character, the selected best pattern name}
#' \item{reason}{Character, explanation of the selection reasoning}
#' \item{cv}{Data frame with CV summary statistics for each model}
#' \item{repeats}{Data frame with detailed results from each CV repeat}
#' \item{meta}{List with metadata about the CV procedure}
#'
#' @export
componentCVPatterns <- function(df, rows, cols,
                                patternFunctions,
                                patternFitters,
                                preferenceOrder = names(patternFunctions),
                                nRepeats = 40,
                                testFraction = 0.2,
                                minCellsForModels = 25,
                                parsimonyMargin = 0.05,
                                requireFitters = TRUE,
                                verbose = FALSE) {
  
  # Validate input data
  stopifnot(is.matrix(df) || is.data.frame(df))
  sub <- as.matrix(df)[rows, cols, drop = FALSE]
  nR <- length(rows)
  nC <- length(cols)
  nCells <- nR * nC

  # Check that pattern functions are provided
  if (is.null(patternFunctions) || length(patternFunctions) == 0L) {
    stop("patternFunctions must be a non-empty named list.")
  }
  if (is.null(patternFitters)) {
    stop("patternFitters must be provided and contain fitters for all patterns.")
  }

  # Required: every pattern must have a fitter
  missingFitters <- setdiff(names(patternFunctions), names(patternFitters))
  if (requireFitters && length(missingFitters) > 0L) {
    stop(sprintf("No fitter defined for pattern(s): %s",
                 paste(missingFitters, collapse = ", ")))
  }

  if (is.null(preferenceOrder)) {
    preferenceOrder <- names(patternFunctions)
  } else {
    preferenceOrder <- intersect(preferenceOrder, names(patternFunctions))
  }
  if (length(preferenceOrder) == 0L) {
    stop("preferenceOrder has no overlap with patternFunctions names.")
  }

  # Early return if insufficient data for cross-validation
  if (nCells < minCellsForModels) {
    mu <- mean(sub)
    return(list(
      decision = "constant",
      reason = sprintf("too few cells (%d < %d) for reliable CV", nCells, minCellsForModels),
      cv = data.frame(model = names(patternFunctions),
                      meanRmse = c(sqrt(mean((sub - mu)^2)), rep(NA_real_, length(patternFunctions) - 1L)),
                      sdRmse = NA_real_,
                      winFrac = c(1, rep(NA_real_, length(patternFunctions) - 1L))),
      repeats = data.frame()
    ))
  }

  patNames <- names(patternFunctions)
  nPat <- length(patNames)
  rmseMat <- matrix(NA_real_, nrow = nRepeats, ncol = nPat)
  colnames(rmseMat) <- patNames

  # Root mean square error calculation
  rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

  # Function to fit and predict for one pattern
  fitPredictOne <- function(patName, m, testMask) {
    trainMask <- !testMask
    fitter <- patternFitters[[patName]]
    if (is.null(fitter)) {
      stop(sprintf("Missing fitter for pattern '%s'.", patName))
    }
    fitted <- fitter(m, trainMask)
    return(rmse(m[testMask], fitted[testMask]))
  }

  for (rep in seq_len(nRepeats)) {
    # Create test mask ensuring each row and column has at least one training observation
    testMask <- {
      total <- nR * nC
      nTest <- max(1L, round(total * testFraction))
      ok <- FALSE
      tries <- 0L
      while (!ok && tries < 200L) {
        tries <- tries + 1L
        mask <- array(FALSE, dim = c(nR, nC))
        mask[sample.int(total, nTest, replace = FALSE)] <- TRUE
        ok <- all(rowSums(!mask) >= 1L) && all(colSums(!mask) >= 1L)
      }
      if (!ok) {
        mask <- array(FALSE, dim = c(nR, nC))
        mask[sample.int(total, 1L)] <- TRUE
      }
      mask
    }

    # Fit each pattern and calculate RMSE
    for (j in seq_len(nPat)) {
      p <- patNames[j]
      rmseMat[rep, j] <- fitPredictOne(p, sub, testMask)
    }
    # Progress reporting
    if (verbose && rep %% max(1L, floor(nRepeats / 10)) == 0L) {
      message(sprintf("CV %d/%d done.", rep, nRepeats))
    }
  }

  meanVec <- colMeans(rmseMat)
  sdVec <- apply(rmseMat, 2L, stats::sd)
  wins <- apply(rmseMat, 1L, function(x) which.min(x))
  winFrac <- sapply(seq_len(nPat), function(j) mean(wins == j))

  # Create cross-validation summary
  cvSummary <- data.frame(
    model = colnames(rmseMat),
    meanRmse = as.numeric(meanVec),
    sdRmse = as.numeric(sdVec),
    winFrac = as.numeric(winFrac),
    stringsAsFactors = FALSE
  )

  # Select best model based on parsimony principle
  best <- min(meanVec)
  within <- meanVec <= (1 + parsimonyMargin) * best
  orderIdx <- match(preferenceOrder, colnames(rmseMat))
  candidates <- preferenceOrder[within[orderIdx]]
  decision <- if (length(candidates) >= 1L) candidates[1L] else names(meanVec)[which.min(meanVec)]

  reason <- sprintf(
    "pure CV: best mean RMSE = %.4f; selected simplest within %.0f%% margin",
    best, 100 * parsimonyMargin
  )

  return(list(
    decision = decision,
    reason = reason,
    cv = cvSummary,
    repeats = data.frame(repeatId = seq_len(nRepeats), rmseMat, check.names = FALSE),
    meta = list(
      nRows = nR, nCols = nC, nCells = nCells,
      nRepeats = nRepeats, testFraction = testFraction,
      parsimonyMargin = parsimonyMargin,
      preferenceOrder = preferenceOrder
    )
  ))
}
