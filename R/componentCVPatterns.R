#' Cross-Validation of Simplivariate Component Patterns
#'
#' Compares patterns on a block with fixed rows and columns. Each pattern is
#' tested on the same held-out cells. Patterns must produce valid predictions on
#' every split. The simplest pattern within `parsimonyMargin` of the lowest
#' pooled RMSE is selected. Custom functions receive a matrix whose held-out
#' cells are `NA` and must return a numeric matrix of the same size.
#'
#' @param df A numeric matrix or data frame. Missing values are allowed.
#' @param rows,cols Fixed row and column indices of the component.
#' @param patterns Built-in pattern names.
#' @param customPatterns Named custom approximation functions.
#' @param customPatternComplexity Named complexities for custom patterns.
#' @param preferenceOrder Tie-break order between equal-complexity patterns.
#' @param nRepeats Number of repeated held-out splits.
#' @param testFraction Requested fraction of usable cells to hold out.
#' @param parsimonyMargin Relative RMSE margin within which the simplest model
#'   is selected.
#' @param cvSeed Seed used to generate splits without changing the caller's RNG.
#' @param verbose Print progress messages.
#' @param splitScores Optional deterministic split priorities with dimensions
#'   `nrow(df)` by `ncol(df)` by `nRepeats`.
#'
#' @return A list containing the selected pattern (`decision`), an explanation
#'   (`reason`), pooled RMSE by pattern (`cv`), RMSE by split (`repeats`), and
#'   the settings used (`meta`). See `simplicaCV()` for evaluating fitted
#'   SIMPLICA components on their search matrices.
#' @examples
#' set.seed(1)
#' x <- outer(1:8, 1:8, "+") + matrix(rnorm(64, sd = 0.1), 8, 8)
#' cv <- componentCVPatterns(x, rows = 1:8, cols = 1:8,
#'                           patterns = c("constant", "additive"))
#' cv$decision
#' cv$cv
#'
#' @export
componentCVPatterns <- function(df, rows, cols,
                                patterns = defaultPatterns(),
                                customPatterns = list(),
                                customPatternComplexity = list(),
                                preferenceOrder = c(patterns, names(customPatterns)),
                                nRepeats = 5,
                                testFraction = 0.2,
                                parsimonyMargin = 0.02,
                                cvSeed = 1,
                                verbose = FALSE,
                                splitScores = NULL) {
  mat <- .simplicaValidateDataMatrix(df)
  rows <- .simplicaValidateIndices(rows, "rows", nrow(mat))
  cols <- .simplicaValidateIndices(cols, "cols", ncol(mat))
  nRepeats <- .simplicaValidateCVControl(
    nRepeats, testFraction, parsimonyMargin, cvSeed)
  .simplicaValidatePatterns(
    patterns, customPatterns, customPatternComplexity
  )
  patternNames <- c(patterns, names(customPatterns))
  preferenceOrder <- unique(c(intersect(preferenceOrder, patternNames), patternNames))

  .componentCVPatternsCore(
    mat, rows, cols, patterns, customPatterns, customPatternComplexity,
    preferenceOrder, nRepeats, testFraction, parsimonyMargin, cvSeed,
    verbose, splitScores)
}

# Post-GA only: fixed random splits, no split search or identifiability SVDs.
# Comparisons use the same held-out cells for both patterns within each split.
#' Compare patterns on held-out cells of a fixed component
#'
#' Takes boundary-validated data, membership indices and CV settings, with an
#' optional precomputed split priorities. Fits all eligible
#' models on shared splits. With compareToBest, ranks by pooled RMSE on common
#' valid splits and selects the simplest model within parsimonyMargin of the
#' best. Otherwise a supplied referencePattern restricts alternatives to
#' strictly simpler models. Returns decision, reason, per-model
#' CV results, repeat RMSEs and metadata. Runs after the GA and preserves caller RNG.
#' @noRd
.componentCVPatternsCore <- function(mat, rows, cols, patterns, customPatterns,
                                     customPatternComplexity, preferenceOrder,
                                     nRepeats, testFraction, parsimonyMargin,
                                     cvSeed, verbose,
                                     splitScores = NULL, referencePattern = NULL, minValidRepeats = nRepeats,
                                     compareToBest = FALSE) {
  sub <- mat[rows, cols, drop = FALSE]
  observed <- which(!is.na(sub))
  nObserved <- length(observed)
  if (!nrow(sub) || !ncol(sub)) return(list(decision = NA_character_,
    reason = "empty component", cv = data.frame(), repeats = matrix(numeric(), 0, 0),
    meta = list(nObserved = 0L, nRepeats = nRepeats)))
  patternNames <- c(patterns, names(customPatterns))
  definitions <- c(defaultPatternComplexity()[patterns], customPatternComplexity)
  # Evaluate and validate each model's parameter count for this component size.
  complexities <- vapply(patternNames, function(pattern) {
    definition <- definitions[[pattern]]
    value <- if (is.function(definition)) {
      definition(nrow(sub), ncol(sub), nObserved)
    } else definition
    .simplicaValidatePatternComplexity(value, pattern)
  }, integer(1))
  # In a fitted object, consider only the GA pattern and strictly simpler ones.
  if (!compareToBest && !is.null(referencePattern)) {
    keep <- patternNames == referencePattern |
      complexities < complexities[referencePattern]
    patternNames <- patternNames[keep]
    complexities <- complexities[keep]
  }
  nTest <- max(1L, as.integer(round(nObserved * testFraction)))
  testable <- nObserved - nTest >= complexities & nObserved > nTest
  cv <- data.frame(model = patternNames, complexity = unname(complexities),
    pooledRmse = NA_real_, nValidRepeats = 0L)
  errors <- matrix(NA_real_, nRepeats, length(patternNames),
    dimnames = list(NULL, patternNames))
  meta <- list(nObserved = nObserved, nTest = nTest, nRepeats = nRepeats,
    testFraction = testFraction, parsimonyMargin = parsimonyMargin,
    referencePattern = referencePattern, minValidRepeats = minValidRepeats)
  if (!compareToBest && !is.null(referencePattern) && length(patternNames) == 1L) {
    return(list(decision = referencePattern, reason = "no simpler pattern",
      cv = cv, repeats = matrix(numeric(), 0L, 0L), meta = meta))
  }
  if (!is.null(splitScores)) {
    if (!is.numeric(splitScores) ||
        !identical(dim(splitScores), c(nrow(mat), ncol(mat), nRepeats)) ||
        any(!is.finite(splitScores))) stop("splitScores must be a finite nrow(df) x ncol(df) x nRepeats array.")
  } else {
    hadSeed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (hadSeed) oldSeed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (hadSeed) assign(".Random.seed", oldSeed, envir = .GlobalEnv) else
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
          rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(cvSeed)
  }
  if (any(testable)) for (repeatId in seq_len(nRepeats)) {
    test <- if (is.null(splitScores)) {
      observed[sample.int(nObserved, nTest)]
    } else {
      priorities <- splitScores[rows, cols, repeatId, drop = FALSE]
      observed[order(priorities[observed])[seq_len(nTest)]]
    }
    train <- sub
    train[test] <- NA_real_
    for (j in which(testable)) {
      pattern <- patternNames[j]
      prediction <- tryCatch({
        if (pattern %in% patterns) {
          fit <- .defaultPatternFitConverged(train, pattern)
          if (fit$converged) fit$fitted else NULL
        } else customPatterns[[pattern]](train)
      }, error = function(e) NULL)
      if (is.numeric(prediction) && is.matrix(prediction) &&
          identical(dim(prediction), dim(sub)) &&
          all(is.finite(prediction[test]))) {
        errors[repeatId, j] <- sum((sub[test] - prediction[test])^2)
      }
    }
  }
  cv$nValidRepeats <- colSums(is.finite(errors))
  if (compareToBest) {
    # Prefer candidates with more valid fits when some cannot share enough splits.
    commonSplits <- rep(TRUE, nRepeats)
    candidates <- which(cv$nValidRepeats >= minValidRepeats)
    candidates <- candidates[order(cv$nValidRepeats[candidates], decreasing = TRUE)]
    compared <- integer()
    for (j in candidates) {
      shared <- commonSplits & is.finite(errors[, j])
      if (sum(shared) >= minValidRepeats) {
        compared <- c(compared, j)
        commonSplits <- shared
      }
    }
    cv$nSharedRepeats <- 0L
    cv$referenceRmse <- NA_real_
    cv$nSharedRepeats[compared] <- sum(commonSplits)
    cv$pooledRmse[compared] <- sqrt(colMeans(errors[commonSplits, compared, drop = FALSE]) / nTest)
    best <- which.min(cv$pooledRmse)
    comparable <- length(best) > 0
    eligible <- integer()
    meta["referencePattern"] <- list(NULL)
    if (comparable) {
      meta$referencePattern <- patternNames[best]
      cv$referenceRmse[compared] <- cv$pooledRmse[best]
      eligible <- which(cv$pooledRmse <= (1 + parsimonyMargin) * cv$pooledRmse[best])
    }
  } else if (is.null(referencePattern)) {
    valid <- cv$nValidRepeats == nRepeats
    cv$pooledRmse[valid] <- sqrt(colMeans(errors[, valid, drop = FALSE]) / nTest)
    reference <- if (any(valid)) min(cv$pooledRmse[valid]) else NA_real_
    eligible <- which(valid & cv$pooledRmse <= (1 + parsimonyMargin) * reference)
    comparable <- is.finite(reference)
  } else {
    referenceErrors <- errors[, match(referencePattern, patternNames)]
    shared <- is.finite(errors) & is.finite(referenceErrors)
    cv$nSharedRepeats <- colSums(shared)
    cv$referenceRmse <- NA_real_
    for (j in which(cv$nSharedRepeats >= minValidRepeats)) {
      cv$pooledRmse[j] <- sqrt(mean(errors[shared[, j], j]) / nTest)
      cv$referenceRmse[j] <- sqrt(mean(referenceErrors[shared[, j]]) / nTest)
    }
    eligible <- which(cv$pooledRmse <= (1 + parsimonyMargin) * cv$referenceRmse)
    comparable <- any(cv$nSharedRepeats[patternNames != referencePattern] >= minValidRepeats)
  }
  decision <- NA_character_
  reason <- "no valid comparison; retain the GA pattern"
  if (comparable) {
    ranked <- order(complexities[eligible], match(patternNames[eligible], preferenceOrder))
    decision <- patternNames[eligible[ranked[1L]]]
    reason <- "simplest pattern within the RMSE margin"
  }
  if (verbose) message("CV: ", if (is.na(decision)) reason else decision)
  list(decision = decision, reason = reason, cv = cv,
    repeats = sqrt(errors / nTest), meta = meta)
}
