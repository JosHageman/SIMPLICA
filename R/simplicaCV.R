#' Cross-validate fitted component patterns
#'
#' Uses held-out cells to select the simplest pattern within `parsimonyMargin`
#' of the reference pattern's pooled RMSE. With `compareToBest = TRUE`, all
#' testable patterns can qualify. Patterns are included in order of valid split
#' count while at least `minValidRepeats` common splits remain. The lowest pooled
#' RMSE on those common splits sets the reference. Otherwise the GA pattern
#' sets the reference and each simpler alternative is compared on its shared splits.
#' Row and column membership, GA fitness, component
#' scores, and the original GA-selected patterns are never changed. When the
#' object is updated, fits returned by simplica() retain the fits used for deflation.
#' Standalone component objects refresh parameter fits for the CV-selected pattern.
#'
#' @param foundObject A `simplica` object with fixed component membership.
#' @param df Original numeric data matrix or data frame.
#' @param patterns Built-in patterns to compare.
#' @param customPatterns Named custom approximation functions.
#' @param customPatternComplexity Named complexities for custom patterns.
#' @param preferenceOrder Tie-break order between equal-complexity patterns.
#' @param nRepeats Number of repeated held-out splits.
#' @param minValidRepeats Minimum number of shared valid splits for comparison with the reference pattern.
#' @param compareToBest Use the lowest CV RMSE among patterns with enough valid
#'   splits as the reference. Defaults to true for a fit returned by simplica().
#' @param testFraction Requested fraction of usable cells to hold out.
#' @param parsimonyMargin Relative RMSE margin for selecting a simpler model.
#' @param cvSeed Seed for reproducible splits; the caller's RNG is preserved.
#' @param updateObject Return the input object with CV-specific fields added.
#' @param verbose Print component and repeat progress.
#'
#' @return With `updateObject = TRUE`, the unchanged input object plus
#' `componentPatternsUpdated` and `componentAudit`. Standalone component objects
#' also receive refreshed `componentFits`.
#' Otherwise a list containing only the two CV-specific results.
#' `componentPatternsUpdated` retains the original GA-selected pattern for a
#' component when CV cannot make a decision.
#'
#' @details Fits returned by simplica() are evaluated component by component on their
#' original residual trajectory. Standalone component objects use the supplied data.
#' Multi-component CV requires the original residual trajectory. Components
#' whose GA score is not positive are not cross-validated.
#'
#' @export
simplicaCV <- function(foundObject,
                       df,
                       patterns = defaultPatterns(),
                       customPatterns = list(),
                       customPatternComplexity = list(),
                       preferenceOrder = c(patterns, names(customPatterns)),
                       nRepeats = 5,
                       testFraction = 0.2,
                       parsimonyMargin = 0.02,
                       cvSeed = 1,
                       updateObject = TRUE,
                       verbose = FALSE,
                       minValidRepeats = nRepeats,
                       compareToBest = !is.null(foundObject[["components"]])) {
  stopifnot(is.numeric(minValidRepeats), length(minValidRepeats) == 1, is.finite(minValidRepeats),
            minValidRepeats == floor(minValidRepeats), minValidRepeats >= 1, minValidRepeats <= nRepeats)
  if (!is.null(foundObject[["components"]])) {
    return(.cvOnResiduals(foundObject, df, patterns, customPatterns, customPatternComplexity,
      preferenceOrder, nRepeats, testFraction, parsimonyMargin, cvSeed, updateObject, verbose, minValidRepeats, compareToBest))
  }
  if (!inherits(foundObject, "simplica")) stop("Input must be a simplica object.")
  if (is.null(foundObject$string)) stop("simplica object lacks 'string'.")
  if (is.null(foundObject$nRows) || is.null(foundObject$nCols))
    stop("simplica object lacks nRows/nCols.")
  mat <- .simplicaValidateDataMatrix(df)
  if (nrow(mat) != foundObject$nRows || ncol(mat) != foundObject$nCols)
    stop("df dimensions do not match simplica object nRows/nCols.")
  .simplicaValidatePatterns(
    patterns, customPatterns, customPatternComplexity
  )
  nRepeats <- .simplicaValidateCVControl(
    nRepeats, testFraction, parsimonyMargin, cvSeed)
  patternNames <- c(patterns, names(customPatterns))
  preferenceOrder <- unique(c(intersect(preferenceOrder, patternNames), patternNames))

  nRows <- foundObject$nRows
  nCols <- foundObject$nCols
  string <- as.integer(foundObject$string)
  nComponents <- .simplicaValidateComponentCount(foundObject$nComponents)
  if (nComponents != 1) stop("Multi-component CV requires a SIMPLICA result with its residual trajectory.")
  if (length(string) != nRows + nCols)
    stop("simplica membership string length does not match nRows + nCols.")
  .simplicaValidateMembership(string, nComponents)
  componentScores <- foundObject$componentScores
  if (is.null(componentScores) || length(componentScores) < nComponents) {
    stop("simplica object lacks a complete 'componentScores' vector.")
  }
  originalPatterns <- as.character(foundObject$componentPatterns)
  if (length(originalPatterns) < nComponents)
    originalPatterns <- c(originalPatterns,
      rep(NA_character_, nComponents - length(originalPatterns)))

  updatedPatterns <- originalPatterns
  componentAudit <- vector("list", nComponents)
  names(componentAudit) <- paste0("component_", seq_len(nComponents))

  component <- as.integer(1)
  if (!isTRUE(componentScores[component] > 0)) {
    componentAudit[[component]] <- list(
      decision = NA_character_,
      reason = "inactive component",
      active = FALSE,
      originalPattern = originalPatterns[component],
      componentId = component
    )
  } else if (is.na(originalPatterns[component]) ||
      !originalPatterns[component] %in% patternNames) {
    componentAudit[[component]] <- list(decision = NA_character_,
      reason = "GA pattern unavailable for comparison", active = TRUE,
      originalPattern = originalPatterns[component], componentId = component)
  } else {
    indices <- .simplicaComponentIndices(string, nRows, nCols, component)
    rows <- indices$rows
    cols <- indices$cols
    if (verbose) message(sprintf("CV component %d (%d x %d)", component,
      length(rows), length(cols)))

    audit <- .componentCVPatternsCore(
      mat = mat, rows = rows, cols = cols, patterns = patterns,
      customPatterns = customPatterns,
      customPatternComplexity = customPatternComplexity,
      preferenceOrder = preferenceOrder, nRepeats = nRepeats,
      testFraction = testFraction, parsimonyMargin = parsimonyMargin,
      cvSeed = cvSeed, verbose = verbose,
      referencePattern = originalPatterns[component], minValidRepeats = minValidRepeats, compareToBest = compareToBest)
    audit$originalPattern <- originalPatterns[component]
    audit$componentId <- component
    audit$active <- TRUE
    if (!is.na(audit$decision)) {
      updatedPatterns[component] <- audit$decision
    }
    componentAudit[[component]] <- audit
  }

  if (!isTRUE(updateObject)) return(list(
    componentPatternsUpdated = updatedPatterns,
    componentAudit = componentAudit))
  answer <- foundObject
  answer$componentPatternsUpdated <- updatedPatterns
  answer$componentAudit <- componentAudit
  answer$componentFits <- .simplicaRefitSelectedComponents(
    answer, mat, customPatterns
  )
  answer
}

.cvOnResiduals <- function(foundObject, df, patterns, customPatterns, customPatternComplexity,
                           preferenceOrder, nRepeats, testFraction, parsimonyMargin, cvSeed, updateObject, verbose, minValidRepeats, compareToBest) {
  residuals <- .simplicaValidateDataMatrix(df)
  if (!identical(dim(residuals), c(foundObject$nRows, foundObject$nCols))) stop("df dimensions do not match simplica object.")
  answer <- foundObject
  answer$componentPatternsUpdated <- foundObject$componentPatterns
  answer$componentAudit <- lapply(seq_len(foundObject$nComponents), function(i) list(decision = NA_character_, active = FALSE))
  for (i in seq_along(foundObject$components)) {
    original <- foundObject$components[[i]]
    updated <- simplicaCV(original, residuals, patterns, customPatterns, customPatternComplexity,
      preferenceOrder, nRepeats, testFraction, parsimonyMargin, cvSeed, FALSE, verbose, minValidRepeats, compareToBest)
    answer$componentPatternsUpdated[i] <- updated$componentPatternsUpdated
    audit <- updated$componentAudit[[1]]
    audit$componentId <- i
    answer$componentAudit[[i]] <- audit
    answer$components[[i]]$componentPatternsUpdated <- updated$componentPatternsUpdated
    answer$components[[i]]$componentAudit <- updated$componentAudit
    # Replay the fitted deflation to evaluate the next component on its search residuals.
    fit <- original$componentFits[[1]]
    residuals[fit$rows, fit$columns] <- .deflateBlock(residuals[fit$rows, fit$columns, drop = FALSE], fit$fitted, foundObject$background$signal)
  }
  if (!updateObject) return(list(componentPatternsUpdated = answer$componentPatternsUpdated,
    componentAudit = answer$componentAudit))
  return(answer)
}
