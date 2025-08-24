#' Test Simplivariate Components with Cross-Validation Pattern Selection
#'
#' This function performs cross-validation-based pattern testing for Simplivariate Components
#' in a SIMPLICA object. It evaluates different pattern functions using cross-validation
#' and selects the best performing pattern for each component. Fitters are required
#' for all patterns with no fallback options.
#'
#' @param foundObject A simplica object containing Simplivariate Components
#' @param df Data frame or matrix with the original data
#' @param patternFunctions List of pattern functions to evaluate (default: defaultPatternFunctions())
#' @param patternFitters List of pattern fitting functions (default: defaultPatternFitters())
#' @param preferenceOrder Character vector specifying preference order for pattern selection (default: names(patternFunctions))
#' @param nRepeats Integer, number of cross-validation repeats (default: 40)
#' @param testFraction Numeric, fraction of data to use for testing (default: 0.2)
#' @param minCellsForModels Integer, minimum number of cells required for model fitting (default: 25)
#' @param parsimonyMargin Numeric, margin for parsimony-based model selection (default: 0.05)
#' @param requireFitters Logical, whether fitters are required for all patterns (default: TRUE)
#' @param updateObject Logical, whether to update and return the input object (default: TRUE)
#' @param verbose Logical, whether to print progress messages (default: FALSE)
#' @param ignoreNaComponents Logical, whether to skip components with NA patterns (default: TRUE)
#'
#' @return If \code{updateObject = TRUE}, returns the input \code{simplica} object 
#' with two new fields:
#' \describe{
#'   \item{\code{componentPatternsUpdated}}{Character vector with the selected 
#'   pattern per component after cross-validation. If a component is skipped or empty, 
#'   the entry is \code{NA}.}
#'   \item{\code{componentAudit}}{Data frame containing detailed cross-validation 
#'   results for each component, with the following columns: 
#'   \describe{
#'     \item{\code{componentId}}{Numeric ID of the component.}
#'     \item{\code{originalPattern}}{Pattern label originally assigned.}
#'     \item{\code{selectedPattern}}{Pattern chosen after CV-based evaluation.}
#'     \item{\code{reason}}{Explanation of why a pattern was selected or skipped.}
#'     \item{\code{nRows}, \code{nCols}, \code{nCells}}{Dimensions of the component.}
#'     \item{\code{nRepeats}, \code{testFraction}, \code{parsimonyMargin}}{CV settings used.}
#'     \item{\code{cvMean_<pattern>}}{Mean RMSE over CV folds for each tested pattern.}
#'     \item{\code{cvSd_<pattern>}}{Standard deviation of RMSE across CV folds.}
#'     \item{\code{winFrac_<pattern>}}{Fraction of CV repeats where the pattern was the best performer.}
#'   }}
#' }
#'
#' If \code{updateObject = FALSE}, returns a list with the same two elements
#' (\code{componentPatternsUpdated}, \code{componentAudit}).
#'
#' @details The function performs the following steps:
#' \itemize{
#'   \item Validates the input simplica object and data dimensions
#'   \item Checks that all pattern functions have corresponding fitters
#'   \item For each simplivariate component, performs cross-validation pattern evaluation
#'   \item Selects the best performing pattern based on RMSE and parsimony
#'   \item Updates component patterns and provides detailed test information
#' }
#'
#' @export

simplicaCV <- function(foundObject,
                       df,
                       patternFunctions = defaultPatternFunctions(),
                       patternFitters = defaultPatternFitters(),
                       preferenceOrder = names(patternFunctions),
                       nRepeats = 40,
                       testFraction = 0.2,
                       minCellsForModels = 25,
                       parsimonyMargin = 0.05,
                       requireFitters = TRUE,
                       updateObject = TRUE,
                       verbose = FALSE,
                       ignoreNaComponents = TRUE) {

  if (!inherits(foundObject, "simplica")) stop("Input must be a simplica object.")
  if (is.null(foundObject$string)) stop("simplica object lacks 'string'.")
  if (is.null(foundObject$nRows) || is.null(foundObject$nCols)) stop("simplica object lacks nRows/nCols.")
  if (is.null(foundObject$componentPatterns)) warning("simplica object lacks 'componentPatterns'; proceeding.")

  mat <- as.matrix(df)
  if (nrow(mat) != foundObject$nRows || ncol(mat) != foundObject$nCols) {
    stop("df dimensions do not match simplica object nRows/nCols.")
  }

  missingFitters <- setdiff(names(patternFunctions), names(patternFitters))
  if (requireFitters && length(missingFitters) > 0L) {
    stop(sprintf("No fitter defined for pattern(s): %s",
                 paste(missingFitters, collapse = ", ")))
  }

  string <- foundObject$string
  nRows <- foundObject$nRows
  nCols <- foundObject$nCols
  originalPatterns <- as.character(foundObject$componentPatterns)

  getComponentIndicesFromString <- function(string, nCols, nRows, k) {
    stopifnot(length(string) == (nCols + nRows))
    idxRows <- string[seq_len(nRows)]
    idxCols <- string[nRows + seq_len(nCols)]
    rows <- which(idxRows == k)
    cols <- which(idxCols == k)
    list(rows = rows, cols = cols)
  }

  idxRows <- string[seq_len(nRows)]
  idxCols <- string[nRows + seq_len(nCols)]
  componentIds <- sort(unique(c(idxRows, idxCols)))
  componentIds <- componentIds[componentIds != 0]

  decisions <- vector("list", length(componentIds))
  names(decisions) <- paste0("component_", componentIds)

  newPatterns <- originalPatterns

  for (i in seq_along(componentIds)) {
    k <- componentIds[i]
    idx <- getComponentIndicesFromString(string, nCols, nRows, k)
    rows <- idx$rows
    cols <- idx$cols
    nR <- length(rows)
    nC <- length(cols)

    if (verbose) message(sprintf("CV component %d (%d x %d)", k, nR, nC))

    originalPatternK <- if (!is.null(originalPatterns) && k >= 1L && k <= length(originalPatterns)) {
      originalPatterns[k]
    } else {
      NA_character_
    }

    if (ignoreNaComponents && is.na(originalPatternK)) {
      # keep updated pattern as NA
      if (!is.null(newPatterns) && k >= 1L && k <= length(newPatterns)) newPatterns[k] <- NA_character_

      decisions[[i]] <- list(
        componentId = k,
        originalPattern = originalPatternK,
        selectedPattern = NA_character_,
        reason = "skipped: original pattern NA (ignored in CV)",
        nRows = nR,
        nCols = nC,
        nCells = nR * nC,
        nRepeats = nRepeats,
        testFraction = testFraction,
        parsimonyMargin = parsimonyMargin,
        # CV summary placeholders
        cvMean_constant = NA_real_,
        cvMean_additive = NA_real_,
        cvMean_multiplicative = NA_real_,
        cvSd_constant = NA_real_,
        cvSd_additive = NA_real_,
        cvSd_multiplicative = NA_real_,
        winFrac_constant = NA_real_,
        winFrac_additive = NA_real_,
        winFrac_multiplicative = NA_real_
      )

      if (verbose) {
        message(sprintf("CV component %d skipped (NA pattern).", k))
      }
      next
    }

    if (nR * nC == 0L) {
      decisions[[i]] <- list(
        componentId = k,
        originalPattern = originalPatternK,
        selectedPattern = NA_character_,
        reason = "empty component",
        nRows = nR,
        nCols = nC,
        nCells = nR * nC,
        nRepeats = nRepeats,
        testFraction = testFraction,
        parsimonyMargin = parsimonyMargin,
        cvMean_constant = NA_real_,
        cvMean_additive = NA_real_,
        cvMean_multiplicative = NA_real_,
        cvSd_constant = NA_real_,
        cvSd_additive = NA_real_,
        cvSd_multiplicative = NA_real_,
        winFrac_constant = NA_real_,
        winFrac_additive = NA_real_,
        winFrac_multiplicative = NA_real_
      )
      next
    }

    audit <- componentCVPatterns(
      df = mat,
      rows = rows,
      cols = cols,
      patternFunctions = patternFunctions,
      patternFitters = patternFitters,
      preferenceOrder = preferenceOrder,
      nRepeats = nRepeats,
      testFraction = testFraction,
      minCellsForModels = minCellsForModels,
      parsimonyMargin = parsimonyMargin,
      requireFitters = requireFitters,
      verbose = verbose
    )

    selected <- audit$decision
    if (!is.null(newPatterns) && k >= 1L && k <= length(newPatterns)) newPatterns[k] <- selected

    cvS <- audit$cv
    decisions[[i]] <- c(
      list(
        componentId = k,
        originalPattern = originalPatternK,
        selectedPattern = selected,
        reason = audit$reason,
        nRows = audit$meta$nRows,
        nCols = audit$meta$nCols,
        nCells = audit$meta$nCells,
        nRepeats = audit$meta$nRepeats,
        testFraction = audit$meta$testFraction,
        parsimonyMargin = audit$meta$parsimonyMargin
      ),
      as.list(setNames(cvS$meanRmse, paste0("cvMean_", cvS$model))),
      as.list(setNames(cvS$sdRmse,   paste0("cvSd_", cvS$model))),
      as.list(setNames(cvS$winFrac,  paste0("winFrac_", cvS$model)))
    )
  }

  allNames <- unique(unlist(lapply(decisions, names)))

  rowList <- lapply(seq_along(decisions), function(i) {
    d <- decisions[[i]]
    if (is.null(d)) d <- list()

    if (length(d)) {
      zeroLen <- names(d)[vapply(d, function(z) length(z) == 0L, logical(1))]
      for (nm in zeroLen) d[[nm]] <- NA
    }

    missing <- setdiff(allNames, names(d))
    for (nm in missing) d[[nm]] <- NA

    tooLong <- names(d)[vapply(d, function(z) length(z) > 1L, logical(1))]
    if (length(tooLong)) {
      stop(sprintf("Non-scalar fields in decisions[[%d]]: %s",
                   i, paste(tooLong, collapse = ", ")))
    }

    d <- d[allNames]
    as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  })

  auditDf <- do.call(rbind, rowList)
  rownames(auditDf) <- NULL

  if (updateObject) {
    outObj <- foundObject
    outObj$componentPatternsUpdated <- newPatterns
    outObj$componentAudit <- auditDf
    return(outObj)
  } else {
    list(componentPatternsUpdated = newPatterns, componentAudit = auditDf)
  }
}
