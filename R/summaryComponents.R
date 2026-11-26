#' Summarize fitted components
#'
#' Summarizes the fitted pattern, score and membership of each component.
#' Returns a data.frame with class \code{"summaryComponents"} and an attribute holding
#' row/column index lists for printing details.
#'
#' @param results A list or 'simplica' object with fields:
#'   \code{nRows}, \code{nCols}, \code{string}, \code{componentScores},
#'   and either \code{componentPatternsUpdated} or \code{componentPatterns}.
#'   The membership string uses bit masks and can assign a row or column to
#'   multiple components.
#' @param scoreCutoff Numeric. Minimum score to include a component. At the
#'   default of 0, only strictly positive components are included; use a lower
#'   value to inspect inactive or negative component slots.
#'
#' @return A data.frame with columns:
#'   \code{componentId}, \code{pattern}, \code{gaPattern}, \code{cvPattern},
#'   \code{cvChanged}, \code{score}, \code{rows}, \code{cols}, and \code{size}.
#'   The \code{pattern} column is the fitted pattern used for deflation in a
#'   fit returned by simplica(), or the effective pattern after CV for a standalone component.
#'   \code{gaPattern} is the original GA selection and \code{cvPattern} is the CV decision.
#'   When CV cannot decide, the fitted pattern is still reported while
#'   \code{cvPattern} and \code{cvChanged} are \code{NA}. They are also \code{NA}
#'   when CV was not run.
#'   class is \code{c("summaryComponents","data.frame")}. The attribute
#'   \code{"indices"} stores a list with per-component \code{rowIdx} and \code{colIdx}.
#'   The logical attribute \code{"cv_run"} records whether CV was run.
#'   The \code{size} column is the full row-by-column rectangle and does not
#'   subtract original missing values or cells shared by multiple components.
#'
#' @examples
#' data("simplicaVignette")
#'
#' componentSummary <- summaryComponents(simplicaVignette$fit)
#' componentSummary
#'
#' componentId <- 1
#' membership <- attr(componentSummary, "indices")[[
#'   match(componentId, componentSummary$componentId)
#' ]]
#' membership$rowIdx
#' membership$colIdx
#'
#' @export
summaryComponents <- function(results, scoreCutoff = 0) {
  if (is.null(results$componentScores)) {
    stop("Object must contain 'componentScores'.")
  }
  scores <- results$componentScores

  # Align pattern labels with score slots: truncate extras, convert to character,
  # and fill missing slots with NA. Returns one label per component score.
  padPatterns <- function(x) {
    padded <- rep(NA_character_, length(scores))
    n <- min(length(x), length(padded))
    if (n > 0) padded[seq_len(n)] <- as.character(x[seq_len(n)])
    padded
  }

  gaPattern <- padPatterns(results$componentPatterns)
  cvRun <- !is.null(results$componentAudit)
  if (!is.null(results[["components"]])) {
    fittedPatterns <- vapply(results$components, function(component) component$componentFits[[1]]$pattern, character(1))
    patterns <- padPatterns(fittedPatterns)
  } else if (!is.null(results$componentPatternsUpdated)) {
    patterns <- padPatterns(results$componentPatternsUpdated)
  } else if (!is.null(results$componentPatterns)) {
    patterns <- gaPattern
  } else {
    stop("Object must contain 'componentPatterns' or 'componentPatternsUpdated'.")
  }
  cvPattern <- rep(NA_character_, length(scores))
  if (cvRun) {
    # Extract the first CV decision per audit, using NA when no decision exists.
    auditDecisions <- vapply(results$componentAudit, function(audit) {
      if (length(audit$decision)) as.character(audit$decision[1]) else NA_character_
    }, character(1))
    cvPattern <- padPatterns(auditDecisions)
  }
  cvChanged <- rep(NA, length(scores))
  comparable <- !is.na(gaPattern) & !is.na(cvPattern)
  cvChanged[comparable] <- gaPattern[comparable] != cvPattern[comparable]

  nRows  <- results$nRows
  nCols  <- results$nCols
  if (is.null(nRows) || is.null(nCols)) {
    stop("Object must contain 'nRows' and 'nCols'.")
  }

  keep <- which(if (scoreCutoff == 0) scores > 0 else scores >= scoreCutoff)
  if (length(keep) == 0) {
    out <- data.frame(
      componentId = integer(0),
      pattern     = character(0),
      gaPattern   = character(0),
      cvPattern   = character(0),
      cvChanged   = logical(0),
      score       = numeric(0),
      rows        = integer(0),
      cols        = integer(0),
      size        = integer(0),
      stringsAsFactors = FALSE
    )
    class(out) <- c("summaryComponents", "data.frame")
    attr(out, "matrix_size") <- c(nRows = nRows, nCols = nCols)
    attr(out, "indices") <- list()  # empty
    attr(out, "cv_run") <- cvRun
    return(out)
  }

  # Build summary and indices in one pass
  hasString <- !is.null(results$string)
  string <- results$string

  nComponents <- results$nComponents
  if (is.null(nComponents)) nComponents <- length(scores)
  nComponents <- .simplicaValidateComponentCount(nComponents)
  if (hasString) {
    if (length(string) != nRows + nCols) {
      stop("Membership string length must equal nRows + nCols.")
    }
    .simplicaValidateMembership(string, nComponents)
  }
  componentMasks <- .simplicaComponentMasks(nComponents)

  idxRows <- seq_len(nRows)
  idxCols <- nRows + seq_len(nCols)

  rowsVec <- integer(length(keep))
  colsVec <- integer(length(keep))
  sizeVec <- integer(length(keep))
  indexList <- vector("list", length(keep))

  for (j in seq_along(keep)) {
    i <- keep[j]
    if (hasString) {
      rowIdx <- which(bitwAnd(as.integer(string[idxRows]), componentMasks[i]) != 0L)
      colIdx <- which(bitwAnd(as.integer(string[idxCols]), componentMasks[i]) != 0L)
    } else {
      rowIdx <- integer(0)
      colIdx <- integer(0)
    }
    rowsVec[j] <- length(rowIdx)
    colsVec[j] <- length(colIdx)
    sizeVec[j] <- rowsVec[j] * colsVec[j]
    indexList[[j]] <- list(componentId = i, rowIdx = rowIdx, colIdx = colIdx)
  }

  df <- data.frame(
    componentId = keep,
    pattern     = patterns[keep],
    gaPattern   = gaPattern[keep],
    cvPattern   = cvPattern[keep],
    cvChanged   = cvChanged[keep],
    score       = scores[keep],
    rows        = rowsVec,
    cols        = colsVec,
    size        = sizeVec,
    stringsAsFactors = FALSE
  )

  # Sort by size desc, then score desc, then componentId asc
  ord <- order(-df$size, -df$score, df$componentId)
  df <- df[ord, , drop = FALSE]
  indexList <- indexList[ord]

  class(df) <- c("summaryComponents", "data.frame")
  attr(df, "matrix_size") <- c(nRows = nRows, nCols = nCols)
  attr(df, "indices") <- indexList
  attr(df, "cv_run") <- cvRun
  return(df)
}
