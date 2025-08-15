#' Summarize GA-found Simplivariate Components
#'
#' Compute a tidy summary of simplivariate Components found by SIMPLICA.
#' Returns a data.frame with class \code{"summaryComponents"} and an attribute holding
#' row/column index lists for printing details.
#'
#' @param results A list or 'simplica' object with fields:
#'   \code{nRows}, \code{nCols}, \code{string}, \code{componentScores},
#'   and either \code{componentPatternsUpdated} or \code{componentPatterns}.
#' @param scoreCutoff Numeric. Minimum score to include a component (default 0).
#'
#' @return A data.frame with columns:
#'   \code{componentId}, \code{pattern}, \code{score}, \code{rows}, \code{cols}, \code{size};
#'   class is \code{c("summaryComponents","data.frame")}. The attribute
#'   \code{"indices"} stores a list with per-component \code{rowIdx} and \code{colIdx}.
#' @export
summaryComponents <- function(results, scoreCutoff = 0) {
  if (is.null(results$componentScores)) {
    stop("Object must contain 'componentScores'.")
  }
  if (!is.null(results$componentPatternsUpdated)) {
    patterns <- results$componentPatternsUpdated
  } else if (!is.null(results$componentPatterns)) {
    patterns <- results$componentPatterns
  } else {
    stop("Object must contain 'componentPatterns' or 'componentPatternsUpdated'.")
  }

  scores <- results$componentScores
  nRows  <- results$nRows
  nCols  <- results$nCols
  if (is.null(nRows) || is.null(nCols)) {
    stop("Object must contain 'nRows' and 'nCols'.")
  }

  keep <- which(scores >= scoreCutoff)
  if (length(keep) == 0L) {
    out <- data.frame(
      componentId = integer(0),
      pattern     = character(0),
      score       = numeric(0),
      rows        = integer(0),
      cols        = integer(0),
      size        = integer(0),
      stringsAsFactors = FALSE
    )
    class(out) <- c("summaryComponents", "data.frame")
    attr(out, "matrix_size") <- c(nRows = nRows, nCols = nCols)
    attr(out, "indices") <- list()  # empty
    return(out)
  }

  # Build summary and indices in one pass
  hasString <- !is.null(results$string)
  string <- results$string

  idxRows <- seq_len(nRows)
  idxCols <- nRows + seq_len(nCols)

  rowsVec <- integer(length(keep))
  colsVec <- integer(length(keep))
  sizeVec <- integer(length(keep))
  indexList <- vector("list", length(keep))

  for (j in seq_along(keep)) {
    i <- keep[j]
    if (hasString) {
      rowIdx <- which(string[idxRows] == i)
      colIdx <- which(string[idxCols] == i)
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
  return(df)
}