#' Print method for summaryComponents
#'
#' @param x An object produced by \code{summaryComponents()}.
#' @param showDetails Logical. If TRUE, also print row/column indices per component.
#' @param maxLinesDetails Integer. Max number of indices to print per dimension
#'   (rows/cols) before truncation (default 200).
#' @param ... Further arguments passed to or from other methods (not used here).
#' 
#' @return The input object \code{x}, invisibly. Called for its side effect of 
#'   printing a formatted component summary to the console.
#' 
#' @export
#' @method print summaryComponents
print.summaryComponents <- function(x, showDetails = FALSE, maxLinesDetails = 200L, ...) {
  ms <- attr(x, "matrix_size")
  if (!is.null(ms)) {
    cat("=== Component Summary ===\n")
    cat("Matrix size:", ms["nRows"], "x", ms["nCols"], "\n")
  } else {
    cat("=== Component Summary ===\n")
  }
  cat("Components shown:", nrow(x), "\n\n")

  # Print the table without the list attribute
  tab <- x
  attr(tab, "matrix_size") <- NULL
  attr(tab, "indices") <- NULL
  class(tab) <- "data.frame"
  print(tab, row.names = FALSE)
  
  if (showDetails && nrow(x) > 0L) {
    inds <- attr(x, "indices")
    cat("\n=== Component Details (row/col indices) ===\n")
    for (k in seq_len(nrow(x))) {
      compId <- x$componentId[k]
      pat  <- x$pattern[k]
      scr  <- x$score[k]
      rowIdx <- inds[[k]]$rowIdx
      colIdx <- inds[[k]]$colIdx

      cat(sprintf("Component %d (%s, score = %.2f):\n", compId, pat, scr))

      # truncation for long vectors
      if (length(rowIdx) > maxLinesDetails) {
        headRows <- rowIdx[seq_len(maxLinesDetails)]
        cat("  Rows:", paste(headRows, collapse = ", "), ", ... (truncated)\n", sep = "")
      } else {
        cat("  Rows:", if (length(rowIdx)) paste(rowIdx, collapse = ", ") else "(none)", "\n")
      }
      if (length(colIdx) > maxLinesDetails) {
        headCols <- colIdx[seq_len(maxLinesDetails)]
        cat("  Cols:", paste(headCols, collapse = ", "), ", ... (truncated)\n", sep = "")
      } else {
        cat("  Cols:", if (length(colIdx)) paste(colIdx, collapse = ", ") else "(none)", "\n")
      }
      cat("\n")
    }
  }
  return(invisible(x))
}
