#' Fitness function with automatic pattern selection per Simplivariate Component
#'
#' @param string Vector with length ncol(df) + nrow(df): component labels for columns and rows.
#' @param df Numeric matrix: full data.
#' @param dfMean Scalar: global mean of df.
#' @param penalty Named vector with penalty weights per pattern.
#' @param patternFunctions Named list of functions returning pattern-based approximations.
#' @param returnPatterns Logical: if TRUE, also returns chosen pattern per component.
#' @param ... Additional arguments passed to the GA functions
#'
#' @return Either total fitness (numeric), or list(fitness, componentPatterns) if returnPatterns = TRUE.
#'
#' @export
fitness <- function(string, df, dfMean, penalty, patternFunctions = defaultPatternFunctions(),
                    returnPatterns = FALSE, ...) {

  components <- sort(unique(string[string != 0]))
  if (length(components) == 0) return(if (returnPatterns) list(fitness = 0, componentPatterns = character()) else 0)

  nComponents <- max(components)
  rows <- nrow(df)
  cols <- ncol(df)

  idxRows <- string[1:rows]
  idxCols <- string[(rows + 1):(cols + rows)]

  result <- numeric(nComponents)
  names(result) <- seq_len(nComponents)
  chosenPatterns <- character(nComponents)

  for (i in seq_len(nComponents)) {
    rowsInComponent <- which(idxRows == i)
    colsInComponent <- which(idxCols == i)

    if (length(rowsInComponent) < 3 || length(colsInComponent) < 3) {
      result[i] <- 0
      chosenPatterns[i] <- NA
      next
    }

    mat <- df[rowsInComponent, colsInComponent, drop = FALSE]

    # Automatic pattern selection
    fitObj <- fitnessForOneComponent(mat, dfMean, patternFunctions, penalty)

    result[i] <- fitObj$fitness
    chosenPatterns[i] <- fitObj$pattern
  }

  totalFitness <- sum(result)

  if (returnPatterns) {
    return(list(fitness = totalFitness,
                componentPatterns = chosenPatterns,
                componentScores = result))
  } else {
    return(totalFitness)
  }
}
