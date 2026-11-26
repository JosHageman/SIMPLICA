#' Measure how broadly cellwise evidence supports a component
#'
#' Takes non-negative support weights, observed axis counts (both NULL for
#' complete data), and nObserved. Returns qCell, qRow, qColumn and qCoherence
#' in that order. The first three measure effective support fractions; coherence
#' compares joint support with the row/column fractions and is capped at one.
#' Returns four zeros for zero support. Called per screened pattern as needed.
#' @noRd
.evidenceCoherenceCore <- function(support, rowN, colN, nObserved) {
  dimensions <- dim(support)
  rowSupport <- .rowSums(support, dimensions[1], dimensions[2])
  total <- sum(rowSupport)
  if (total == 0) return(c(0, 0, 0, 0))

  columnSupport <- .colSums(support, dimensions[1], dimensions[2])
  totalSq <- total * total
  qCell <- totalSq / (nObserved * sum(support * support))
  if (is.null(rowN)) {
    qRow <- totalSq /
      (length(rowSupport) * sum(rowSupport * rowSupport))
    qColumn <- totalSq /
      (length(columnSupport) * sum(columnSupport * columnSupport))
  } else {
    qRow <- totalSq /
      (nObserved * sum(rowSupport * rowSupport / rowN))
    qColumn <- totalSq /
      (nObserved * sum(columnSupport * columnSupport / colN))
  }
  return(c(qCell, qRow, qColumn, min(1, qCell / (qRow * qColumn))))
}

#' Precompute the invariant state shared by all chromosome evaluations
#'
#' Takes validated data, background parameters, pattern
#' definitions and minimum component dimensions. Returns zero-filled data and
#' observation state, gene indices, model references, and membership/size penalty
#' tables. Called once before the GA; validateCustom controls custom-output checks
#' in subsequent fitness evaluations.
#' @noRd
.prepareFitnessControl <- function(df, dfMean, dfScale,
                                   patterns, customPatterns, customPatternComplexity,
                                   minimumRows, minimumColumns,
                                   validateCustom = FALSE) {
  nRows <- nrow(df)
  nCols <- ncol(df)
  nRowSizes <- nRows - minimumRows + 1L
  nColumnSizes <- nCols - minimumColumns + 1L
  sizeChoiceComplexity <- log(nRowSizes) + log(nColumnSizes)
  sizeChoicePenalty <- 2 * sizeChoiceComplexity
  builtInIds <- match(patterns, defaultPatterns())
  customFunctions <- unname(customPatterns)
  customNames <- names(customPatterns)
  customComplexity <- unname(customPatternComplexity[customNames])
  flags <- logical(6)
  flags[builtInIds] <- TRUE
  dfHasNA <- anyNA(df)
  if (dfHasNA) {
    W0 <- !is.na(df)
    X0 <- df
    X0[!W0] <- 0
    storage.mode(W0) <- "double"
  } else {
    W0 <- NULL
    X0 <- df
  }
  return(list(X0 = X0, W0 = W0,
       rowGenes = seq_len(nRows), colGenes = nRows + seq_len(nCols),
       dfHasNA = dfHasNA, dfMean = dfMean, dfMeanSq = dfMean * dfMean,
       invDfScaleSq = 1 / (dfScale * dfScale),
       rowMembershipComplexity = lchoose(nRows, 0:nRows),
       columnMembershipComplexity = lchoose(nCols, 0:nCols),
       sizeChoiceComplexity = sizeChoiceComplexity,
       sizeChoicePenalty = sizeChoicePenalty,
       builtInIds = builtInIds, builtInNames = patterns, builtInFlags = flags,
       customFunctions = customFunctions,
       customNames = customNames,
       customComplexity = customComplexity,
       nCustom = length(customFunctions),
       minimumRows = minimumRows, minimumColumns = minimumColumns,
       validateCustom = validateCustom))
}

# Reporting only: inactive components keep a zero score and no chosen pattern.
#' Build the diagnostic record for a skipped component
#'
#' Retains membership and size-choice complexities and penalties, records reason,
#' and sets scores to zero with unavailable model metrics marked NA.
#' Returns a list only when detailed fitness reporting is requested.
#' @noRd
.inactiveComponentDiagnostics <- function(membershipComplexity, sizeChoiceComplexity, reason) {
  return(list(evidence = NA_real_, complexity = NA_real_, patternPenalty = NA_real_,
              membershipComplexity = membershipComplexity, membershipPenalty = 2 * membershipComplexity,
              sizeChoiceComplexity = sizeChoiceComplexity, sizeChoicePenalty = 2 * sizeChoiceComplexity,
              qCell = NA_real_, qRow = NA_real_, qColumn = NA_real_, qCoherence = NA_real_,
              scoreBeforeEvidenceCoherence = 0, scoreAfterEvidenceCoherence = 0,
              active = FALSE, inactiveReason = reason))
}

# HOT PATH: one binary chromosome represents exactly one component.
# Positive model scores are weighted by Qcell times Qcoherence.
.fitnessCore <- function(string, control, returnPatterns = FALSE) {
  rowGenes <- control$rowGenes
  colGenes <- control$colGenes
  minimumRows <- control$minimumRows
  minimumColumns <- control$minimumColumns
  X0 <- control$X0
  W0 <- control$W0
  dfHasNA <- control$dfHasNA
  builtInIds <- control$builtInIds
  builtInNames <- control$builtInNames
  builtInFlags <- control$builtInFlags
  customFunctions <- control$customFunctions
  customNames <- control$customNames
  customComplexity <- control$customComplexity
  nCustom <- control$nCustom
  dfMean <- control$dfMean
  dfMeanSq <- control$dfMeanSq
  invDfScaleSq <- control$invDfScaleSq
  rowMembershipComplexity <- control$rowMembershipComplexity
  columnMembershipComplexity <- control$columnMembershipComplexity
  sizeChoiceComplexity <- control$sizeChoiceComplexity
  sizeChoicePenalty <- control$sizeChoicePenalty
  validateCustom <- control$validateCustom
  rowsInComponent <- which(string[rowGenes] == 1)
  colsInComponent <- which(string[colGenes] == 1)
  m <- length(rowsInComponent)
  n <- length(colsInComponent)
  membershipComplexity <- rowMembershipComplexity[m + 1] + columnMembershipComplexity[n + 1]
  membershipPenalty <- 2 * membershipComplexity
  if (m < minimumRows || n < minimumColumns) {
    return(.invalidComponentFitness(returnPatterns,
      membershipComplexity, sizeChoiceComplexity, "insufficient component dimensions"))
  }
  X <- X0[rowsInComponent, colsInComponent, drop = FALSE]
  W <- NULL
  incomplete <- FALSE
  if (dfHasNA) {
    W <- W0[rowsInComponent, colsInComponent, drop = FALSE]
    if (all(W != 0)) W <- NULL else incomplete <- TRUE
  }

  invalid <- FALSE
  if (incomplete) {
    rowN <- rowSums(W)
    colN <- colSums(W)
    rowX <- rowSums(X)
    sumX <- sum(rowX)
    sumX2 <- sum(X * X)
    nObserved <- sum(rowN)
    invalid <- nObserved == 0 || any(rowN < 3L) || any(colN < 3L)
  } else {
    rowN <- colN <- NULL
    nObserved <- m * n
    rowMeansX <- rowMeans(X)
    mu <- mean(rowMeansX)
    rssConstant <- sum((X - mu)^2)
  }
  if (invalid) return(.invalidComponentFitness(returnPatterns,
    membershipComplexity, sizeChoiceComplexity, "infeasible observed support"))
  if (incomplete) {
    backgroundRss <- sumX2 - 2 * dfMean * sumX + nObserved * dfMeanSq
  } else {
    mat <- X
    backgroundRss <- rssConstant + nObserved * (mu - dfMean)^2
  }
  penaltyScale <- log(max(nObserved, 2))
  bestScore <- -Inf
  if (returnPatterns) {
    bestPattern <- NA_character_
    bestEvidence <- bestComplexity <- NA_real_
    bestPatternPenalty <- NA_real_
    bestQCell <- bestQRow <- bestQColumn <- bestQCoherence <- NA_real_
    bestScoreBeforeEvidenceCoherence <- NA_real_
    bestScoreAfterEvidenceCoherence <- NA_real_
  }
  nBuiltIn <- length(builtInIds)
  nCandidates <- nBuiltIn + nCustom
  candidateRss <- rep(NA_real_, nCandidates)
  candidateEvidence <- candidateComplexity <- candidateUpperBound <-
    rep(NA_real_, nCandidates)
  builtInFits <- NULL
  customResidual <- vector("list", nCustom)

  if (incomplete) {
    observed <- W != 0
    observedValues <- X[observed]
    observedRows <- observedColumns <- NULL
  }

  if (nBuiltIn) {
    metrics <- .defaultPatternMetricsCore(
      X, W, rowN, colN, if (incomplete) sumX else 0,
      builtInFlags, builtInIds)
    rss <- metrics$rss
    builtInFits <- metrics$fits
    if (incomplete) {
      observedCells <- which(observed)
      observedRows <- (observedCells - 1L) %% m + 1L
      observedColumns <- (observedCells - 1L) %/% m + 1L
    }
    builtInCandidates <- seq_len(nBuiltIn)
    candidateRss[builtInCandidates] <- rss
    complexity <- c(1, m, n, m + n - 1, m + n - 2,
                    2 * m + 2 * n - 4)[builtInIds]
    availableBuiltIn <- which(is.finite(rss))
    evidence <- (backgroundRss - rss[availableBuiltIn]) * invDfScaleSq
    candidateEvidence[availableBuiltIn] <- evidence
    candidateComplexity[availableBuiltIn] <- complexity[availableBuiltIn]
    candidateUpperBound[availableBuiltIn] <- evidence -
      penaltyScale * complexity[availableBuiltIn] - membershipPenalty -
      sizeChoicePenalty
  }

  if (nCustom) {
    if (incomplete) {
      mat <- X
      mat[!observed] <- NA_real_
    }
    for (j in seq_len(nCustom)) {
      candidate <- nBuiltIn + j
      approximation <- customFunctions[[j]](mat)
      if (validateCustom &&
          (!is.matrix(approximation) ||
           !identical(dim(approximation), c(m, n)))) {
        stop("Pattern '", customNames[j],
             "' did not return a matrix with matching dimensions.")
      }
      if (incomplete) {
        fittedResidual <- observedValues - approximation[observed]
      } else {
        fittedResidual <- mat - approximation
      }
      currentRss <- sum(fittedResidual^2)
      candidateRss[candidate] <- currentRss
      customResidual[[j]] <- fittedResidual
      if (!is.finite(currentRss)) next
      definition <- customComplexity[[j]]
      complexity <- if (is.function(definition)) {
        value <- definition(m, n, nObserved)
        if (validateCustom) {
          .simplicaValidatePatternComplexity(value, customNames[j])
        } else {
          value
        }
      } else definition
      evidence <- (backgroundRss - currentRss) * invDfScaleSq
      candidateEvidence[candidate] <- evidence
      candidateComplexity[candidate] <- complexity
      candidateUpperBound[candidate] <-
        evidence - penaltyScale * complexity - membershipPenalty -
        sizeChoicePenalty
    }
  }

  if (nCandidates) {
    selectionBounds <- candidateUpperBound
    selectedCandidate <- rep(FALSE, nCandidates)
    backgroundSquaredResidual <- if (incomplete) {
      (observedValues - dfMean)^2
    } else (mat - dfMean)^2
    supportMatrix <- if (incomplete) matrix(0, m, n)
    bestOriginalIndex <- nCandidates + 1L

    for (selection in seq_len(nCandidates)) {
      candidate <- which.max(selectionBounds)
      if (!length(candidate)) {
        # Stable radix order puts missing bounds last in original order.
        candidate <- which(!selectedCandidate)[1]
      }
      selectedCandidate[candidate] <- TRUE
      selectionBounds[candidate] <- NA_real_
      currentRss <- candidateRss[candidate]
      if (!is.finite(currentRss)) next
      baseScore <- candidateUpperBound[candidate]
      # Equality is deliberately retained: an equal upper bound can still
      # tie after cellwise adjustment and original order must then win.
      if (baseScore < bestScore) break

      candidateQCell <- candidateQRow <- candidateQColumn <-
        candidateQCoherence <- NA_real_

      if (candidate <= nBuiltIn) {
        j <- candidate
        id <- builtInIds[j]
        candidateName <- builtInNames[j]
      } else {
        j <- candidate - nBuiltIn
        candidateName <- customNames[j]
      }

      if (baseScore > 0) {
        if (candidate <= nBuiltIn) {
          fit <- builtInFits[[id]]
          fittedResidual <- if (incomplete) {
            observedValues - (fit[[2L]] + fit[[3L]][observedRows] +
              fit[[4L]][observedColumns] +
              fit[[5L]][observedRows] * fit[[6L]][observedColumns])
          } else mat - .alsFitted(fit)
        } else {
          fittedResidual <- customResidual[[j]]
        }
        cellEvidence <- (backgroundSquaredResidual -
          fittedResidual * fittedResidual) * invDfScaleSq
        positive <- which(cellEvidence > 0)
        support <- numeric(length(cellEvidence))
        support[positive] <- -expm1(-cellEvidence[positive] / 2)
        if (incomplete) {
          if (candidate <= nBuiltIn) {
            supportMatrix[observedCells] <- support
          } else {
            supportMatrix[observed] <- support
          }
          coherence <- .evidenceCoherenceCore(
            supportMatrix, rowN, colN, nObserved
          )
        } else {
          dim(support) <- c(m, n)
          coherence <- .evidenceCoherenceCore(
            support, NULL, NULL, nObserved
          )
        }
        candidateQCell <- coherence[1]
        candidateQRow <- coherence[2]
        candidateQColumn <- coherence[3]
        candidateQCoherence <- coherence[4]
      }

      score <- if (baseScore > 0) {
        candidateQCell * candidateQCoherence * baseScore
      } else baseScore
      better <- score > bestScore
      if (!better && is.finite(score) && score == bestScore &&
          candidate < bestOriginalIndex) better <- TRUE
      if (better) {
        bestScore <- score
        bestOriginalIndex <- candidate
        if (returnPatterns) {
          bestPattern <- candidateName
          bestEvidence <- candidateEvidence[candidate]
          bestComplexity <- candidateComplexity[candidate]
          bestPatternPenalty <- penaltyScale * bestComplexity
          bestQCell <- candidateQCell
          bestQRow <- candidateQRow
          bestQColumn <- candidateQColumn
          bestQCoherence <- candidateQCoherence
          bestScoreBeforeEvidenceCoherence <- baseScore
          bestScoreAfterEvidenceCoherence <- score
        }
      }
    }
  }
  if (!is.finite(bestScore)) return(.invalidComponentFitness(returnPatterns,
    membershipComplexity, sizeChoiceComplexity, "no finite pattern fitness"))
  if (!returnPatterns) return(bestScore)
  diagnostics <- list(
    evidence = bestEvidence,
    complexity = bestComplexity,
    patternPenalty = bestPatternPenalty,
    membershipComplexity = membershipComplexity,
    membershipPenalty = membershipPenalty,
    sizeChoiceComplexity = sizeChoiceComplexity,
    sizeChoicePenalty = sizeChoicePenalty,
    qCell = bestQCell,
    qRow = bestQRow,
    qColumn = bestQColumn,
    qCoherence = bestQCoherence,
    scoreBeforeEvidenceCoherence = bestScoreBeforeEvidenceCoherence,
    scoreAfterEvidenceCoherence = bestScoreAfterEvidenceCoherence,
    active = isTRUE(bestScoreAfterEvidenceCoherence > 0)
  )

  return(list(fitness = bestScore, componentPatterns = bestPattern,
    componentScores = setNames(bestScore, "1"), componentDiagnostics = list(diagnostics)))
}

.invalidComponentFitness <- function(returnPatterns, membershipComplexity, sizeChoiceComplexity, reason) {
  if (!returnPatterns) return(-Inf)
  diagnostic <- .inactiveComponentDiagnostics(membershipComplexity, sizeChoiceComplexity, reason)
  return(list(fitness = -Inf, componentPatterns = NA_character_,
    componentScores = c("1" = -Inf), componentDiagnostics = list(diagnostic)))
}
