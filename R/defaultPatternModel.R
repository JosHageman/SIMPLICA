#' Names of the default SIMPLICA patterns
#'
#' @return A character vector containing the six default pattern names.
#' @examples
#' defaultPatterns()
#' @importFrom compiler cmpfun
#' @export
defaultPatterns <- function() {
  c("constant", "row", "column", "additive", "multiplicative",
    "additive_multiplicative")
}

# Unchecked direct solutions, with the same positional output as .alsFitCore.
#' Solve a constant, row or column pattern directly
#'
#' Takes prepared ALS state d and default pattern id 1, 2 or 3. Returns exact
#' least-squares coefficients and RSS in the same positional format as
#' .alsFitCore(), with zero iterations and a TRUE convergence flag.
#' @noRd
.defaultPatternDirectCore <- function(d, id) {
  mu <- d[[7L]] * d[[10L]]
  a <- d[[25L]]; b <- d[[26L]]
  rss <- d[[8L]] - d[[7L]] * mu
  if (id == 2L) {
    means <- d[[5L]] * d[[11L]]
    mu <- mean(means)
    a <- means - mu
    rss <- d[[8L]] - sum(d[[5L]] * means)
  } else if (id == 3L) {
    means <- d[[6L]] * d[[12L]]
    mu <- mean(means)
    b <- means - mu
    rss <- d[[8L]] - sum(d[[6L]] * means)
  }
  list(max(0, rss), mu + d[[17L]], a, b, d[[25L]], d[[26L]], 0L, TRUE)
}

# HOT PATH: prepare once per component and share across requested models.
# A NULL W denotes complete data; model fitting uses the same ALS updates.
#' Screen requested default patterns for one candidate component
#'
#' Takes zero-filled data X, observation mask W (NULL for complete data),
#' observed axis counts and total, and default-model flags/ids. Prepares shared
#' statistics once, uses direct solutions or at most three ALS updates per model,
#' and returns RSS in ids order plus positional fits indexed by default model id.
#' Inputs are prepared by the fitness core and are not validated here.
#' @noRd
.defaultPatternMetricsCore <- function(X, W, rowN, colN, sumX, flags, ids) {
  if (is.null(W)) {
    m <- nrow(X); n <- ncol(X)
    rowN <- rep.int(n, m); colN <- rep.int(m, n)
    sumX <- sum(X)
  }
  d <- .alsPrepareCore(X, W, flags[5L] || flags[6L], rowN, colN, sumX)
  fits <- vector("list", 6L)
  rss <- numeric(length(ids))
  for (j in seq_along(ids)) {
    id <- ids[j]
    fit <- if (id <= 3L) {
      .defaultPatternDirectCore(d, id)
    } else .alsFitCore(d, id != 5L, id != 4L, 3L, 1e-8)
    fits[[id]] <- fit
    rss[j] <- fit[[1L]]
  }
  list(rss = rss, fits = fits)
}

# Boundary adapter for CV and post-selection parameter estimates.
# The fitting implementation is shared with the GA; only the budget differs.
#' Fit a default pattern for cross-validation or parameter reporting
#'
#' Accepts a matrix with NA for unavailable cells and one default pattern name.
#' Checks observed support, then uses the shared direct/ALS kernels with the
#' supplied convergence tolerance and iteration budget. Returns named effects,
#' normalized interaction factors and scale, fitted values, RSS and convergence
#' details, restoring the original matrix dimensions after removing empty axes.
#' @noRd
.defaultPatternFitConverged <- function(mat, pattern, tol = 1e-8,
                                         maxIter = 500) {
  id <- match(pattern, defaultPatterns())
  if (length(id) != 1L || is.na(id)) stop("Unknown default pattern: ", pattern)
  observed <- !is.na(mat)
  W <- observed
  storage.mode(W) <- "double"
  rowN <- rowSums(W); colN <- colSums(W)
  if (!sum(rowN) || (id == 2L && any(rowN == 0)) ||
      (id == 3L && any(colN == 0)) ||
      (id >= 4L && (any(rowN == 0) || any(colN == 0)))) {
    stop("Insufficient observed cells for the requested pattern.")
  }
  # Constant/row/column fits may have empty axes that carry no fitted effect.
  # Remove those axes only at this boundary and restore the original shape.
  keepRows <- rowN > 0; keepColumns <- colN > 0
  X <- mat[keepRows, keepColumns, drop = FALSE]
  W <- W[keepRows, keepColumns, drop = FALSE]
  X[W == 0] <- 0
  rowN <- rowN[keepRows]; colN <- colN[keepColumns]
  d <- .alsPrepareCore(X, W, id >= 5L, rowN, colN, sum(X))
  fit <- if (id <= 3L) {
    .defaultPatternDirectCore(d, id)
  } else .alsFitCore(d, id != 5L, id != 4L, maxIter, tol)
  a <- p <- numeric(nrow(mat)); b <- q <- numeric(ncol(mat))
  a[keepRows] <- fit[[3L]]; b[keepColumns] <- fit[[4L]]
  p[keepRows] <- fit[[5L]]; q[keepColumns] <- fit[[6L]]
  fit[[3L]] <- a; fit[[4L]] <- b; fit[[5L]] <- p; fit[[6L]] <- q
  pNorm <- sqrt(sum(p * p)); qNorm <- sqrt(sum(q * q))
  interactionScale <- pNorm * qNorm
  if (interactionScale == 0) {
    interactionRows <- numeric(nrow(mat))
    interactionColumns <- numeric(ncol(mat))
  } else {
    interactionRows <- p / pNorm
    interactionColumns <- q / qNorm
    if (interactionRows[which.max(abs(interactionRows))] < 0) {
      interactionRows <- -interactionRows
      interactionColumns <- -interactionColumns
    }
  }
  list(mu = fit[[2L]], rowEffects = fit[[3L]], columnEffects = fit[[4L]],
       interactionScale = interactionScale, interactionRows = interactionRows,
       interactionColumns = interactionColumns, fitted = .alsFitted(fit),
       rss = fit[[1L]], converged = fit[[8L]], iterations = fit[[7L]])
}
