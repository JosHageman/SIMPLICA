# Unchecked R kernels. Caller guarantees finite numeric matrices, W in {0,1},
# X=0 where W=0, positive observation counts on every axis, valid model flags,
# and maxIter >= 1. No validation, coercion, match.arg() or RNG here.
#
# Prepare ONCE per candidate component, then share d across model fits.
# additive=TRUE, interaction=FALSE: additive SC
# additive=FALSE, interaction=TRUE: multiplicative SC
# additive=TRUE, interaction=TRUE: additive-multiplicative SC
# Constant/row/column models keep their existing direct solutions.

#' Prepare shared statistics and starting values for component model fits
#'
#' Centers the zero-filled data matrix X using the binary observation mask W,
#' observed row/column counts rn/cn, and observed total sumX. When interaction
#' is requested, initializes rank-one factors from the strongest residual column.
#' Returns the positional state documented below, reused across candidate models.
#' Called once per candidate component; inputs must already satisfy the kernel
#' assumptions above.
#' @noRd
.alsPrepareCore <- function(X, W, interaction, rn, cn, sumX) {
  m <- nrow(X); n <- ncol(X)
  N <- sum(rn); invN <- 1 / N
  offset <- sumX * invN
  # Center once to protect the sufficient-statistic RSS from large offsets.
  Z <- if (is.null(W)) X - offset else X - W * offset
  rz <- .rowSums(Z, m, n); cz <- .colSums(Z, m, n)
  total <- sum(rz); squares <- sum(Z * Z)
  invR <- 1 / rn; invC <- 1 / cn
  mu <- total * invN
  a <- (rz - mu * rn) / n
  b <- (cz - mu * cn) / m
  zeroR <- numeric(m); zeroC <- numeric(n)
  p <- zeroR; q <- zeroC
  if (interaction) {
    R <- if (is.null(W)) Z - mu else Z - W * mu
    R <- R - a - rep(b, each = m)
    energy <- .colSums(R * R, m, n)
    p <- R[, which.max(energy)]
    p <- p - sum(p) / m
    norm <- sqrt(sum(p * p))
    if (norm > 0) {
      p <- p / norm
      q <- drop(crossprod(R, p))
      q <- q - sum(q) / n
    } else {
      p <- zeroR
    }
  }
  # Immutable positional state, shared by all model fits:
  # 1 Z, 2 W, 3/4 row/column counts, 5/6 centered row/column sums,
  # 7 total, 8 squares, 9 N, 10 1/N, 11/12 inverse axis counts,
  # 13/14 sums of inverse counts, 15/16 m/n, 17 offset, 18 RSS scale,
  # 19 complete flag, 20 initial mu, 21/22 main effects,
  # 23/24 interaction factors, 25/26 zero vectors.
  list(Z, W, rn, cn, rz, cz, total, squares, N, invN, invR, invC,
       sum(invR), sum(invC), m, n, offset,
       max(squares - total * total * invN, .Machine$double.eps),
       N == m * n, mu, a, b, p, q, zeroR, zeroC)
}

# Constrained conditional least squares. Zero denominators are a mathematical
# degeneracy, not an input check. The usual positive case allocates no zero vec.
#' Solve one zero-sum conditional least-squares factor update
#'
#' Takes diagonal quadratic weights den and right-hand side rhs. Returns a
#' factor vector summing to zero; zero-weight coordinates absorb the constraint
#' when present. Used for interaction updates with missing or unowned cells.
#' @noRd
.alsFactorCore <- function(den, rhs) {
  positive <- den > 0
  if (all(positive)) {
    inverse <- 1 / den
    (rhs - sum(rhs * inverse) / sum(inverse)) * inverse
  } else {
    value <- numeric(length(den))
    if (any(positive)) {
      value[positive] <- rhs[positive] / den[positive]
      value[!positive] <- -sum(value) / sum(!positive)
    }
    value
  }
}

#' Fit a structured component with alternating least squares
#'
#' Uses prepared state d to fit an intercept, optional row/column main effects,
#' and an optional rank-one interaction with zero-sum effects and factors.
#' Stops after maxIter updates or an RSS change within tol times the prepared
#' RSS scale. Returns the positional list of RSS, intercept, effects, factors,
#' iteration count and convergence flag, without constructing a fitted matrix.
#' Called per candidate model; GA screening uses a short iteration budget.
#' @noRd
.alsFitCore <- function(d, additive, interaction, maxIter = 3L, tol = 1e-8) {
  Z <- d[[1L]]; W <- d[[2L]]
  rn <- d[[3L]]; cn <- d[[4L]]; rz <- d[[5L]]; cz <- d[[6L]]
  total <- d[[7L]]; squares <- d[[8L]]; N <- d[[9L]]; invN <- d[[10L]]
  invR <- d[[11L]]; invC <- d[[12L]]
  sumInvR <- d[[13L]]; sumInvC <- d[[14L]]
  m <- d[[15L]]; n <- d[[16L]]; complete <- d[[19L]]
  mu <- d[[20L]]
  if (additive) { a <- d[[21L]]; b <- d[[22L]] } else { a <- d[[25L]]; b <- d[[26L]] }
  if (interaction) { p <- d[[23L]]; q <- d[[24L]] } else { p <- d[[25L]]; q <- d[[26L]] }
  twiceRz <- rz + rz; twiceCz <- cz + cz
  threshold <- tol * d[[18L]]
  done <- FALSE
  old <- 0

  # Round zero computes the initial loss using the SAME scalar loss expression.
  # No reconstructed/residual/weighted-interaction matrix in any update round.
  for (iteration in seq.int(0L, maxIter)) {
    if (iteration > 0L) {
      if (interaction) {
        if (complete) Wq <- sum(q) else Wq <- drop(W %*% q)
        interactionTotal <- sum(p * Wq)
      } else interactionTotal <- 0
      if (additive) mainTotal <- sum(rn * a) + sum(cn * b) else mainTotal <- 0
      mu <- (total - mainTotal - interactionTotal) * invN
    }

    if (additive) {
      muR <- mu * rn; muC <- mu * cn
      if (iteration > 0L) {
        if (complete) Wb <- sum(b) else Wb <- drop(W %*% b)
        rhs <- rz - muR - Wb
        if (interaction) rhs <- rhs - p * Wq
        a <- (rhs - sum(rhs * invR) / sumInvR) * invR
      }
      if (complete) Wa <- sum(a) else Wa <- drop(crossprod(W, a))
      if (iteration > 0L) {
        rhs <- cz - muC - Wa
        if (interaction) {
          if (complete) WpOld <- sum(p) else WpOld <- drop(crossprod(W, p))
          rhs <- rhs - q * WpOld
        }
        b <- (rhs - sum(rhs * invC) / sumInvC) * invC
      }
    }

    if (interaction) {
      if (iteration > 0L) {
        if (complete) denP <- sum(q * q) else denP <- drop(W %*% (q * q))
        rhsP <- drop(Z %*% q) - mu * Wq
        if (additive) {
          if (complete) Wbq <- sum(b * q) else Wbq <- drop(W %*% (b * q))
          rhsP <- rhsP - a * Wq - Wbq
        }
        if (complete) {
          if (denP > 0) p <- (rhsP - sum(rhsP) / m) / denP else p <- d[[25L]]
        } else p <- .alsFactorCore(denP, rhsP)
      }
      if (complete) {
        Wp <- sum(p)
        denQ <- sum(p * p)
      } else {
        Wp <- drop(crossprod(W, p))
        denQ <- drop(crossprod(W, p * p))
      }
      rhsQ <- drop(crossprod(Z, p)) - mu * Wp
      if (additive) {
        if (complete) Wap <- sum(a * p) else Wap <- drop(crossprod(W, a * p))
        rhsQ <- rhsQ - Wap - b * Wp
      }
      if (iteration > 0L) {
        if (complete) {
          if (denQ > 0) q <- (rhsQ - sum(rhsQ) / n) / denQ else q <- d[[26L]]
        } else q <- .alsFactorCore(denQ, rhsQ)
      }
    }

    # Expand sum W*(Z - mu - a - b - p*q)^2 using sufficient statistics.
    # Wa, rhsQ and denQ already exist for the conditional updates. Only vector
    # reductions remain; interaction RSS is evaluated BEFORE norm balancing.
    rss <- squares - mu * (2 * total - mu * N)
    if (additive) rss <- rss + sum(a * (rn * a - twiceRz + 2 * muR)) +
      sum(b * (cn * b - twiceCz + 2 * muC + 2 * Wa))
    if (interaction) rss <- rss - 2 * sum(q * rhsQ) + sum(denQ * q * q)
    rss <- max(0, rss)

    if (interaction && iteration > 0L) {
      pn <- sqrt(sum(p * p)); qn <- sqrt(sum(q * q))
      if (pn > 0 && qn > 0) {
        balance <- sqrt(qn / pn)
        p <- p * balance; q <- q / balance
      }
    }
    if (iteration > 0L && abs(old - rss) <= threshold) { done <- TRUE; break }
    old <- rss
  }
  # Unnamed result: RSS, mu, row effects, column effects, row interaction,
  # column interaction, iterations, converged. No fitted matrix allocated.
  list(rss, mu + d[[17L]], a, b, p, q, iteration, done)
}

# Optional materialization AFTER screening/selection; never needed for ALS RSS.
#' Reconstruct fitted cell values from a positional model fit
#'
#' Takes a result from .alsFitCore() or .defaultPatternDirectCore() and returns
#' the full intercept + row effect + column effect + rank-one interaction matrix.
#' Used when cellwise evidence or post-selection predictions are needed.
#' @noRd
.alsFitted <- function(fit) {
  fit[[2L]] + fit[[3L]] + rep(fit[[4L]], each = length(fit[[3L]])) +
    tcrossprod(fit[[5L]], fit[[6L]])
}

# Compile once when sourced, never during a fitness evaluation.
.alsPrepareCore <- compiler::cmpfun(.alsPrepareCore)
.alsFactorCore <- compiler::cmpfun(.alsFactorCore)
.alsFitCore <- compiler::cmpfun(.alsFitCore)