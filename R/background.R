#' Estimate the background location and scale before a GA run
#'
#' Pools finite cells of df and compares one Gaussian with a two-Gaussian
#' mixture using BIC. The mixture is fitted by EM from several quantile starts;
#' its largest-proportion component represents the background when selected.
#' Returns signal (mean), scale (standard deviation), proportion and method.
#' Uses the single Gaussian when no valid mixture fit is available.
#' Stops when the data cannot support automatic estimation.
#' @noRd
.estimateBackground <- function(df) {
  x <- as.numeric(df)
  x <- x[is.finite(x)]
  n <- length(x)
  failureMessage <- paste0(
    "Automatic background estimation failed; supply both ",
    "backgroundSignal and backgroundScale manually."
  )
  if (n < 2L) stop(failureMessage)

  gaussianMean <- mean(x)
  gaussianVariance <- mean((x - gaussianMean)^2)
  if (!is.finite(gaussianMean) || !is.finite(gaussianVariance) ||
      gaussianVariance <= 0) {
    stop(failureMessage)
  }
  gaussianScale <- sqrt(gaussianVariance)
  gaussianLogLik <- sum(stats::dnorm(
    x, mean = gaussianMean, sd = gaussianScale, log = TRUE
  ))
  if (!is.finite(gaussianLogLik)) stop(failureMessage)
  gaussian <- list(signal = gaussianMean, scale = gaussianScale, proportion = 1, method = "gaussian")

  varianceFloor <- gaussianVariance * 1e-6
  starts <- list(
    c(0, 1),
    c(0.10, 0.90),
    c(0.20, 0.80),
    c(0.25, 0.75),
    c(1 / 3, 2 / 3),
    c(0.40, 0.60)
  )
  mixtureFits <- vector("list", length(starts))

  for (startIndex in seq_along(starts)) {
    means <- as.numeric(stats::quantile(
      x, probs = starts[[startIndex]], names = FALSE, type = 8
    ))
    if (any(!is.finite(means)) || means[1L] == means[2L]) next

    threshold <- mean(means)
    proportion <- mean(x <= threshold)
    proportion <- min(max(proportion, 0.05), 0.95)
    proportions <- c(proportion, 1 - proportion)
    variances <- rep(gaussianVariance, 2L)
    previousLogLik <- -Inf
    converged <- FALSE

    for (iteration in seq_len(500L)) {
      logDensity1 <- log(proportions[1L]) - 0.5 * (
        log(2 * pi * variances[1L]) + (x - means[1L])^2 / variances[1L]
      )
      logDensity2 <- log(proportions[2L]) - 0.5 * (
        log(2 * pi * variances[2L]) + (x - means[2L])^2 / variances[2L]
      )
      maximumLogDensity <- pmax(logDensity1, logDensity2)
      logMixtureDensity <- maximumLogDensity + log(
        exp(logDensity1 - maximumLogDensity) +
          exp(logDensity2 - maximumLogDensity)
      )
      logLik <- sum(logMixtureDensity)
      if (!is.finite(logLik)) break

      responsibility1 <- exp(logDensity1 - logMixtureDensity)
      effectiveSize1 <- sum(responsibility1)
      effectiveSize2 <- n - effectiveSize1
      if (!is.finite(effectiveSize1) || !is.finite(effectiveSize2) ||
          effectiveSize1 <= 0 || effectiveSize2 <= 0) break

      newMeans <- c(
        sum(responsibility1 * x) / effectiveSize1,
        sum((1 - responsibility1) * x) / effectiveSize2
      )
      newVariances <- c(
        sum(responsibility1 * (x - newMeans[1L])^2) / effectiveSize1,
        sum((1 - responsibility1) * (x - newMeans[2L])^2) /
          effectiveSize2
      )
      newVariances <- pmax(newVariances, varianceFloor)
      newProportion <- min(max(effectiveSize1 / n, 1e-6), 1 - 1e-6)

      if (iteration > 1L &&
          abs(logLik - previousLogLik) <= 1e-8 * (1 + abs(previousLogLik))) {
        converged <- TRUE
        proportions <- c(newProportion, 1 - newProportion)
        means <- newMeans
        variances <- newVariances
        break
      }
      previousLogLik <- logLik
      proportions <- c(newProportion, 1 - newProportion)
      means <- newMeans
      variances <- newVariances
    }

    if (!converged) next
    logDensity1 <- log(proportions[1L]) - 0.5 * (
      log(2 * pi * variances[1L]) + (x - means[1L])^2 / variances[1L]
    )
    logDensity2 <- log(proportions[2L]) - 0.5 * (
      log(2 * pi * variances[2L]) + (x - means[2L])^2 / variances[2L]
    )
    maximumLogDensity <- pmax(logDensity1, logDensity2)
    finalLogLik <- sum(maximumLogDensity + log(
      exp(logDensity1 - maximumLogDensity) +
        exp(logDensity2 - maximumLogDensity)
    ))
    if (is.finite(finalLogLik) && all(is.finite(means)) &&
        all(is.finite(variances)) && all(variances > 0) &&
        all(is.finite(proportions)) && all(proportions > 0)) {
      mixtureFits[[startIndex]] <- list(
        logLik = finalLogLik,
        means = means,
        scales = sqrt(variances),
        proportions = proportions
      )
    }
  }

  validFits <- !vapply(mixtureFits, is.null, logical(1L))
  if (!any(validFits)) {
    gaussian$method <- "gaussian_fallback"
    return(gaussian)
  }
  mixtureFits <- mixtureFits[validFits]
  bestFit <- mixtureFits[[which.max(vapply(
    mixtureFits, function(fit) fit$logLik, numeric(1L)
  ))]]

  gaussianBic <- -2 * gaussianLogLik + 2 * log(n)
  mixtureBic <- -2 * bestFit$logLik + 5 * log(n)
  if (gaussianBic <= mixtureBic) return(gaussian)

  backgroundComponent <- which.max(bestFit$proportions)
  signal <- bestFit$means[backgroundComponent]
  scale <- bestFit$scales[backgroundComponent]
  proportion <- bestFit$proportions[backgroundComponent]
  if (!is.finite(signal) || !is.finite(scale) || scale <= 0 ||
      !is.finite(proportion) || proportion <= 0 || proportion > 1) {
    stop(failureMessage)
  }
  list(
    signal = signal,
    scale = scale,
    proportion = proportion,
    method = "gaussian_mixture"
  )
}
