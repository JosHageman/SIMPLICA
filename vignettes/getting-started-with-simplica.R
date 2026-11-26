## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 4.5, dpi = 120)

## ----data---------------------------------------------------------------------
library(SIMPLICA)
data("simplicaVignette")

x <- simplicaVignette$data
dim(x)
mean(is.na(x))

## ----dataPlot, fig.alt="Heatmap of the original simulated matrix, before biclustering.", out.width="100%"----
oldPar <- par(bg = "grey85")
image(seq_len(ncol(x)), seq_len(nrow(x)), t(as.matrix(x)[nrow(x):1, , drop = FALSE]),
      col = hcl.colors(100, "Viridis"),
      xlab = "Column", ylab = "Row", yaxt = "n")
ticks <- unique(round(seq(1, nrow(x), length.out = 4)))
axis(2, at = ticks, labels = nrow(x) + 1 - ticks)
par(oldPar)

## ----patternPlot, fig.height=5, fig.alt="Six ideal component patterns shown on a common colour scale.", out.width="100%"----
a <- seq(-2, 2, length.out = 8)
b <- seq(-2, 2, length.out = 10)

patternMatrices <- list(
  constant = matrix(1, nrow = length(a), ncol = length(b)),
  row = outer(a, rep(1, length(b))),
  column = outer(rep(1, length(a)), b),
  additive = outer(a, b, "+"),
  multiplicative = outer(a, b),
  additive_multiplicative = outer(a, b, "+") + outer(a, b)
)

oldPar <- par(mfrow = c(2, 3), mar = c(1, 1, 3, 1))
limits <- range(unlist(patternMatrices))
for (pattern in names(patternMatrices)) {
  m <- patternMatrices[[pattern]]
  image(t(m[nrow(m):1, ]), zlim = limits,
        col = hcl.colors(100, "Blue-Red 3"),
        axes = FALSE, xlab = "", ylab = "",
        main = gsub("_", "-", pattern), cex.main = 0.9)
  box()
}
par(oldPar)

## ----defaultFit, eval=FALSE---------------------------------------------------
# analysis <- simplica(df = x, numSimComp = 6)
# fit <- analysis$best

## ----reproduce, eval=FALSE----------------------------------------------------
# analysis <- simplica(
#   df = x,
#   numSimComp = 6,
#   mySeeds = 1:4,
#   popSize = 100,
#   elitism = 40,
#   maxIter = 500,
#   run = 150
# )
# 
# fit <- analysis$best

## ----fit----------------------------------------------------------------------
fit <- simplicaVignette$fit

## ----summary------------------------------------------------------------------
componentSummary <- summaryComponents(fit)
componentSummary

## ----members------------------------------------------------------------------
membership <- attr(componentSummary, "indices")[[1]]
membership$rowIdx
membership$colIdx

## ----subset-------------------------------------------------------------------
xComponent <- x[membership$rowIdx, membership$colIdx, drop = FALSE]
dim(xComponent)

## ----plot, fig.alt="Original matrix with rows and columns rearranged and the extracted components outlined.", out.width="100%"----
plotComponentResult(x, fit$string, fit$componentPatterns,
                    fit$componentScores, rearrange = TRUE,
                    showAxisLabels = FALSE)

## ----patterns-----------------------------------------------------------------
componentSummary[, c("componentId", "gaPattern", "cvPattern")]

## ----search-------------------------------------------------------------------
fit$seedFitness
fit$bestSeeds
fit$stopReason

## ----residuals----------------------------------------------------------------
range(x, na.rm = TRUE)
range(fit$residuals, na.rm = TRUE)

## ----residualPlot, fig.height=4, fig.alt="Original and remaining matrices on the same colour scale.", out.width="100%"----
oldPar <- par(mfrow = c(1, 2), mar = c(4, 2, 3, 1))
limits <- range(x, fit$residuals, na.rm = TRUE)
palette <- hcl.colors(100, "Viridis")

image(t(as.matrix(x)[nrow(x):1, , drop = FALSE]), zlim = limits, col = palette,
      axes = FALSE, xlab = "Columns", ylab = "",
      main = "Original matrix")
box()
image(t(as.matrix(fit$residuals)[nrow(x):1, , drop = FALSE]), zlim = limits, col = palette,
      axes = FALSE, xlab = "Columns", ylab = "",
      main = "Remaining matrix")
box()
par(oldPar)

