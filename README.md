# SIMPLICA

![R-CMD-check](https://github.com/joshageman/SIMPLICA/actions/workflows/R-CMD-check.yaml/badge.svg)
![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)
![CRAN status](https://www.r-pkg.org/badges/version/SIMPLICA)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SIMPLICA)

Simplivariate Component Analysis (SIMPLICA) identifies subsets of rows and columns in numeric data matrices that follow a common mathematical pattern.

Components are extracted sequentially using a genetic algorithm. After extracting a component, its fitted contribution relative to the background is subtracted before searching for the next component.

Rows and columns may belong to multiple components. Missing observations are represented by `NA` and do not require imputation.

## Features

- Sequential component extraction and residual deflation.
- Automatic estimation of background location and scale.
- Six built-in component patterns: constant, row, column, additive, multiplicative and additive-multiplicative.
- Support for user-defined component patterns.
- Overlapping component membership.
- Direct handling of missing observations.
- Cross-validation for component pattern selection after extraction.

## Installation

The released version of SIMPLICA is available from CRAN:

```r
install.packages("SIMPLICA")
```

## Example

The package includes the dataset used in the vignette, together with precomputed analysis results and the GA settings used to obtain them.

```r
library(SIMPLICA)
data("simplicaVignette")

x <- simplicaVignette$data
settings <- simplicaVignette$info$gaSettings

analysis <- do.call(simplica, c(list(df = x), settings))
fit <- analysis$best

summaryComponents(fit)
```

SIMPLICA uses a computationally intensive genetic algorithm. Depending on the data and the GA settings, an analysis may take several minutes or longer.

The maximum number of components is specified by `numSimComp`. Component extraction stops earlier if no component with a positive fitness score is found.

The fitted components are stored in `fit$componentFits` and the final residual matrix in `fit$residuals`.

## Visualisation

```r
plotComponentResult(
    x,
    fit$string,
    fit$componentPatterns,
    fit$componentScores,
    rearrange = TRUE
)
```

## Cross-validation

Cross-validation compares alternative component patterns for the selected subsets of rows and columns. It does not change component membership or the fitted contributions used for residual deflation.

The component summary reports both the GA-selected pattern and the cross-validation suggestion.

## Documentation

The package vignette provides an introduction to SIMPLICA, the component patterns, the analysis procedure and the interpretation of results.

```r
vignette("getting-started-with-simplica", package = "SIMPLICA")
```

See [NEWS.md](NEWS.md) for changes introduced in version 2.0.0.

## Citation

```r
citation("SIMPLICA")
```

## Author

Jos Hageman  
Wageningen University & Research  
jos.hageman@wur.nl

## License

GPL-3