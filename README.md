# SIMPLICA <img src="man/figures/logo.png" align="right" height="120" />

[![R-CMD-check](https://github.com/joshageman/SIMPLICA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joshageman/SIMPLICA/actions)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

**Simplivariate Component Analysis** for detecting constant, additive, multiplicative, and user-defined biclusters in numeric data matrices.  
The method uses a genetic algorithm to simultaneously identify multiple simplivariate components, with flexible pattern definitions and built-in visualization tools.  
SIMPLICA is designed for applications in **life sciences** and **chemometrics** where meaningful patterns are embedded in complex data.

Before SIMPLICA
[<img src="https://github.com/user-attachments/assets/bc2e6154-2198-4cec-b9b7-f4373140a872" width="500"/>]([image.png](https://github.com/user-attachments/assets/bc2e6154-2198-4cec-b9b7-f4373140a872))

[<img src="https://github.com/user-attachments/assets/5f700440-b06a-40ab-9ab9-56ea1bc0df63" width="500"/>]([image.png](https://github.com/user-attachments/assets/5f700440-b06a-40ab-9ab9-56ea1bc0df63))

## Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("joshageman/SIMPLICA")
```

Once on CRAN:

```r
install.packages("SIMPLICA")
```

## Example

This example uses the built-in `simplicaToy` dataset.

```r
library(SIMPLICA)

# Load toy dataset
data("simplicaToy")

# Run SIMPLICA
fit <- simplica(
  df = simplicaToy$data,
  verbose = TRUE
)

# Print summary of found components
summary(fit)

# Plot detected clusters
plotClusterResult(
  df = simplicaToy$data,
  string = fit$best$string,
  clusterPatterns = fit$best$clusterPatterns,
  clusterScores = fit$best$clusterScores
)
```

## Features

- Detects **constant**, **additive**, **multiplicative**, and **user-defined** patterns.
- Simultaneous optimization of multiple biclusters via **genetic algorithms**.
- Flexible penalty settings to control overfitting.
- Customizable pattern functions.
- Publication-ready visualizations.
- Suitable for small to medium-sized datasets typical in life sciences and chemometrics.

## Citation

If you use SIMPLICA in your research, please cite:

```
Hageman, J. (2025). SIMPLICA: Simplivariate Component Analysis for Biclustering. R package version 1.0.0.
```

## License

GPL-3 © [Jos Hageman](https://github.com/joshageman)
