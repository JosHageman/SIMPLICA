# SIMPLICA 2.0.0

## Component extraction

- Replaced simultaneous component extraction with sequential component extraction and deflation.
- Components are extracted one at a time by subtracting their fitted contribution relative to the background.
- Component extraction stops when the requested maximum number of components is reached or no component with a positive fitness score is found.
- Introduced automatic estimation of background location and scale using Gaussian mixture modelling and BIC.
- Background location and scale can also be supplied by the user and remain fixed throughout the analysis.

## Fitness function

- Redesigned the fitness function.
- Component scores account for improvement over the background model, model complexity, component size and row and column membership complexity.
- Introduced measures of cellwise evidence and evidence coherence to assess the distribution of support within a component.

## Component models and fitting

- Six built-in component patterns: constant, row, column, additive, multiplicative and additive-multiplicative.
- Introduced a new alternating least squares (ALS) implementation.
- Complete and incomplete data are handled using the same model-fitting approach.
- Missing observations are handled directly without imputation.
- Component fitting during the GA search and subsequent parameter estimation use the same fitting routines with different iteration limits.
- Revised support for user-defined component patterns and their associated model complexity.

## Genetic algorithm

- Replaced multi-component chromosomes with binary chromosomes representing the row and column membership of a single component.
- Redesigned population initialization, selection, crossover, mutation and elitism.
- Added control over selection pressure, the maximum number of mutated genes and the maximum number of crossover points.
- Multiple random seeds are evaluated for each component extraction.
- Revised default GA settings based on a simulation-based experimental design.

## Cross-validation

- Redesigned cross-validation as a separate step following component extraction.
- Pattern comparisons are based on repeated cellwise holdout and pooled prediction RMSE.
- Simpler patterns can be selected using a configurable parsimony margin.
- Patterns are compared using shared valid cross-validation splits.
- Cross-validation does not change component membership, GA fitness scores or the fitted contributions used for deflation.
- Cross-validation can be performed automatically or separately using `simplicaCV()`.

## User interface and output

- Revised the arguments and return structure of `simplica()`.
- Fitted component parameters, final residuals, seed-specific fitness scores and stopping reasons are included in the results.
- Updated `summaryComponents()` to report both GA-selected and cross-validated patterns.
- Updated `plotComponentResult()` for the revised component representation.
- Reduced the number of exported functions. Internal fitting and GA functions are no longer part of the public interface.
- Replaced `defaultPatternFunctions()` and `defaultPatternFitters()` with `defaultPatterns()` and `defaultPatternComplexity()`.

## Documentation and examples

- Revised function documentation.
- Added a package vignette describing a complete SIMPLICA analysis.
- Replaced the previous example dataset with `simplicaVignette`, containing data and precomputed analysis results.

## Backward compatibility

- Version 2.0.0 introduces substantial methodological and implementation changes.
- Scripts written for version 1.0.0 may require modification.
- Fitness scores and analysis results are not directly comparable between versions 1.0.0 and 2.0.0.
