#' Refit selected component patterns for parameter reporting
#'
#' Takes a fitted object, original data and available custom pattern functions.
#' Uses updated CV patterns when present and fits default models on the supplied
#' residual matrix with a larger ALS budget than GA screening.
#' Returns one record per component with indices, activity, fit status, RSS and
#' default-model parameters. Custom fits have no parameter extractor. Inactive
#' slots and numerical failures receive explicit status records; fitting problems
#' are collected into a warning rather than discarding the other component fits.
#' @noRd
.simplicaRefitSelectedComponents <- function(foundObject, df,
                                             customPatterns = list()) {
  nRows <- foundObject$nRows
  nCols <- foundObject$nCols
  nComponents <- foundObject$nComponents
  string <- as.integer(foundObject$string)
  componentScores <- foundObject$componentScores
  selectedPatterns <- if (!is.null(foundObject$componentPatternsUpdated)) {
    foundObject$componentPatternsUpdated
  } else {
    foundObject$componentPatterns
  }
  selectedPatterns <- as.character(selectedPatterns)
  builtInPatterns <- defaultPatterns()
  rowNames <- rownames(df)
  columnNames <- colnames(df)
  componentFits <- vector("list", nComponents)
  names(componentFits) <- paste0("component_", seq_len(nComponents))
  refitWarnings <- character()

  for (component in seq_len(nComponents)) {
    indices <- .simplicaComponentIndices(
      string, nRows, nCols, component
    )
    rows <- indices$rows
    columns <- indices$cols
    pattern <- selectedPatterns[component]
    active <- isTRUE(componentScores[component] > 0)
    if (!active) {
      componentFits[[component]] <- list(
        componentId = component,
        active = FALSE,
        pattern = pattern,
        rows = rows,
        columns = columns,
        fitStatus = "inactive",
        parameters = NULL
      )
      next
    }

    componentData <- df[rows, columns, drop = FALSE]
    observed <- !is.na(componentData)
    nObserved <- sum(observed)
    fitError <- NULL

    builtIn <- pattern %in% builtInPatterns
    fit <- tryCatch({
      if (builtIn) {
        .defaultPatternFitConverged(componentData, pattern, tol = 1e-8, maxIter = 500)
      } else {
        customFunction <- customPatterns[[pattern]]
        if (is.null(customFunction)) {
          stop("custom pattern function is not available")
        }
        fitted <- customFunction(componentData)
        if (!is.numeric(fitted) || !is.matrix(fitted) ||
            !identical(dim(fitted), dim(componentData))) {
          stop("fit did not return a numeric matrix with component dimensions")
        }
        if (any(!is.finite(fitted[observed]))) {
          stop("fit returned non-finite values on observed cells")
        }
        fitted
      }
    }, error = function(error) {
      fitError <<- error
      NULL
    })
    if (is.null(fit)) {
      componentFits[[component]] <- list(
        componentId = component,
        active = TRUE,
        pattern = pattern,
        rows = rows,
        columns = columns,
        nObserved = nObserved,
        rss = NA_real_,
        converged = NA,
        iterations = NA_integer_,
        fitStatus = "numerical_failure",
        parameters = NULL,
        errorMessage = conditionMessage(fitError),
        errorClass = class(fitError)
      )
      refitWarnings <- c(refitWarnings, sprintf(
        "component %d (%s): %s",
        component, pattern, conditionMessage(fitError)
      ))
      next
    }
    if (builtIn) {
      rowEffects <- fit$rowEffects
      columnEffects <- fit$columnEffects
      interactionRows <- fit$interactionRows
      interactionColumns <- fit$interactionColumns
      if (!is.null(rowNames)) {
        names(rowEffects) <- rowNames[rows]
        names(interactionRows) <- rowNames[rows]
      }
      if (!is.null(columnNames)) {
        names(columnEffects) <- columnNames[columns]
        names(interactionColumns) <- columnNames[columns]
      }
      parameters <- switch(
        pattern,
        constant = list(mu = fit$mu),
        row = list(mu = fit$mu, rowEffects = rowEffects),
        column = list(mu = fit$mu, columnEffects = columnEffects),
        additive = list(
          mu = fit$mu,
          rowEffects = rowEffects,
          columnEffects = columnEffects
        ),
        multiplicative = list(
          mu = fit$mu,
          interactionScale = fit$interactionScale,
          interactionRows = interactionRows,
          interactionColumns = interactionColumns
        ),
        additive_multiplicative = list(
          mu = fit$mu,
          rowEffects = rowEffects,
          columnEffects = columnEffects,
          interactionScale = fit$interactionScale,
          interactionRows = interactionRows,
          interactionColumns = interactionColumns
        )
      )
      fitStatus <- if (isTRUE(fit$converged)) "ok" else "not_converged"
      componentFits[[component]] <- list(
        componentId = component,
        active = TRUE,
        pattern = pattern,
        rows = rows,
        columns = columns,
        nObserved = nObserved,
        fitted = fit$fitted,
        rss = fit$rss,
        converged = fit$converged,
        iterations = fit$iterations,
        fitStatus = fitStatus,
        parameters = parameters
      )
      if (!isTRUE(fit$converged)) {
        refitWarnings <- c(refitWarnings, sprintf(
          "component %d (%s): did not converge in %d iterations",
          component, pattern, fit$iterations
        ))
      }
      next
    }

    residual <- componentData[observed] - fit[observed]
    componentFits[[component]] <- list(
      componentId = component,
      active = TRUE,
      pattern = pattern,
      rows = rows,
      columns = columns,
      nObserved = nObserved,
      fitted = fit,
      rss = sum(residual * residual),
      converged = NA,
      iterations = NA_integer_,
      fitStatus = "custom_no_parameter_extractor",
      parameters = NULL
    )
  }

  if (length(refitWarnings)) {
    warning(
      "Post-selection component parameter refit issue(s): ",
      paste(refitWarnings, collapse = "; "),
      call. = FALSE
    )
  }
  componentFits
}
