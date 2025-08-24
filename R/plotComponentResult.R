#' Plot non-contiguous simplivariate components by pattern type, with optional reordering
#'
#' Visualizes GA-detected simplivariate components on the original matrix as outlined cells,
#' colored by pattern type.
#'
#' @param df Original data matrix
#' @param string Best GA string (vector of length nrow(df) + ncol(df); rows first, then cols)
#' @param componentPatterns Vector of component types (from fitness(..., returnPatterns = TRUE))
#' @param componentScores Vector of fitness scores per component
#' @param scoreCutoff Minimum score a component must have to be shown (default: 0 = show all)
#' @param showAxisLabels Logical: show axis tick labels (default: TRUE)
#' @param showComponentLabels Logical: show component labels inside clusters (default: TRUE)
#' @param title Title for the plot (default: "Detected Components")
#' @param rearrange Logical: reorder rows and columns to group components (default: FALSE)
#' @param grayscale Logical: use grayscale for heatmap background (default: TRUE)
#'
#' @return ggplot object
#' @export
plotComponentResult <- function(df, string, componentPatterns, componentScores,
                                scoreCutoff = 0,
                                showAxisLabels = TRUE,
                                showComponentLabels = TRUE,
                                title = "Detected Components",
                                rearrange = FALSE,
                                grayscale = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")
  if (!requireNamespace("ggsci",    quietly = TRUE)) stop("Please install 'ggsci'.")

  stopifnot(is.matrix(df) || is.data.frame(df))
  df <- as.matrix(df)

  nRows <- nrow(df); nCols <- ncol(df)
  if (!is.numeric(string) || length(string) != (nRows + nCols)) {
    stop("Argument 'string' must be numeric with length nrow(df) + ncol(df).")
  }

  idxRows <- string[1:nRows]
  idxCols <- string[(nRows + 1):(nRows + nCols)]

  makeSafeNames <- function(nms, prefix, n) {
    if (is.null(nms)) nms <- rep("", n)
    nms[is.na(nms)] <- ""
    empty <- nms == ""
    if (any(empty)) nms[empty] <- paste0(prefix, seq_len(sum(empty)))
    if (anyDuplicated(nms)) nms <- make.unique(nms, sep = "_")
    nms
  }
  rownames(df) <- makeSafeNames(rownames(df), "r", nRows)
  colnames(df) <- makeSafeNames(colnames(df), "c", nCols)

  rowNames <- rownames(df)
  colNames <- colnames(df)

  keepIdx <- which(componentScores >= scoreCutoff)

  fastLexiOrder <- function(m) {
    n <- nrow(m)
    if (n <= 1L) return(seq_len(n))
    if (ncol(m) == 0L || all(colSums(m) == 0L)) return(seq_len(n))
    if (is.logical(m)) m <- m + 0L
    if (is.double(m))  m <- (m != 0) + 0L
    ordCols <- order(colSums(m), decreasing = TRUE)
    m <- m[, ordCols, drop = FALSE]
    dfOrd <- as.data.frame(-m)
    dfOrd$rowOnes <- rowSums(m)
    dfOrd$orig <- seq_len(n)
    do.call(order, dfOrd)
  }

  if (rearrange && length(keepIdx) > 1L) {
    rowMembership <- matrix(0L, nrow = nRows, ncol = length(keepIdx))
    colMembership <- matrix(0L, nrow = nCols, ncol = length(keepIdx))
    for (j in seq_along(keepIdx)) {
      k <- keepIdx[j]
      if (any(idxRows == k)) rowMembership[which(idxRows == k), j] <- 1L
      if (any(idxCols == k)) colMembership[which(idxCols == k), j] <- 1L
    }
    rowOrder <- fastLexiOrder(rowMembership)
    colOrder <- fastLexiOrder(colMembership)
    df <- df[rowOrder, colOrder, drop = FALSE]
    rowIndexMap <- integer(nRows); rowIndexMap[rowOrder] <- seq_along(rowOrder)
    colIndexMap <- integer(nCols); colIndexMap[colOrder] <- seq_along(colOrder)
    rowNames <- rownames(df); colNames <- colnames(df)
  } else {
    rowIndexMap <- seq_len(nRows)
    colIndexMap <- seq_len(nCols)
  }

  dataLong <- data.frame(
    Row   = rep(rowNames, times = ncol(df)),
    Col   = rep(colNames, each  = nrow(df)),
    Value = as.vector(df),
    stringsAsFactors = FALSE
  )
  dataLong$Row <- factor(dataLong$Row, levels = rowNames)
  dataLong$Col <- factor(dataLong$Col, levels = colNames)

  componentTiles  <- data.frame()
  componentLabels <- data.frame()

  if (length(keepIdx) > 0) {
    patternCounts <- list()
    for (k in keepIdx) {
      pattern <- componentPatterns[k]
      if (is.null(patternCounts[[pattern]])) patternCounts[[pattern]] <- 0L
      patternCounts[[pattern]] <- patternCounts[[pattern]] + 1L
      label <- paste0(pattern, "_", patternCounts[[pattern]])

      rawRowIdx <- which(idxRows == k)
      rawColIdx <- which(idxCols == k)

      if (length(rawRowIdx) > 0 && length(rawColIdx) > 0) {
        rowIdx <- rowIndexMap[rawRowIdx]
        colIdx <- colIndexMap[rawColIdx]

        cells <- expand.grid(
          Row = rowNames[rowIdx],
          Col = colNames[colIdx],
          stringsAsFactors = FALSE
        )
        cells$ComponentID <- label
        cells$Pattern <- pattern
        componentTiles <- rbind(componentTiles, cells)

        componentLabels <- rbind(componentLabels, data.frame(
          Row = rowNames[min(rowIdx)],
          Col = colNames[min(colIdx)],
          Label = label,
          Pattern = pattern,
          stringsAsFactors = FALSE
        ))
      }
    }

    if (nrow(componentTiles) > 0) {
      componentTiles$Row <- factor(componentTiles$Row, levels = rowNames)
      componentTiles$Col <- factor(componentTiles$Col, levels = colNames)
    }
    if (nrow(componentLabels) > 0) {
      componentLabels$Row <- factor(componentLabels$Row, levels = rowNames)
      componentLabels$Col <- factor(componentLabels$Col, levels = colNames)
    }
  }

  fillScale <- if (grayscale) ggplot2::scale_fill_gradient(low = "white", high = "black") else ggplot2::scale_fill_viridis_c()

  xScale <- ggplot2::scale_x_discrete(expand = c(0, 0), drop = FALSE, breaks = colNames, labels = colNames)
  yScale <- ggplot2::scale_y_discrete(expand = c(0, 0), drop = FALSE, breaks = rowNames, labels = rowNames)

  # basisplot
  p <- ggplot2::ggplot(dataLong, ggplot2::aes(x = Col, y = Row, fill = Value)) +
    ggplot2::geom_tile() +
    fillScale +
    xScale + yScale +
    ggplot2::labs(title = title) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    )

  # axis labels
  if (showAxisLabels) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(),
      axis.ticks  = ggplot2::element_line()
    )
  } else {
    p <- p + ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  }

  # outline van componenten
  if (nrow(componentTiles) > 0) {
    p <- p +
      ggplot2::geom_tile(
        data = componentTiles,
        ggplot2::aes(x = Col, y = Row, color = ComponentID),
        fill = NA,
        linewidth = 1.2,
        inherit.aes = FALSE
      ) +
      ggsci::scale_color_d3(name = "Simplivariate Components") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = NA)))
  }

  # labels op de componenten
  if (showComponentLabels && nrow(componentLabels) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = componentLabels,
        ggplot2::aes(x = Col, y = Row, label = Label),
        color = "#bfbfbf",
        hjust = -0.2,
        vjust = -0.2,
        size = 3,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  return(p)
}
