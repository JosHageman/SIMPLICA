#' Plot non-contiguous simplivariate components by pattern type, with optional reordering
#'
#' Visualizes GA-detected simplivariate components on the original matrix as outlined cells, colored by pattern type.
#'
#' @param df Original data matrix
#' @param string Best GA string (vector of length ncol(df) + nrow(df))
#' @param componentPatterns Vector of component types (from fitness(..., returnPatterns = TRUE))
#' @param componentScores Vector of fitness scores per component
#' @param scoreCutoff Minimum score a component must have to be shown (default: 0 = show all)
#' @param showLabels Show axis labels (default: TRUE)
#' @param title Title for the plot
#' @param rearrange Logical: reorder rows and columns to group components (default: FALSE)
#' @param grayscale Logical: use grayscale for heatmap background (default: TRUE)
#'
#' @return ggplot object
#' @export
plotComponentResult <- function(df, string, componentPatterns, componentScores,
                                scoreCutoff = 0,
                                showLabels = TRUE,
                                title = "Detected Components",
                                rearrange = FALSE,
                                grayscale = TRUE) {

  if (!requireNamespace("reshape2", quietly = TRUE)) stop("Please install 'reshape2'.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install 'ggplot2'.")
  if (!requireNamespace("ggsci", quietly = TRUE)) stop("Please install 'ggsci'.")
  if (!requireNamespace("seriation", quietly = TRUE)) stop("Please install 'seriation'.")

  stopifnot(is.matrix(df) || is.data.frame(df))
  df <- as.matrix(df)

  rows <- nrow(df)
  cols <- ncol(df)
  idxRows <- string[1:rows]
  idxCols <- string[(rows + 1):(cols + rows)]

  if (is.null(rownames(df))) rownames(df) <- paste0("Row", 1:rows)
  if (is.null(colnames(df))) colnames(df) <- paste0("Col", 1:cols)

  keepIdx <- which(componentScores >= scoreCutoff)

  # Optional reordering based on membership to kept components
  if (rearrange && length(keepIdx) > 0) {
    # Build binary membership matrices for rows and columns (one column per kept component)
    rowMembership <- matrix(0L, nrow = rows, ncol = length(keepIdx))
    colMembership <- matrix(0L, nrow = cols, ncol = length(keepIdx))

    j <- 1L
    while (j <= length(keepIdx)) {
      k <- keepIdx[j]
      if (any(idxRows == k)) rowMembership[which(idxRows == k), j] <- 1L
      if (any(idxCols == k)) colMembership[which(idxCols == k), j] <- 1L
      j <- j + 1L
    }

    # Compute binary (Jaccard) distances; handle degenerate cases gracefully
    safeOrder <- function(m) {
      # If all-zero columns or only one row, return identity
      if (nrow(m) <= 1L || all(colSums(m) == 0L)) {
        return(seq_len(nrow(m)))
      }
      d <- stats::dist(m, method = "binary")
      if (all(is.na(d)) || length(d) == 0L) return(seq_len(nrow(m)))
      ord <- tryCatch({
        ser <- seriation::seriate(d, method = "ARSA", rep = 25, verbose = FALSE)
        seriation::get_order(ser)
      }, error = function(e) seq_len(nrow(m)))
      if (anyNA(ord)) ord <- seq_len(nrow(m))
      ord
    }

    rowOrder <- safeOrder(rowMembership)
    colOrder <- safeOrder(colMembership)

    # Reorder df
    df <- df[rowOrder, colOrder, drop = FALSE]

    # Build index maps so we can translate original indices to reordered positions
    rowIndexMap <- integer(rows)
    rowIndexMap[rowOrder] <- seq_along(rowOrder)
    colIndexMap <- integer(cols)
    colIndexMap[colOrder] <- seq_along(colOrder)
  } else {
    # Identity maps if not rearranging
    rowIndexMap <- seq_len(rows)
    colIndexMap <- seq_len(cols)
  }

  # Long data for heatmap
  dataLong <- reshape2::melt(df, varnames = c("Row", "Col"), value.name = "Value")

  componentTiles <- data.frame()
  componentLabels <- data.frame()

  if (length(keepIdx) == 0) {
    message("No components exceed scoreCutoff.")
    fillScale <- if (grayscale) ggplot2::scale_fill_gradient(low = "white", high = "black") else ggplot2::scale_fill_viridis_c()
    return(
      ggplot2::ggplot(dataLong, ggplot2::aes(x = Col, y = Row, fill = Value)) +
        ggplot2::geom_tile() +
        fillScale +
        ggplot2::labs(title = paste(title, "(no components shown)")) +
        ggplot2::theme_minimal()
    )
  }

  patternCounts <- list()
  componentLabelsText <- character(length(componentPatterns))

  # Build tiles and labels using mapped indices after any reordering
  for (k in keepIdx) {
    pattern <- componentPatterns[k]
    if (is.null(patternCounts[[pattern]])) patternCounts[[pattern]] <- 0L
    patternCounts[[pattern]] <- patternCounts[[pattern]] + 1L
    label <- paste0(pattern, "_", patternCounts[[pattern]])
    componentLabelsText[k] <- label

    rawRowIdx <- which(idxRows == k)
    rawColIdx <- which(idxCols == k)

    if (length(rawRowIdx) > 0 && length(rawColIdx) > 0) {
      # Map to reordered positions
      rowIdx <- rowIndexMap[rawRowIdx]
      colIdx <- colIndexMap[rawColIdx]

      # Expand to all cell outlines
      r <- 1L
      while (r <= length(rowIdx)) {
        c <- 1L
        while (c <= length(colIdx)) {
          componentTiles <- rbind(componentTiles, data.frame(
            Row = rownames(df)[rowIdx[r]],
            Col = colnames(df)[colIdx[c]],
            ComponentID = label,
            Pattern = pattern,
            stringsAsFactors = FALSE
          ))
          c <- c + 1L
        }
        r <- r + 1L
      }

      # Label at top-left of the component in reordered space
      componentLabels <- rbind(componentLabels, data.frame(
        Row = rownames(df)[min(rowIdx)],
        Col = colnames(df)[min(colIdx)],
        Label = label,
        Pattern = pattern,
        stringsAsFactors = FALSE
      ))
    }
  }

  p <- ggplot2::ggplot(dataLong, ggplot2::aes(x = Col, y = Row, fill = Value)) +
    ggplot2::geom_tile() +
    {if (grayscale) ggplot2::scale_fill_gradient(low = "white", high = "black") else ggplot2::scale_fill_viridis_c()} +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

  if (!showLabels) {
    p <- p + ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  }

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

  if (nrow(componentLabels) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = componentLabels,
        ggplot2::aes(x = Col, y = Row, label = Label),
        color = "#bfbfbf",  # lichte grijs voor betere zichtbaarheid
        hjust = -0.2,
        vjust = -0.2,
        size = 3,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  return(p)
}
