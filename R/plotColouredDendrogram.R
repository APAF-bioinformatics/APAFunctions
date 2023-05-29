#' plot a dendrogram, normally called after HClust(). TO DO: merge with APAFunctions::createClusterDendrogram. Maybe use ggplot
#'
#' @param hclust_result The hclust_results of the HClust() function
#' @param glabel Group labels
#' @param clabel column? cluster? labels TO DO: check this
#' @param ... Ellipses allow extra arguments to the function, taken in as a list
#'
plotColouredDendrogram <- function (hclust_result, glabel, clabel = NULL, ...) {
  if (!is.null(clabel)) { # Lots of formating if there's a clabel, but this defaults to NULL
    clabel <- as.factor(as.vector(clabel))
    plotLegend(clabel)
    w = 4
    layout(matrix(c(2, 1), ncol = 2), widths = c(1, lcm(w)))
    plot.new()
    par(mar = c(1, 1, 1, 1))
    plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
    legend(0, 1, legend = levels(clabel), pch = 21, col = rainbow(length(levels(clabel)))[1:nlevels(clabel)])
    merge <- hclust_result$merge
    N <- 1 + length(hclust_result$height)
    idx <- match(-(1:N), merge)
    oo <- ifelse(idx > (N - 1), idx - N + 1, idx) #test if the index is greater than 1+hclust height (N), if true subtract N+1 from idx, otherwise keep idx
    height <- hclust_result$height
    y <- height[oo] - ((max(height) - min(height))/10)
    y_ordered <- y[hclust_result$order]
    colors <- rainbow(length(levels(clabel)))[as.numeric(clabel[hclust_result$order])]
    par(srt = 90)
    points(1:N, y = y_ordered, col = colors, pch = 16)
    par(srt = 0)
    plot(hclust_result, labels = glabel, ...)
  } else {
    plot(hclust_result, labels = NULL, ...) # KR - the dendrogram final branch labels overlap to the point of unintelligiblity. Re-enable in an interactive R-Shiny plot
  }
}
