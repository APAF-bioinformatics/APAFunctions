#' plot a dendrogram, normally called after HClust()
#'
#' @param result The results of the HClust() function
#' @param glabel Group labels
#' @param clabel column? cluster? labels TO DO: check this 
#' @param ... Ellipses allow extra arguments to the function, taken in as a list  
#'
plotColouredDendrogram <- function (result, glabel, clabel = NULL, ...) {
  if (!is.null(clabel)) {
    clabel <- as.factor(as.vector(clabel))
    # plotLegend(clabel)
    w = 4
    layout(matrix(c(2, 1), ncol = 2), widths = c(1, lcm(w)))
    plot.new()
    par(mar = c(1, 1, 1, 1))
    plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
    legend(0, 1, legend = levels(clabel), pch = 21, col = rainbow(length(levels(clabel)))[1:nlevels(clabel)])
    plot(result, labels = glabel, ...)
    merge <- result$merge
    N <- 1 + length(result$height)
    idx <- match(-(1:N), merge)
    oo <- ifelse(idx > (N - 1), idx - N + 1, idx)
    height <- result$height
    y <- height[oo] - ((max(height) - min(height))/10)
    yy <- y[result$order]
    col <- rainbow(length(levels(clabel)))[as.numeric(clabel[result$order])]
    par(srt = 90)
    points(1:N, y = yy, col = col, pch = 16)
    par(srt = 0)
  } else {
    plot(result, labels = glabel, ...)
  }
}
