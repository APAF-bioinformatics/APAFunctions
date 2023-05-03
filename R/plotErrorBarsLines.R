#' plot error bars with lines included 
#'
#' @param v Matrix of aggregated genes 
#' @param barSizes error bar sizes, normall twice the standard deviation
#' @param lines lines from sample cluster data
#' @param labels labels normal provided from ordered group, but defaults to NULL 
#' @param color  colours, often provided by the rainbow() function   
#' @param ylim  y limit of the plot, the normally the minimum value  
#' @param ... ellipses allow extra arguments to a function, when they are provided as a list  
#'
plotErrorBarsLines <- function (v, barSizes, lines, labels = NULL, color = "blue", ylim=c(min(lines), max(lines)), ...) {
    barSizes[is.na(barSizes)] <- 0
    topBars <- v + 0.5 * barSizes
    bottomBars <- v - 0.5 * barSizes
    if (is.null(labels)) 
        labels <- 1:length(v)
    ylims <- c(min(bottomBars, ylim[1], min(lines)), max(topBars, ylim[2], max(lines)))
    par(pch = 19, xaxt = "n")
    plot(as.numeric(labels), v, ylim = ylims, col = color, type = "b", lwd = 3, ...)
    par(xaxt = "s")
    my.at <- 1:length(v)   # KR - I don't know what this line return does, is this an implicit for loop? 
        axis(1, at = my.at, labels = labels)
    for (i in 1:length(v)) {
        lines(c(i, i), c(topBars[i], bottomBars[i]))
    }
    for (i in 1:ncol(lines)) {
        lines(as.numeric(labels), lines[, i], lwd = 0.5, lty = "dotted", col = "gray")
    }
}