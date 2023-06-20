#' The one function to plot dendrograms. Future: maybe rewrite with ggdendro?
#'
#' @param experiment
#' @param hclust_result
#' @param cluster.data
#' @param Group
#' @param clusterMetric
#' @param Cluster
#' @param Anova.idx
#' @param glabel
#' @param clabel
#' @param ...
#'
plotClusterDendrogram <- function (experiment=c("SWATH", "TMT"), hclust_result, cluster.data, Group, clusterMetric, Cluster, Anova.idx,
                                   glabel, clabel = NULL, ...) {
  # TO DO: The SwATH implementation can probably be written in ggplot::ggdendro. In TMT, the section formatting the plot if clabel exists is too long.
  # TO DO: atomic vector $ broken if plot=TRUE in HClust() which calls this function. Not broken if plot=FALSE

  if ( experiment == "SWATH") {
    png("CLUST1Genes.png", 1000, 1000)
    clustID <- HClust_res$clustID
    Cluster[Anova.idx] = clustID
    res <- HClust((cluster.data), metric=clusterMetric, method="complete", clabel=clustID) # Oh no, I think HClust is making CLUST1Genes. Separate out clust_calc() and clust_plot()
    dev.off()

    gp = Group
    # wrangle for plotting
    r.temp <- aggregate(t(cluster.data), by=list(gp=gp), FUN=mean)
    ag.sample <- r.temp[,-1]
    rownames(ag.sample) <- r.temp[,1]
    ag.genes <- aggregate(t(ag.sample), by=list(Cluster=clustID), FUN=mean)
    ag.sd <- aggregate(t(ag.sample), by=list(Cluster=clustID), FUN=sd)
    ag.matrix <- as.matrix(ag.genes[,-1])
    ag.counts <- summary(as.factor(clustID))
    ag.bars <- as.matrix(ag.sd[,-1])

    #plot dendrogram
    png("ClusterExpressionProfiles.png", 2000, 2000, res=300)
    layout(matrix(1:6, ncol=3, byrow=TRUE))
    NSig <- numClusters
    for(i in 1:NSig) {
      colors <- rainbow(6)
      gname <-  paste("Cluster", i, "(", ag.counts[i], "proteins )")
      lines <- ag.sample[, clustID==i, drop=FALSE]
      plotErrorBarsLines(ag.matrix[i,], 2*ag.bars[i,], lines, labels=order(levels(as.factor(gp))), color=colors[i], main=gname, xlab="Group", ylab="Log Normalised Area", ylim=c(min(ag.matrix), max(ag.matrix)))
    }
  }

  if ( experiment == "TMT" ) {
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

  dev.off()
}
