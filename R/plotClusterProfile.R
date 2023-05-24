#' plotClusterProfile
#'
#' @param cluster.data
#' @param clustID
#' @param Group
#' @param k
#' @param ylab
#'
#' @return
#' @export
#'
#' TO DO: drop the overlaid data labels
plotClusterProfile <- function(cluster.data, clustID, Group, k=4, ylab="Abundance") {
  # TO DO: refactor this
  cluster.data <- log(scaled.cluster.data)
  numClusters <- k
  r.temp <- aggregate(t(cluster.data), by=list(Group=Group), FUN=mean)
  ag.sample <- r.temp[,-1]
  rownames(ag.sample) <- r.temp[,1]
  ag.genes <- aggregate(t(ag.sample), by=list(Cluster=clustID), FUN=mean)
  ag.sd <- aggregate(t(ag.sample), by=list(Cluster=clustID), FUN=sd)
  ag.matrix <- as.matrix(ag.genes[,-1])
  ag.counts <- summary(as.factor(clustID))
  ag.bars <- as.matrix(ag.sd[,-1])

  png("ClusterPatterns.png", 2000, 2000, res=300)
  par(bg=gray(.95), fg=gray(0.3), oma= c(5, 2, 2, 1) + 0.1, col.main="black", col.sub="black", col.lab="black", col.axis="black")
  layout(matrix(1:4, ncol=2, byrow=TRUE))
  for(i in 1:numClusters) {
    colors <- rainbow(4)
    # cols <- rep("gray", 6)
    gname <-  paste("Cluster", i, "(", ag.counts[i], "proteins )")
    lines <- ag.sample[, clustID==i, drop=FALSE]
    plotErrorBarsLines(ag.matrix[i,], 2*ag.bars[i,], lines,
                       labels=1:ncol(ag.matrix),
                       col=colors[i],  main=gname, # bgcol="gray", split=split,
                       ylab=ylab, xlab="",
                       ylim=c(min(ag.matrix), max(ag.matrix)))
    axis(1,at=1:ncol(ag.matrix), las=2, labels=colnames(ag.matrix), col="black")
    abline(h=0, lty="dotted")
  }
  dev.off()
}
