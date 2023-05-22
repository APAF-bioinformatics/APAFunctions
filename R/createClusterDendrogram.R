#' Makes CLUST1Gene dendrogram, but also ClusterExpressionProfiles? TO DO: sort out who does what between this and HClust()
#'
#' @param res1  Dunno, this should be renamed here and in the main script
#' @param Cluster
#' @param cluster.data
#' @param clusterMetric
#' @param Anova.idx
#' @param Group
#'
createClusterDendrogram <- function(res1, cluster.data, clusterMetric, Cluster, Anova.idx, Group){
  png("CLUST1Genes.png", 1000, 1000)

  clustID <- res1$clustID
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
  dev.off()
}
