
#' Plots a heatmap with either euclidean or cor distance; ANOVA or no ANOVA 
#'
#' @param Anova.idx index of where significant ANOVA values are  
#' @param prot.ag table with Protein name and intensities by group  
#' @param dist either [euclidean or cordist] 
#' @param Group Group for the labels
#' @param useAnova whether ANOVA has been preformed 
#'
plotHeatmaps <- function(prot.ag, Group, dist, Anova.idx=NULL, useAnova=FALSE){ # A clever solution would probably lose a few lines.. Moving on. 
  ## GERI: turned off row labels as they were just row numbers and gave no information
  if (useAnova == TRUE){
    x <- as.matrix(log(na.omit(prot.ag[Anova.idx,-1]+1)))
  } else {
    x <- as.matrix(log(na.omit(prot.ag[,-1]+1)))
  }
  
  if (dist == "euclidean"){
    if (useAnova == TRUE){
      png("Heatmap euclidean - Anova DE.png", 2000, 2000, res=300)
    } else {
      png("Heatmap euclidean - all.png", 2000, 2000, res=300)
    }
    hm <- heatmap(x, col=colorRampPalette(c("green", "red"))(120),  margins=c(10,15), cexRow=1, ColSideColors=rainbow(ncol(x)), labRow = NA)
    
  } else if (dist == "cordist") { 
    cordist <- function(d) {as.dist((1-cor(t(d)))/2)}
    if (useAnova == TRUE){
      png("Heatmap cordist - Anova DE.png", 2000, height=2000, res=300)
    } else {
      png("Heatmap cordist - all.png", 2000, height=2000, res=300)
    }
    hm <- heatmap(x, col=colorRampPalette(c("green", "red"))(120),  margins=c(10,15), cexRow=1, distfun=cordist, ColSideColors=rainbow(ncol(x)), labRow = NA)
  }
  
  else {
    stop("Heatmap currently only supports euclidean or cor distances")
  }
  
  legend("topright", fill=rainbow(nlevels(as.factor(Group))), legend=levels(as.factor(Group)))
  dev.off()
}
