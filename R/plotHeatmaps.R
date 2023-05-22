#' Plots a heatmap with either euclidean or cor distance; ANOVA or no ANOVA
#'
#' @param exp SWATH or TMT at the moment, but this should be joined together
#' @param Anova.idx index of where significant ANOVA values are
#' @param data either data_irs (list of ion data from TMT) or prot.ag (table with Protein name and intensities by group)
#' @param dist either [euclidean or cordist]
#' @param Group Group for the labels
#' @param useAnova whether ANOVA has been preformed
#'
library(heatmap3)

plotHeatmaps <- function(exp, Group, data, dist, Anova.idx=NULL, useAnova=FALSE){ # A clever solution would probably lose a few lines below.. Moving on.
grp_colors = rainbow(nlevels(Group))
 if (exp == "TMT") {
    heatmap3(cor(log(na.omit(data+.5)), use="pairwise.complete.obs"), #distfun=cordist,
             col=colorRampPalette(c("green","black", "red"))(1024),
             main="IRS correlation",
             colsideColors=rainbow(nlevels(Group))[Group], margins=c(10,10))
    legend("topright", fill=grp_colors[1:nlevels(Group)], legend=levels(Group), xpd=TRUE, cex=.6)
    png("Correlation heatmap IRS.png", 2000, 2000,res=300)
  } else if (exp == "SWATH") {
    ## GERI: turned off row labels as they were just row numbers and gave no information
    if (useAnova == TRUE){
      x <- as.matrix(log(na.omit(data[Anova.idx,-1]+1)))
    } else {
      x <- as.matrix(log(na.omit(data[,-1]+1)))
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
  }
  dev.off()
}
