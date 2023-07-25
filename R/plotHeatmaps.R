#' Plots a heatmap with either euclidean or cor distance; ANOVA or no ANOVA
#'
#' @param experiment SWATH or TMT at the moment
#' @param Anova.idx index of where significant ANOVA values are
#' @param data ion data: either in the format of a list of normalised ion data matrix from TMT, or prot.ag (table with Protein name and intensities by group) in SWATH
#' @param dist distance function either [euclidean or cordist]
#' @param Group Group for the labels
#' @param useAnova whether ANOVA has been preformed
#'
plotHeatmaps <- function(experiment=c("SWATH", "TMT"), data, Group, dist, Anova.idx=NULL, useAnova=FALSE){
  Group <- as.factor(Group)
  grp_colors = RColorBrewer::brewer.pal(nlevels(Group), "Set2")
  if (experiment == "TMT") {
    if (useAnova==TRUE) {
      if(nrow(data[Anova.idx,]) > 3) {
        x <- as.matrix(na.omit(log(data[Anova.idx,]+.5)))
        maintitle = "TMT - Anova DE Heatmap"
        file <- "Heatmap - Anova DE.png"
        png("Heatmap - Anova DE.png", 2000, 2000, res = 300)
      }
    } else {
      x <- cor(log(na.omit(data+.5)))
      maintitle = "TMT - IRS Heatmap"
      file <- "Correlation heatmap IRS.png"
      png("Correlation heatmap IRS.png", 2000, 2000, res = 300)
    }
    par(oma=c(0,0,1,0))
    heatmap3(x, margins=c(8,5), cexRow=1, col=colorRampPalette(c("red", "white", "blue"))(120),
             ColSideColors=grp_colors[Group], main = maintitle, ColSideLabs = NA, labRow = F)
    legend("topright", fill=grp_colors[1:nlevels(Group)], legend=levels(Group), xpd=TRUE,cex=.6)
    invisible(dev.off())

  } else if (experiment == "SWATH") {
    if (useAnova == TRUE){
      x <- as.matrix(log(na.omit(data[Anova.idx,-1]+1)))
    } else {
      x <- as.matrix(log(na.omit(data[,-1]+1)))
    }
    if (dist == "euclidean"){
      disfun <- function(d) as.dist(1 - cor(t(d), use = "pa"))
      if (useAnova == TRUE){
        maintitle = "SWATH - Euclidean heatmap, Anova DE"
        file <- "Heatmap euclidean - Anova DE.png"
        png("Heatmap euclidean - Anova DE.png", 2000, 2000, res=300)
      } else {
        maintitle = "SWATH - Euclidean heatmap"
        file <- "Heatmap euclidean - all.png"
        png("Heatmap euclidean - all.png", 2000, 2000, res=300)
      }
    } else if (dist == "cordist") {
      disfun <- function(d) {as.dist((1-cor(t(d)))/2)}
      if (useAnova == TRUE){
        maintitle = "SWATH - Cordist heatmap, Anova DE"
        file <- "Heatmap cordist - Anova DE.png"
        png("Heatmap cordist - Anova DE.png", 2000, height=2000, res=300)
      } else {
        maintitle = "SWATH - Cordist heatmap"
        file <- "Heatmap cordist - all.png"
        png("Heatmap cordist - all.png", 2000, height=2000, res=300)
      }
    }
    else { stop("Heatmap currently only supports euclidean or cor distances")}
    par(oma=c(0,0,1,0))
    heatmap(x, col=colorRampPalette(c("red", "blue"))(120),  margins=c(8,5), cexRow=1, distfun=disfun,
            ColSideColors=grp_colors[Group], main = maintitle, labRow = F)
    legend("topright", fill=grp_colors[1:nlevels(Group)], legend=levels(Group))
    invisible(dev.off())
    message("Heapmap output written to file: ", file)
    print(distfun)
  }
}
