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

plotHeatmaps <- function(exp, data, Group, dist, Anova.idx=NULL, useAnova=FALSE){
  Group <- as.factor(Group)
  grp_colors = rainbow(nlevels(Group))
  if (exp == "TMT") {
    if (useAnova==TRUE) {
      if(nrow(data[Anova.idx,]) > 3) {
        x <- as.matrix(na.omit(log(data[Anova.idx,]+.5)))
        maintitle = "TMT - Anova DE Heatmap"
        png("Heatmap - Anova DE.png", 2000, 2000, res = 300)
      }
    } else {
      x <- cor(log(na.omit(data+.5)))
      maintitle = "TMT - IRS Heatmap"
      png("Correlation heatmap IRS.png", 2000, 2000, res = 300)
    }
    heatmap3(x, margins=c(8,8), cexRow=1, col=colorRampPalette(c("green", "black", "red"))(120),
             ColSideColors=grp_colors[Group], main = maintitle)
    legend("topright", fill=grp_colors[1:nlevels(Group)], legend=levels(Group), xpd=TRUE,cex=.6)
    dev.off()

    } else if (exp == "SWATH") {
    if (useAnova == TRUE){
      x <- as.matrix(log(na.omit(data[Anova.idx,-1]+1)))
    } else {
      x <- as.matrix(log(na.omit(data[,-1]+1)))
    }
    if (dist == "euclidean"){
      disfun <- function(d) as.dist(1 - cor(t(d), use = "pa"))
      if (useAnova == TRUE){
        maintitle = "SWATH - Euclidean heatmap, Anova DE"
        png("Heatmap euclidean - Anova DE.png", 2000, 2000, res=300)
      } else {
        maintitle = "SWATH - Euclidean heatmap"
        png("Heatmap euclidean - all.png", 2000, 2000, res=300)
      }
    } else if (dist == "cordist") {
      disfun <- function(d) {as.dist((1-cor(t(d)))/2)}
      if (useAnova == TRUE){
        maintitle = "SWATH - Cordist heatmap, Anova DE"
        png("Heatmap cordist - Anova DE.png", 2000, height=2000, res=300)
      } else {
        maintitle = "SWATH - Cordist heatmap"
        png("Heatmap cordist - all.png", 2000, height=2000, res=300)
      }
    }
    else { stop("Heatmap currently only supports euclidean or cor distances")}

    heatmap(x, col=colorRampPalette(c("green", "red"))(120),  margins=c(10,15), cexRow=1, distfun=disfun,
            ColSideColors=grp_colors[Group], labRow = NA, main = maintitle)
    legend("topright", fill=grp_colors[1:nlevels(Group)], legend=levels(Group))
    dev.off()
  }
}
