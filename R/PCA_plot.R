#' Generate 2D and 3D PCA plots
#'
#' @param experiment Currently TMT or SWATH
#' @param data Currently only used by TMT to get rownames
#' @param pca.components The output of PCA_calc()
#' @param Group Group information for each column from PCA_calc() input (usually prot.ag). No replicate information.
#'
PCA_plot <- function(experiment=c("SWATH", "TMT"), data, pca.components, Group){

  # STATE OF FUNCTION: works - only aesthetic changes required.
  # TO DO: PCA 3D plot could potentially be done in ggplot?
  # colours don't match the rest of the graphs - done through lattice.
  # --> could they be overwritten? Otherwise leave as is.

  grp_colors = trellis.par.get()$superpose.symbol$col
  z <- pca.components$componentScores

  if (experiment == "SWATH"){
    png("PCA3D.png", 2000, 2000, res=300)
    plot(cloud(z[, 1] ~ z[, 3] + z[, 2], groups = as.factor(Group),
               auto.key = list(points = TRUE, pch = 19, space = "right"),  #auto.key legend is ignoring pch styles, look into GGplot alternative
               xlab = "PC 3", ylab = "PC 2", zlab = "PC 1", distance = 0.1,
               main = "Projection in the space of the first 3 principal components"))
    invisible(dev.off())
    message("3D PCA plot written to file: PCA3D.png")

    cols <- rownames(pca.components$summary$x)
    png("PCA2D.png", 2000, 2000, res=300)
    layout(matrix(1:4, nrow=2))
    plot(z[,1], z[,2], col=grp_colors[Group], pch=20, xlab="PC1", ylab="PC2")
    #text(z[,1], z[,2], cols, pos=3, cex=.5)
    plot(z[,1], z[,3], col=grp_colors[Group], pch=20, xlab="PC1", ylab="PC3")
    #text(z[,1], z[,3], cols, pos=3, cex=.5)
    plot(z[,2], z[,3], col=grp_colors[Group], pch=20, xlab="PC2", ylab="PC3")
    #text(z[,2], z[,3], cols, pos=3, cex=.5)
    data[data == 0] <- NA
    boxplot(log(data), par(las=2), pch = 20, main="Boxplots of log data", col = grp_colors[Group])
    invisible(dev.off())
    message("PCA plot written to file: PCA2D.png")
  }
  else if (experiment == "TMT") {
    ld = pca.components$componentLoadings
    props = round(100*pca.components$summary$importance[2,1:3], 1)# proportion of variance of the top 3 components

    png("PCA3D.png", 2000, 2000, res=300)
    s3d <- scatterplot3d(z[,1:3], color=grp_colors[Group], col.axis=gray(0.85), col.grid="lightblue", box = T, angle = 26, pch=20)
    s3d$points3d(z[,1:3], pch=21)
    legend("topright", fill=grp_colors[1:nlevels(Group)], legend=levels(Group), cex=.8)
    #text(s3d$xyz.convert(3+z[,1], 3+z[,2], z[,3]), labels = colnames(data), cex=0.4)
    # removing labels - clutters graph too much
    invisible(dev.off())
    message("3D PCA plot written to file: PCA3D.png")

    png("PCA2D.png", 2000, 2000, res=300)
    layout(matrix(1:4, ncol=2))
    plot(z[,1], z[,2], col=grp_colors[Group], pch=20, xlab=paste0("PC1(", props[1],"%)"), ylab=paste0("PC2(", props[2], "%)"))
    points(z[,1], z[,2], pch=21, cex=1.1, lwd=1.3)
    #text(z[,1], z[,2], colnames(data), pos=3, cex=.5)
    plot(z[,1], z[,3], col=grp_colors[Group], pch=20, xlab=paste0("PC1(", props[1],"%)"), ylab=paste0("PC3(", props[3],"%)"))
    points(z[,1], z[,3], pch=21, cex=1.1, lwd=1.3)
    #text(z[,1], z[,3], colnames(data), pos=3, cex=.5)
    plot(z[,2], z[,3], col=grp_colors[Group], pch=20, xlab=paste0("PC2(", props[2], "%)"), ylab=paste0("PC3(", props[3],"%)"))
    points(z[,2], z[,3], pch=21, cex=1.1, lwd=1.3)
    #text(z[,2], z[,3], colnames(data), pos=3, cex=.5)
    plot(z[,2], z[,3], col=grp_colors[Group], pch=20, xlab="", ylab="", axes=FALSE, type='n')
    legend("center", fill=grp_colors[1:nlevels(Group)], legend=levels(Group))
    invisible(dev.off())
    message("PCA plot written to file: PCA2DAll.png")

    # ord.list = list()
    # png("PCATopLoadings.png", width=2000, height=700, res=300)
    # par(oma=c(2,1,1,1))
    # layout(matrix(1:3, nrow=1))
    # for (i in 1:3) {
    #   ord = order(abs(ld[,i]), decreasing=TRUE)[1:5]
    #   barplot(sort(ld[ord, i]), las=2, main=paste("Top loadings PC", i))
    #   ord.list[[i]] = ord
    # }
    # invisible(dev.off())

    # png("PCATopLoadingsProteinPatterns.png", width=2500, height=2500, res=300)
    # par(mar=c(5,3,3,1))
    # layout(matrix(1:15, nrow=3, ncol = 5, byrow=T))
    # for (i in 1:3) {
    #   ord = ord.list[[i]]
    #   for (j in 1:5) {
    #     matchprot <- rownames(ld)[ord[j]]
    #     boxdat <- as.numeric(data[match(matchprot, rownames(data)),])
    #     boxplot(boxdat ~ Group, boxwex=0.5, main=matchprot, col="gray", las=2, xlab = "")
    #   }
    # }
    # invisible(dev.off())
  }
}
