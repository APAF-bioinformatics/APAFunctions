#' Generate 2D and 3D PCA plots
#'
#' @param components The output of PCA_calc()
#' @param labelValue Group information for each column from PCA_calc() input (usually prot.ag). No replicate information.
#' 
PCA_plot <- function(components, labelValue){
  z <- components$componentScores
  png("PCA3D.png", 2000, 2000, res=300)
  plot(cloud(z[, 1] ~ z[, 3] + z[, 2], groups = as.factor(labelValue), 
           auto.key = list(points = TRUE, pch = 19, space = "right"),  #auto.key legend is ignoring pch styles, look into GGplot alternative  
           xlab = "PC 3", ylab = "PC 2", zlab = "PC 1", distance = 0.1, 
           main = "Projection in the space of the first 3 principal components"))
  dev.off()
  cols <- rownames(components$summary$x)
  png("PCA 2d - all.png", 2000, 2000, res=300)
  layout(matrix(1:4, nrow=2))
  plot(z[,1], z[,2], col=rainbow(nlevels(as.factor(Group)))[as.factor(Group)], pch=20, xlab="PC1", ylab="PC2")
  text(z[,1], z[,2], cols, pos=3, cex=.5)
  plot(z[,1], z[,3], col=rainbow(nlevels(as.factor(Group)))[as.factor(Group)], pch=20, xlab="PC1", ylab="PC3")
  text(z[,1], z[,3], cols, pos=3, cex=.5)
  plot(z[,2], z[,3], col=rainbow(nlevels(as.factor(Group)))[as.factor(Group)], pch=20, xlab="PC2", ylab="PC3")
  text(z[,2], z[,3], cols, pos=3, cex=.5)
  boxplot(log(prot.ag[,-1]),par(las=2), pch = 20, main="Boxplots of log data")
  dev.off()
}
