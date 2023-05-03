#' Creates an Excel workbook with each image and a description. Doesn't require any arguments -- creating this workbook is self-contained
#'
createResultsImagexlsx <- function(){
  Images = c("DataOverall", "PCA3D", "PCA 2D All",  "HeatmapEuclideanAll", "HeatmapCordist Anova DE", "ClusterExpressionProfiles")
  ImageNames = c("DataOverall.png", "PCA3D.png", "PCA 2d - all.png", "Heatmap euclidean - all.png", "Heatmap cordist - Anova DE.png", "ClusterExpressionProfiles.png")
  ImageExplanations = c("DataOverall - contains density plots of the relative ion ratios with respect to the first sample, and boxplots of log ion peak areas, log transformed or normalized using the requested normalization.",
                        "PCA 3d: Principal component analysis 3D plot.",
                        "PCA 2d: Plots from a PCA analysis carried out on normalized protein data , and a boxplot of the normalized protein level data.",
                        "Heatmap Euclidean all contains the heatmap of all proteins (Euclidean metric)",
                        "Heamtap cordist - Anova DE contains the heatmap of the differentially expressed proteins from the Anova analysis ( p-value < cutoff and MaxFC > cutoff); correlation based metric",
                        "ClusterExpressionProfiles - shows the expression patterns of individual proteins, likewise only using those differentially expressed based on the Anova analysis.")

  # This doesn't need to be headerstyle2 anymore
  hs2 <- createStyle(fontColour = "navy", fgFill = "yellow", halign = "center", valign = "center", textDecoration = "Bold",border = "TopBottomLeftRight")

  wb <- openxlsx::createWorkbook("Results_Images.xlsx")
  for (img in 1:length(Images)) {
    addWorksheet(wb, Images[img])
    writeData(wb, sheet=img, data.frame(Info = ImageExplanations[img]), headerStyle=hs2)
    try(insertImage(wb, img, ImageNames[img], startRow = 5,  startCol = 2, width=8, height=8))
  }
  openxlsx::saveWorkbook(wb, file="Results_Images.xlsx",  overwrite=T)
}
