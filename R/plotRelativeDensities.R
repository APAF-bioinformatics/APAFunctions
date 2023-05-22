#' Plot the relative of ions (provide matrix w/ or wo/ normalisation)
#'
#' @param mat matrix of ion intensities 
#' @param Group Group from the design file  
#' @param idx index of the group?
#' @param main The label? of the densities  
#'
plotRelativeDensities <-function(mat, Group=NULL, idx=NULL, main="Densities") {
  if (is.null(Group)) Group = rep("A", ncol(mat))
  Group = as.factor(Group)
  
  if (is.null(idx)) idx = 1;
  C = rainbow(nlevels(Group))[Group]
  
  # empty plot
  plot(density(log(na.omit(mat[,1]))), type="n", ylim=c(0,2), xlim=c(-10,10), main=main)
  abline(v=0, col="blue")
  
  # calculate all relative ratios
  for (i in 1:ncol(mat)) {
    if (i != idx) lines(density(log(na.omit(mat[,i]/mat[,idx]))), col=C[i]);
  }
}
