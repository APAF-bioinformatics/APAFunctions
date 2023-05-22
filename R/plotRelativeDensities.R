#' Plot the relative of ions (provide ion matrix w/ or wo/ normalisation)
#'
#' @param data matrix of ion intensities
#' @param Group Group from the design file
#' @param idx index of the group?
#' @param main The label? of the densities
#'
plotRelativeDensities <-function(data, Group=NULL, idx=NULL, main="Densities") {
  if (is.null(Group)) Group = rep("A", ncol(data))
  Group = as.factor(Group)

  if (is.null(idx)) idx = 1;
  C = rainbow(nlevels(Group))[Group]

  # empty plot
  plot(density(log(na.omit(data[,1]))), type="n", ylim=c(0,2), xlim=c(-10,10), main=main)
  abline(v=0, col="blue")

  # calculate all relative ratios
  for (i in 1:ncol(data)) {
    if (i != idx) lines(density(log(na.omit(data[,i]/data[,idx]))), col=C[i]);
  }
}
