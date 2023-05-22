#' Generic correlation plot function
#'
#' @param data
#' @param Group
plotCorelationsPlots <- function(data, Group){

  # This function is just used to run pairs()
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
  }

  for (level in levels(Group)) {
    d_no_NA <- na.omit(data[, level, drop=FALSE]) # KR - Took a stab here, not sure if this is what it's meant to do
    if(ncol(d_no_NA) > 1) {
      png(paste("Cor", level, ".png", sep=""), 2000, 2000,res=300)
      pairs(log(dd+.5), lower.panel = panel.smooth, upper.panel = panel.cor, main=level)
      dev.off()
    }
}
