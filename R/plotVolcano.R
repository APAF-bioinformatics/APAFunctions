#' The Volcano plot function for all APAF scripts
#'
#' @param exp SWATH or TMT
#' @param FC
#' @param pval
#' @param counts  number of times target has been detected
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on the plot. No other options are accepted
#' @param FCCutoff  Fold Change cut off. Should be passed in from the script environment, but defaults to 1.5
#' @param ProtStats ProteinName, pval, FC
#'
plotVolcano <- function(exp, FC, pval, counts, PeporProt, comp.idx, FCCutoff=1.5, pvalcutoff=0.05){
  if (PeporProt != "Protein" && PeporProt != "Peptide") stop("plotVolcano() only supports 'Protein' or 'Peptide'.")

    legendtext <- seq(from = 0, to = round(max(FC+5), digits = -1), by = 5)

    if (exp == "SWATH"){
    colorintens <- colorRampPalette(c("#ED90A4", "#ABB150", "#00C1B2", "#ACA2EC"))(max(legendtext))
    colorspace <- colorintens[as.numeric(counts)]

    if (PeporProt == "Protein"){
      png(paste(C,"VolcanoProteins.png"), 2000, 2000, res=300)
      legendtitle = "Protein count"
      maintitle = "Volcano proteins %"
    } else if (PeporProt == "Peptide"){
      png(paste(C,"VolcanoPeptides.png"), 2000, 2000, res=300)
      legendtitle = "Peptide count"
      maintitle = "Volcano peptides %"
    }
  }

   else if (exp == "TMT") {
    png(paste('Volcano plot for targeted', comp.idx, '.png', sep=''), 2000, 2000, res=300)
    maintitle = paste("Protein volcano plot", comp.idx)
    colorspace <- 'black'
  }

  plot(log(FC), -log(pval), main=maintitle, xlab="log(FC)", ylab="-log(p-value)", pch=20, cex=1.5, xlim=c(-3,3), ylim=c(0, min(max(-log(pval), na.rm=TRUE), 20)), col = colorspace)
  legend("topleft", title=legendtitle, legend=legendtext, col=colorintens[legendtext],pch=20, box.col = NA)
  abline(v=log(FCCutoff))
  abline(v=-log(FCCutoff))
  abline(h=-log(pvalcutoff))
  dev.off()
}
