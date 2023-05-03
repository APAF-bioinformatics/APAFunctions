#' The Volcano plot function for all APAF scripts
#'
#' @param ProtPepCounts ProteinName, NumberofPeptides
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on the plot. No other options are accepted 
#' @param FCcutoff  Fold Change cut off. Should be passed in from the script environment, but defaults to 1.5
#' @param pvalcutoff P-value cut off. Should be passed in from the script environment, but defaults to 0.05
#' @param ProtStats ProteinName, pval, FC
#'
plotVolcano <- function(ProtPepCounts = ag.pepN, ProtStats=ProtStats, PeporProt, FCcutoff=1.5, pvalcutoff=0.05){
  if (PeporProt != "Protein" && PeporProt != "Peptide") stop("plotVolcano() only supports 'Protein' or 'Peptide'.")
  
  legendtext <- seq(from = 0, to = round(max(ProtPepCounts[,2]+5), digits = -1), by = 5)
  colintens <- colorRampPalette(c("#ED90A4", "#ABB150", "#00C1B2", "#ACA2EC"))(max(legendtext))
  # if I was clever it would just put peptide or protein in res.[pep|prot] and the labels. Leaving for now 
  if (PeporProt == "Protein"){
    png(paste(C,"VolcanoProteins.png"), 2000, 2000, res=300)
    plot(log(res.prot[,2]), -log(res.prot[,1]), main="Volcano proteins %",xlab="Log FC", ylab="Log P-value", pch=20, cex=1.5, xlim=c(-3,3), col = colintens[as.numeric(ProtPepCounts[,2])])
    legend("topleft",title="Protein count", legend = legendtext, col=colintens[legendtext],pch=20, box.col = NA)
  } else if (PeporProt == "Peptide"){
    png(paste(C,"VolcanoPeptides.png"), 2000, 2000, res=300)
    plot(log(ProtStats[,3]), -log(ProtStats[,2]), main="Volcano peptides %",xlab="Log FC", ylab="Log P-value", pch=20, cex=1.5, xlim=c(-3,3), ylim=c(0, min(max(-log(ProtStats[,2]), na.rm=TRUE), 20)), col = colintens[as.numeric(ProtPepCounts[,2])])
    legend("topleft",title="Peptide count", legend = legendtext, col=colintens[legendtext],pch=20, box.col = NA)
  }
  abline(v=log(FCcutoff))
  abline(v=-log(FCcutoff))
  abline(h=-log(pvalcutoff))
  dev.off()
<<<<<<< HEAD
}#' The Volcano plot function for all APAF scripts
#'
#' @param ProtPepCounts ProteinName, NumberofPeptides
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on the plot. No other options are accepted 
#' @param FCcutoff  Fold Change cut off. Should be passed in from the script environment, but defaults to 1.5
#' @param pvalcutoff P-value cut off. Should be passed in from the script environment, but defaults to 0.05
#' @param ProtStats ProteinName, pval, FC
#'
plotVolcano <- function(ProtPepCounts = ag.pepN, ProtStats=ProtStats, PeporProt, FCcutoff=1.5, pvalcutoff=0.05){
  if (PeporProt != "Protein" && PeporProt != "Peptide") stop("plotVolcano() only supports 'Protein' or 'Peptide'.")
  
  legendtext <- seq(from = 0, to = round(max(ProtPepCounts[,2]+5), digits = -1), by = 5)
  colintens <- colorRampPalette(c("#ED90A4", "#ABB150", "#00C1B2", "#ACA2EC"))(max(legendtext))
  # if I was clever it would just put peptide or protein in res.[pep|prot] and the labels. Leaving for now 
  if (PeporProt == "Protein"){
    png(paste(C,"VolcanoProteins.png"), 2000, 2000, res=300)
    plot(log(res.prot[,2]), -log(res.prot[,1]), main="Volcano proteins %",xlab="Log FC", ylab="Log P-value", pch=20, cex=1.5, xlim=c(-3,3), col = colintens[as.numeric(ProtPepCounts[,2])])
    legend("topleft",title="Protein count", legend = legendtext, col=colintens[legendtext],pch=20, box.col = NA)
  } else if (PeporProt == "Peptide"){
    png(paste(C,"VolcanoPeptides.png"), 2000, 2000, res=300)
    plot(log(ProtStats[,3]), -log(ProtStats[,2]), main="Volcano peptides %",xlab="Log FC", ylab="Log P-value", pch=20, cex=1.5, xlim=c(-3,3), ylim=c(0, min(max(-log(ProtStats[,2]), na.rm=TRUE), 20)), col = colintens[as.numeric(ProtPepCounts[,2])])
    legend("topleft",title="Peptide count", legend = legendtext, col=colintens[legendtext],pch=20, box.col = NA)
  }
  abline(v=log(FCcutoff))
  abline(v=-log(FCcutoff))
  abline(h=-log(pvalcutoff))
  dev.off()
=======
>>>>>>> fa008a906a5bf777c33ee18adbd35e63ddd69098
}