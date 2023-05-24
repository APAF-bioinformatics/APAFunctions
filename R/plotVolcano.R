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

  # TO DO:
  # Change the axis labels, titles, and maybe colouring based on the exp and PeporProt

  volcdat <- data.frame(FC = FC, pval = pval)
  #volcdat$annot <- rownames(volcdat)
  volcdat$sig <- ifelse(volcdat$pval>pvalcutoff, "no",
                        ifelse(is.na(volcdat$pval), "no",
                               ifelse(abs(volcdat$FC)>FCCutoff, "yes", "no")))
  #subset <- subset(volcdat, volcdat$sig == "yes")
  #subset <- order(subset$FC, decreasing = TRUE)
  #volcdat$labs <- ifelse(volcdat$annot %in% ,
  #                       volcdat$annot, "")

  ggplot(volcdat, aes(x = FC, y = -log10(pval), col = sig, label = labs)) +
    geom_point(show.legend = F) + # can show legend if wanting to
    theme_bw() +
    geom_hline(yintercept = -log10(pvalcutoff)) +
    geom_vline(xintercept = c(abs(-FCCutoff), FCCutoff)) +
    scale_color_manual(values = c("no" = "gray70", "yes" = "red")) #+
  #geom_text(aes(label = ifelse(labs == "", "", labs)), col = "black")


}

#####################################################
# The previous non-ggplot solution
#####################################################
# if (PeporProt != "Protein" && PeporProt != "Peptide") stop("plotVolcano() only supports 'Protein' or 'Peptide'.")
#
#   legendtext <- seq(from = 0, to = round(max(FC+5), digits = -1), by = 5)
#
#   if (exp == "SWATH"){
#   colorintens <- colorRampPalette(c("#ED90A4", "#ABB150", "#00C1B2", "#ACA2EC"))(max(legendtext))
#   colorspace <- colorintens[as.numeric(counts)]
#
#   if (PeporProt == "Protein"){
#     png(paste(comp.idx, "VolcanoProteins.png"), 2000, 2000, res=300)
#     legendtitle = "Protein count"
#     maintitle = "Volcano proteins %"
#   } else if (PeporProt == "Peptide"){
#     png(paste(comp.idx, "VolcanoPeptides.png"), 2000, 2000, res=300)
#     legendtitle = "Peptide count"
#     maintitle = "Volcano peptides %"
#   }
# }
#
#  else if (exp == "TMT") {
#   png(paste('Volcano plot for targeted', comp.idx, '.png', sep=''), 2000, 2000, res=300)
#   maintitle = paste("Protein volcano plot", comp.idx)
#   colorspace <- 'black'
# }
#
# plot(log(FC), -log(pval), main=maintitle, xlab="log(FC)", ylab="-log(p-value)", pch=20, cex=1.5, xlim=c(-3,3), ylim=c(0, min(max(-log(pval), na.rm=TRUE), 20)), col = colorspace)
# legend("topleft", title=legendtitle, legend=legendtext, col=colorintens[legendtext],pch=20, box.col = NA)
# abline(v=log(FCCutoff))
# abline(v=-log(FCCutoff))
# abline(h=-log(pvalcutoff))
# dev.off()
