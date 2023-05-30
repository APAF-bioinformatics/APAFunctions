#' The Volcano plot function for all APAF scripts
#'
#' @param exp SWATH or TMT
#' @param FC
#' @param pval
#' @param counts  number of times target has been detected
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on the plot. No other options are accepted
#' @param FCcutoff  Fold Change cut off. Should be passed in from the script environment, but defaults to 1.5
#' @param ProtStats ProteinName, pval, FC
#'
plotVolcano <- function(exp=c("SWATH", "TMT"), FC, pval, counts, PeporProt=c("Peptide", "Protein"), comp.idx, FCcutoff=2, pvalcutoff=0.05){

  if (exp == "TMT"){
    title = "Hello, I'm TMT"
  } else if (exp == "SWATH") {
    title = "Hello, I'm SWATH"
  } else { title = "We haven't covered this exp yet, oops" }

  volcdat <- data.frame(FC = FC, pval = pval)
  #volcdat$annot <- rownames(volcdat)
  volcdat$sig <- ifelse(volcdat$pval>pvalcutoff, "no",
                        ifelse(is.na(volcdat$pval), "no",
                               ifelse(abs(volcdat$FC)>FCcutoff, "yes", "no")))
  volcdat$rowname <- rownames(volcdat)
  subset <- subset(volcdat, volcdat$sig == "yes")
  subset <- subset[order(abs(subset$FC), decreasing = TRUE),]
  xmin <- floor(summary(volcdat$FC)[1][[1]])
  xmax <- ceiling(summary(volcdat$FC)[6][[1]])
  xbreaks <- seq(from = xmin, to = xmax, by = 1)
  xbreaks <- append(xbreaks, FCcutoff)
  xbreakcol <- c(rep("black", length(xbreaks)-1), "red")
  ymax <- ceiling(-log10(summary(volcdat$pval)[1][[1]]))
  ybreaks <- seq(from = 0, to = ymax, by = 1)
  ybreaks <- append(ybreaks, -log10(pvalcutoff))
  ybreakcol <- c(rep("black", length(ybreaks)-1), "red")


  ggplot(volcdat, aes(x = FC, y = -log10(pval), col = sig, label = labs)) +
    geom_point(show.legend = F) + # can show legend if wanting to
    theme_classic() +
    scale_x_continuous(breaks = xbreaks, limits = c(xmin,xmax)) +
    # if pvalcutoff is 0.01 or is same as other break, will be overlapped on graph
    scale_y_continuous(breaks = ybreaks, labels = c(ybreaks[1:(length(ybreaks)-1)], paste0("(", pvalcutoff, ")")), limits = c(ymin,ymax)) +
    geom_hline(yintercept = -log10(pvalcutoff), linetype = "dashed", alpha = 0.3, col = "red") +
    geom_vline(xintercept = c(abs(-FCcutoff), FCcutoff), linetype = "dashed", alpha = 0.3, col = "red") +
    scale_color_manual(values = c("no" = "gray70", "yes" = "red")) +
    theme(text = element_text(size = 16),
          axis.text.x = element_text(color = xbreakcol),
          axis.text.y = element_text(color = ybreakcol),
          plot.title = element_text(hjust = 0.5)) +
    geom_label_repel(data = subset[1:3,], aes(label = rowname), fill = NA, col = "black", show.legend = F, nudge_x = 0.2, nudge_y = 0.1) +
    labs(x = expression("Fold change (log"[2]*")"), y = expression("-log"[10]~"(p-value)"),
         title = title)
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
# abline(v=log(FCcutoff))
# abline(v=-log(FCcutoff))
# abline(h=-log(pvalcutoff))
# dev.off()
