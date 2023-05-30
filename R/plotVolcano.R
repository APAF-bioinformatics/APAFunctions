#' The Volcano plot function for all APAF scripts
#'
#' @param exp SWATH or TMT
#' @param FC
#' @param pval
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on the plot. No other options are accepted
#' @param comp.idx
#' @param pvalcutoff
#' @param FCcutoff  Fold Change cut off. Should be passed in from the script environment, but defaults to 1.5
#' @param numlabelled Number of points to label on graph, chooses most extreme
#'
plotVolcano <- function(exp=c("SWATH", "TMT"), FC, pval, PeporProt=c("Peptide", "Protein"), comp.idx, FCcutoff=1.5, pvalcutoff=0.05, numlabelled = 0){

  #TO DO: clarify if specifying peptide or protein is necessary

  if (exp == "TMT"){
    title = "Hello, I'm TMT"
  } else if (exp == "SWATH") {
    title = "Hello, I'm SWATH"
  } else { title = "We haven't covered this exp yet, oops" }

  volcdat <- data.frame(FC = log2(FC), pval = pval)
  #volcdat$annot <- rownames(volcdat)
  volcdat$sig <- ifelse(volcdat$pval>pvalcutoff, "no",
                        ifelse(is.na(volcdat$pval), "no",
                               ifelse(abs(volcdat$FC)>FCcutoff, "yes", "no")))
  volcdat$rowname <- rownames(volcdat)
  subset <- subset(volcdat, volcdat$sig == "yes")
  subset <- subset[order(abs(subset$pval), decreasing = FALSE),]
  if (numlabelled > nrow(subset)) {
    numlabelled <- nrow(subset)
  }

  xmin <- floor(summary(volcdat$FC)[1][[1]])
  xmax <- ceiling(summary(volcdat$FC)[6][[1]])
  if (FCcutoff > xmax){
    xmin <- -FCcutoff
    xmax <- FCcutoff
  }
  xbreaks <- seq(from = xmin, to = xmax, by = 1)
  xbreaks <- c(xbreaks, -FCcutoff, FCcutoff)
  xbreakcol <- c(rep("black", length(xbreaks)-2), "red", "red")
  ymax <- ceiling(-log10(summary(volcdat$pval)[1][[1]]))
  ybreaks <- seq(from = 0, to = ymax, by = 1)
  ybreaks <- append(ybreaks, -log10(pvalcutoff))
  ybreakcol <- c(rep("black", length(ybreaks)-1), "red")

  plot <- ggplot(volcdat, aes(x = FC, y = -log10(pval), col = sig, label = labs)) +
    geom_point(show.legend = F) + # can show legend if wanting to
    theme_classic() +
    scale_x_continuous(breaks = xbreaks, labels = c(as.integer(xbreaks[1:(length(xbreaks)-2)]), -FCcutoff, FCcutoff), limits = c(xmin,xmax)) +
    # if pvalcutoff is 0.01 or is same as other break, will be overlapped on graph
    scale_y_continuous(breaks = ybreaks, labels = c(ybreaks[1:(length(ybreaks)-1)], paste0("(", pvalcutoff, ")")), limits = c(0,ymax)) +
    geom_hline(yintercept = -log10(pvalcutoff), linetype = "dashed", alpha = 0.4, col = "red") +
    geom_vline(xintercept = c(-FCcutoff, FCcutoff), linetype = "dashed", alpha = 0.4, col = "red") +
    scale_color_manual(values = c("no" = "gray70", "yes" = "red")) +
    theme(text = element_text(size = 16),
          axis.text.x = element_text(color = xbreakcol),
          axis.text.y = element_text(color = ybreakcol),
          plot.title = element_text(hjust = 0.5)) +
    geom_label_repel(data = subset[1:numlabelled,], aes(label = rowname), fill = NA, col = "black", show.legend = F, nudge_x = 0.2, nudge_y = 0.1) +
    labs(x = expression("Fold change (log"[2]*")"), y = expression("-log"[10]~"(p-value)"),
         title = title)

  png(paste0("Volcano plot - comp ", comp.idx), res = 300, height = 2500, width = 3000)
  print(plot)
  dev.off()
}
