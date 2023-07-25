#' The Volcano plot function for all APAF scripts
#'
#' @param experiment SWATH or TMT
#' @param FC
#' @param pval
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on the plot. No other options are accepted
#' @param comp.idx
#' @param pvalcutoff
#' @param FCcutoff  Fold Change cut off. Should be passed in from the script environment, but defaults to 1.5
#' @param interactive TRUE or FALSE to plot an interactive volcano plot into a HTML file
#'
plotVolcano <- function(experiment=c("SWATH", "TMT"), FC, pval, names, comp.idx, FCcutoff=1.5, pvalcutoff=0.05, saveInteractive = TRUE){

  savename <- paste0("Volcano plot - comp", comp.idx)
  if (experiment == "TMT"){
    title = paste0("TMT: ", savename)
  } else if (experiment == "SWATH") {
    title = paste0("SWATH: ", savename)
  } else { title = "We haven't covered this experiment yet" }

  volcdat <- data.frame(FC = log2(FC), pval = pval, names = names)
  #volcdat$annot <- rownames(volcdat)
  volcdat$sig <- ifelse(volcdat$pval>pvalcutoff, "no",
                        ifelse(is.na(volcdat$pval), "no",
                               ifelse(abs(volcdat$FC)>FCcutoff, "yes", "no")))
  subsig <- subset(volcdat, volcdat$sig == "yes")
  subsig <- subsig[order(abs(subsig$pval), decreasing = FALSE),]

  xmin <- floor(summary(volcdat$FC)[1][[1]])
  xmax <- ceiling(summary(volcdat$FC)[6][[1]])
  if (FCcutoff > xmax){
    xmin <- -FCcutoff
    xmax <- FCcutoff
  }
  #xbreaks <- seq(from = xmin, to = xmax, by = 1)
  #xbreaks <- c(xbreaks, -FCcutoff, FCcutoff)
  #xbreakcol <- c(rep("black", length(xbreaks)-2), "red", "red")
  ymax <- ceiling(-log10(summary(volcdat$pval)[1][[1]]))
  #ybreaks <- seq(from = 0, to = ymax, by = 1)
  #ybreaks <- append(ybreaks, -log10(pvalcutoff))
  #ybreakcol <- c(rep("black", length(ybreaks)-1), "red")

  plot <- ggplot(volcdat, aes(x = FC, y = -log10(pval), col = sig, text = names)) +
    geom_point(show.legend = F) +
    theme_classic() +
    #scale_x_continuous(breaks = xbreaks, labels = c(as.integer(xbreaks[1:(length(xbreaks)-2)]), -FCcutoff, FCcutoff), limits = c(xmin,xmax)) +
    # if pvalcutoff is 0.01 or is same as other break, will be overlapped on graph
    #scale_y_continuous(breaks = ybreaks, labels = c(ybreaks[1:(length(ybreaks)-1)], paste0("(", pvalcutoff, ")")), limits = c(0,ymax)) +
    geom_hline(yintercept = -log10(pvalcutoff), linetype = "dashed", alpha = 0.4, col = "red") +
    geom_vline(xintercept = c(-FCcutoff, FCcutoff), linetype = "dashed", alpha = 0.4, col = "red") +
    scale_color_manual(values = c("no" = "gray70", "yes" = "red")) +
    theme(text = element_text(size = 16),
          #axis.text.x = element_text(color = as.character(xbreakcol)),
          #axis.text.y = element_text(color = as.character(ybreakcol)),
          plot.title = element_text(hjust = 0.5))
  #labs(x = expression("Fold change (log"[2]*")"), y = expression("-log"[10]~"(p-value)"),
  #     title = title)
  plotpng <- plot + labs(x = expression("Fold change (log"[2]*")"), y = expression("-log"[10]~"(p-value)"), title = title)
  plotint <- plot + labs(x = "Fold change (log2)", y = "-log10(p-value)", title = title) + guides(col = "none")

  png(paste0(savename, ".png"), res = 300, height = 2500, width = 3000)
  print(plotpng)
  dev.off()

  if (saveInteractive == TRUE){
    htmlwidgets::saveWidget(ggplotly(plotint, tooltip = "names"), paste0(savename, "_interactive.html"))
  }
}
