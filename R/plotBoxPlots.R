#' Plotting any APAF boxplots. To be done using ggplot, and take in any experiment type
#'
#' @param experiment the experiment type
#' @param dataraw Data before normalisation
#' @param datanorm Data after normalisation
#' @param normalisation  The type(s) of normalisation from APAFunctions::normalise()
#' @param design The processed design file object that contains labels (TMT)
#' @param Group Groups representing columns of the data
#'
#' @import grid
#' @import gridBase
#' @import patchwork
#'
plotBoxPlots <- function(experiment=c("SWATH", "TMT"), dataraw, datanorm, normalisation, design, Group){
  ldat <- list(dataraw, datanorm)
  for (i in 1:2){
    dat <- ldat[[i]]
    limmadatin <- limma::plotDensities(log(na.omit(dat+.5)))
    xlimma <- as.data.frame(limmadatin$X)
    ylimma <- as.data.frame(limmadatin$Y)
    colnames(xlimma) <- as.character(colnames(dat))
    colnames(ylimma) <- as.character(Group)
    xlimma <- suppressMessages(melt(xlimma, value.name = "X", variable.name = "Sample"))
    ylimma <- suppressMessages(melt(ylimma, value.name = "Y", variable.name = "Group"))
    limmadat <- cbind(xlimma, ylimma)
    head(limmadat)
    ldat[[i+2]] <- limmadat

    dat.ggplot <- dat
    dat.ggplot$id <- rownames(dat)
    dat.ggplot <- melt(dat.ggplot, id.vars = "id")
    if (experiment == "SWATH"){
      dat.ggplot <- merge(dat.ggplot, design, by.x = "variable", by.y = "Sample.Name")
    } else if (experiment == "TMT"){
      dat.ggplot$Label <- gsub(".*\\.", "", dat.ggplot$variable)
      dat.ggplot <- merge(dat.ggplot, design, by = "Label")
    }
    dat.ggplot$variable <- factor(dat.ggplot$variable, levels = colnames(dat))
    ldat[[i+4]] <- dat.ggplot
  }

  p1 <- ggplot(ldat[[3]], aes(x = X, y = Y, group = Group, col = as.character(Group))) +
    geom_smooth(se = FALSE) +
    theme_classic() +
    theme(text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "", y = "Density", col = "Group", title = "Pre-normalisation")

  p2 <- ggplot(ldat[[5]], aes(x = variable, y = log2(value), fill = Group)) +
    geom_boxplot(show.legend = F) +
    theme_classic() +
    theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
    labs(x = "", y = "")

  p3 <- ggplot(ldat[[4]], aes(x = X, y = Y, group = Group, col = as.character(Group))) +
    geom_smooth(se = FALSE) +
    theme_classic() +
    theme(text = element_text(size = 14),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom") +
    labs(x = "Intensity", y = "Density", col = "Group",
         title = paste0("Normalised (", normalisation, ")"))

  p4 <- ggplot(ldat[[6]], aes(x = variable, y = log2(value), fill = Group)) +
    geom_boxplot(show.legend = F) +
    theme_classic() +
    theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
    labs(x = "", y = "")

  plot <- (p1+p2)/(p3+p4) & theme(legend.position = "bottom")
  png("normalisationQC.png", width = 3000, height = 2000, res = 300)
  print(plot + plot_layout(guides = "collect"))
  dev.off()
}
