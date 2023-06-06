#' Plotting any APAF boxplots. To be done using ggplot, and take in any experiment type
#'
#' @param exp the experiment type
#' @param data for TMT this is the ion data matrix
#' @param normalization  The types of normalisation from APAFunctions::normalize() should  be listed here, but those are yet to be finalised
#' @param design the processed design file object that contains labels
#' @param Group groups that is represented as factors
#'
#' @import grid
#' @import gridBase
#' @import patchwork
#'
plotBoxPlots <- function(exp=c("SWATH", "TMT"), data, normalization, design, Group){
  grp_colors = rainbow(nlevels(Group))

  if (exp == "SWATH"){
    png("DataOverall.png", 2000, 2000, res=300)
    layout(matrix(1:4, nrow=2))
    APAFunctions::plotRelativeDensities(data, Group, idx=1, main="Relative density ions no norm")
    # KR - I know normalisation has already been done in the SWATHANOVA.R script, but duplicating here is not too expensive
    data.norm <- APAFunctions::normalize(exp, data_raw=data, normalization, Group)
    APAFunctions::plotRelativeDensities(data=data.norm, Group, idx=1, main=normalization)
    boxplot(log(data), main="Boxplots ions no norm", las=2, pch=20)
    boxplot(log(data.norm), main=normalization, las=2, pch=20)
    dev.off()
  }

  else if (exp == "TMT"){
    # raw and normalised to total ion count
    # KR - this ggplot is not displaying when running TMT as on the evening 24/05/2023

    # plot limma densities
    limmadatin <- limma::plotDensities(log(na.omit(data+.5)))
    xlimma <- as.data.frame(limmadatin$X)
    ylimma <- as.data.frame(limmadatin$Y)
    colnames(xlimma) <- as.character(Replicate)
    colnames(ylimma) <- as.character(Group)
    xlimma <- melt(xlimma, value.name = "X", variable.name = "Replicate")
    ylimma <- melt(ylimma, value.name = "Y", variable.name = "Group")
    limmadat <- cbind(xlimma, ylimma)
    head(limmadat)

    p1 <- ggplot(limmadat, aes(x = X, y = Y, group = Group, col = as.character(Group))) +
      geom_smooth(se = FALSE) +
      theme_classic() +
      theme(text = element_text(size = 14),
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom") +
      labs(x = "Intensity", y = "Density", title = paste(normalization, "normalised"),
           col = "Group")
    p1
    dat.ggplot <- data
    dat.ggplot$id <- rownames(data)
    dat.ggplot <- melt(dat.ggplot, id.vars = "id")
    dat.ggplot$Label <- gsub(".*\\.", "", dat.ggplot$variable)
    dat.ggplot <- merge(dat.ggplot, designLong, by = "Label")
    p2 <- ggplot(dat.ggplot, aes(x = variable, y = log2(value), fill = Group)) +
      geom_boxplot(show.legend = F) +
      theme_classic() +
      theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
      labs(x = "", y = "")

    #library(patchwork)
    plot <- p1+p2
    png("BoxplotDensity_GS.png", width = 3500, height = 1700, res = 300)
    print(plot)
    dev.off()

    png("BoxplotDensity.png", width=3500, height=1700,res=300)
    layout(matrix(1:2, nrow=1))
    par(mar=c(13,4,4,2)+.1)
    limma::plotDensities(log(na.omit(data+.5)), col=grp_colors[Group], legend=FALSE, main=paste(normalization, " normalised"))
    legend('topright', fill=grp_colors[1:nlevels(Group)], legend=levels(Group))
    boxplot(log(data[, order(Group)]+.5), las=2, col=grp_colors[Group[order(Group)]],
            main=paste(normalization, "and IRS normalised"),
            cex.axis=0.6)
    dev.off()
  }
}
