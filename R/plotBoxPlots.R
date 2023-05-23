#' Plotting any APAF boxplots. To be done using ggplot, and take in any experiment type
#'
#' @param exp the experiment type
#' @param data for TMT this is the ion data matrix
#' @param design the processed design file object that contains labels
#' @param Group groups that is represented as factors
plotBoxPlots <- function(exp, data, design, Group){
grp_colors = rainbow(nlevels(Group))
  if (exp=="TMT"){
    # raw and normalised to total ion count
    dat.ggplot <- data
    dat.ggplot$id <- rownames(data_irs)
    dat.ggplot <- melt(dat.ggplot, id.vars = "id")
    dat.ggplot$Label <- gsub(".*\\.", "", dat.ggplot$variable)
    dat.ggplot <- merge(dat.ggplot, designLong, by = "Label")
    p1 <- ggplot(dat.ggplot, aes(x = variable, y = log2(value), fill = Group)) +
    geom_boxplot(show.legend = F) +
    theme_classic() +
    theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
    labs(x = "", y = "")
    png(paste("Boxplot raw and", SampleLoadNorm, "norm.png"), 2000, 2000, res=300)
    layout(matrix(1:4, ncol=2))
    p1+p1+p1/p1}
    dev.off()

    # density boxplot
    png("BoxplotDensity.png", width=3500, height=1700,res=300)
    layout(matrix(1:2, nrow=1))
    par(mar=c(13,4,4,2)+.1)
    limma::plotDensities(log(na.omit(data_irs+.5)), col=grp_colors[Group], legend=FALSE, main=paste(SampleLoadNorm, "and IRS normalised"))
    legend('topright', fill=grp_colors[1:nlevels(Group)], legend=levels(Group))
    boxplot(log(data[, order(Group)]+.5), las=2, col=grp_colors[Group[order(Group)]],
            main=paste(SampleLoadNorm, "and IRS normalised"),
            cex.axis=0.6)
    dev.off()
}
