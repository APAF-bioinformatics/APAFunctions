#' An APAF wrapper for t.test() that works for both proteins and peptides
#'
#' @param data input matrix, is as subset of either prot.ag or pep.ag
#' @param Group  Group for the labels
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on which type of matrix is being used
#' @param doLogs take the log() of the matrix. Defaults to TRUE
#' @param numerator the Group for the comparison, defaults to levels(Group[1])
#'
applyttest_old <- function(data, Group, doLogs=TRUE,  numerator=levels(Group)[1]) {
  Group = as.factor(as.vector(Group))
  if (!(nlevels(Group) == 2)) stop("T test needs two levels only in group.");
  data = as.matrix(data)

  pval = rep(NA, nrow(data))
  FC = rep(NA, nrow(data))

  for (i in 1:nrow(data)) {
    rowdat <- as.data.frame(data[i,])
    rowdat$Group <- as.character(Group)
    colnames(rowdat)[1] <- "value"

    # Count the number of non-NA values in each group
    group_counts <- rowdat %>%
      group_by(Group) %>%
      summarise(count = sum(sum(!is.na(as.numeric(value)))))

    # Check if the protein meets the threshold criteria
    if (sum(group_counts$count >= 3) ==2) {
      pval[i] <- t.test(rowdat[,1] ~ Group, var.equal=TRUE)$p.value
      if (doLogs) {
        FC[i] = exp(mean(subset(rowdat, rowdat$Group==numerator)$value)/mean(subset(rowdat, !rowdat$Group==numerator)$value))
      } else {
        FC[i] = mean(subset(rowdat, rowdat$Group==numerator)$value)/mean(subset(rowdat, !rowdat$Group==numerator)$value)
      }
    } else {
      NULL
    }
  }
  res = (data.frame(pval, FC)) #res.prot has just pval and FC
  return(res)
}
