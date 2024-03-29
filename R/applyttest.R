#' Apply t-test within comparions - includes check with
#'
#' @param data
#' @param Group
#' @param doLogs
#' @param numerator
#'
#' @return
#' @export
#'
#' @examples
applyttest <- function(data, Group, doLogs=TRUE,  numerator=levels(Group)[1]) {
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
