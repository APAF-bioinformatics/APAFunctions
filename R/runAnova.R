#' run ANOVA on data set based on groups
#'
#' @param data normalised data frame, row names as protein idenitifiers
#' @param Group Group information corresponding to the column names in data
#'
#' @return
#' @export
#'
#' @examples
#'
#' #### NEED TO FIX ####
#' ## look at TMT.R for more implementation ##
#'
runAnova <- function(data, Group) {
  # Create an empty column for threshold status
  anova_results <- as.data.frame(matrix(nrow = nrow(data), ncol = 4))
  colnames(anova_results) <- c("Protein", "obsThreshold", "notes", "Pval")

  # Iterate over each row (protein) in the dataset
  for (i in 1:nrow(data)) {
    # Extract the peptide intensities for the current protein
    intensities <- as.numeric(data[i, -ncol(data)])

    # Count the number of non-NA values in each group
    group_counts <- sapply(unique(str_extract(names(data)[-ncol(data)], "R\\d+")), function(x) sum(!is.na(intensities[grep(x, names(data)[-ncol(data)])])))

    # Check if the protein meets the threshold criteria
    if (sum(group_counts >= 3) >= 2) {
      anova_results$obsThreshold[i] <- "Passed"
      # Perform ANOVA, change NA to 0 if passed threshold
      intensities <- ifelse(is.na(intensities), 0, intensities)
      anova_res <- aov(intensities ~ str_extract(names(data)[-ncol(data)], "R\\d+"))
      #anova_res2 <- kruskal.test(intensities ~ str_extract(names(data)[-ncol(data)], "R\\d+"))
      anova_results$Pval[i] <- summary(anova_res)[[1]]$`Pr(>F)`[1]
      if (sum(group_counts >= 3) < length(Group)){ # TO DO: group number -1 for more than 3 groups
        anova_results$notes[i] <- paste("Group(s)", which(group_counts==FALSE), "didn't meet count threshold")
      }
    } else {
      anova_results$obsThreshold[i] <- "Not Passed"
      anova_results$Pval[i] <- NA
      anova_results$notes[i] <- paste("Group(s)", paste(which(group_counts==FALSE), collapse = ", "), "didn't meet count threshold")
    }
  }
  anova_results$Protein <- rownames(data)
  anova_results$FDR <- p.adjust(anova_results$Pval, method = "BH") # adjust pvalues
  return(anova_results) # return results
  # make count matrix - for t-tests
}
