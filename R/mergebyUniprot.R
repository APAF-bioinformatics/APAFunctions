#' Merge the Uniprot annotation with the results
#'
#' @param input_table
#' @param ANOVA_TABLE TRUE or FALSE whether merging in the ANOVA results
#'
#' @return
#' @export
#'
#' @examples
mergebyUniprot <- function(input_table, ANOVA_TABLE=FALSE){ # KR - don't like having the ANOVA_TABLE boolean, but move on for now
  tmp = input_table
  tmp$order <- 1:nrow(tmp)
  if (ncol(annotated)==4){
    tmp.f <- merge(tmp, annotated, by.x = "Protein", by.y = "RefSeqQuery", all.x=TRUE, sort=FALSE)
  } else {
    tmp$Uniprot = sapply(strsplit(tmp$Protein, split = "[|]"), function(x) x[2])
    tmp.f = merge(tmp, annotated, by.x="Uniprot", by.y=1, all.x=TRUE, sort=FALSE)
  }
  if (ANOVA_TABLE==TRUE){
    tmp.f = tmp[order(tmp$order),]
    tmp.f <- tmp.f[,-which(colnames(tmp.f) %in% c("order"))]
  }
  return(tmp.f)
}
