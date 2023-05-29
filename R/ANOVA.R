#' Runs an ANOVA and returns Anova.adj and Anova.idx
#'
#' @param data
#' @param Group
#' @param FCcutoff
#' @param pvalcutoff
ANOVA <- function(data, Group, FCcutoff, pvalcutoff) {
  Anova = rep(NA, nrow(data))

  data.ag = aggregate(t(data), by=list(Group=Group), FUN=function(v){exp(mean(log(na.omit(v))))})
  Means = t(data.ag[,-1])
  colnames(Means) = paste("Means",data.ag[,1])
  MaxFC = apply(Means, 1, FUN=function(v){max(v)/min(v)})

  for (i in 1:nrow(data)) {
    v = t(data[i,])
    nna.idx = !is.na(v)
    an.res = try(anova(lm(log(v[nna.idx]+.5) ~ Group[nna.idx, drop=TRUE]))[1,"Pr(>F)"])
    if (!inherits(an.res, "try-error")) Anova[i] = an.res;
  }

  Anova.adj = p.adjust(Anova, method = "fdr")
  Anova.idx = !is.na(MaxFC) & (MaxFC > FCcutoff) & !is.na(Anova) & (Anova < pvalcutoff)

  return(list(Anova=Anova, Anova.adj=Anova.adj, Anova.idx=Anova.idx, Means=Means, MaxFC=MaxFC))
}
