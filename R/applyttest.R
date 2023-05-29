#' An APAF wrapper for t.test() that works for both proteins and peptides
#'
#' @param data input matrix, is as subset of either prot.ag or pep.ag
#' @param Group  Group for the labels
#' @param PeporProt Specify 'Protein' or 'Peptide' depending on which type of matrix is being used
#' @param doLogs take the log() of the matrix. Defaults to TRUE
#' @param numerator the Group for the comparison, defaults to levels(Group[1])
#'
applyttest <- function(data, Group, PeporProt=c("Peptide", "Protein"), doLogs=TRUE,  numerator=levels(Group)[1]) {
   # TO DO: have better error handling so that we don't get "Error in t.test.default(log(v)) : not enough 'x' observations"
  if (PeporProt != "Peptide" && PeporProt != "Protein") stop("applyttest() only supports 'Protein' or 'Peptide'.")

  Group = as.factor(as.vector(Group))
  if (!(nlevels(Group) == 2)) stop("T test needs two levels only in group.");


  if (PeporProt == "Protein"){
    data = as.matrix(data)
  } else if (PeporProt == "Peptide"){ #have to lose some extraneous colums from pep.ag
    Protein = data[,1]
    data = data[,-c(1:2)]
  }

  pval = rep(NA, nrow(data))
  FC = rep(NA, nrow(data))

  if (PeporProt == "Peptide"){
    if (doLogs == TRUE) data = log(data+1);
  } else if(PeporProt == "Protein") {
    if (doLogs == TRUE) data = log(data);
  }


  if (PeporProt == "Protein"){
    pval = sapply(1:nrow(data), FUN=function(i){
      res = NA
      tmp = try(t.test(data[i,] ~ Group, var.equal=TRUE, silent=TRUE))
      if (!inherits(tmp, "try-error")) res = tmp$p.value
      res
    })
  }

  mn.1 =  apply(data[,Group == numerator], 1, FUN=function(v){mean(na.omit(v))})
  mn.2 =  apply(data[,Group == setdiff(levels(Group), numerator)], 1, FUN=function(v){mean(na.omit(v))})

  if (doLogs) {
    FC = exp(mn.1)/exp(mn.2)
  } else {
    FC = mn.1/mn.2
  }
  if (PeporProt == "Protein"){
    res = (data.frame(pval, FC)) #res.prot has just pval and FC
  }


  if (PeporProt == "Peptide"){
    # calculate pval by protein from peptide ratios
    data.pval = aggregate(FC, by=list(Protein=Protein), FUN=function(v){ res=NA;tmp=try(t.test(log(v)));
    if (!inherits(tmp, "try-error")) res=tmp$p.value; res})

    # calculate FC by protein
    data.FC = aggregate(FC, by=list(Protein=Protein), FUN=function(v){ exp(mean(log(na.omit(v))))})
    if (sum(data.FC[,1] != data.pval[,1]) > 0) stop("Error in calculating peptide level tests.")
    res = data.frame(data.pval, data.FC[,-1])
    colnames(res) = c("Protein", "pval", "FC")
    res = res[match(unique(Protein), res[,1]),]  #res.pep has Protein, pval, FC
  }
  return(res)
}
