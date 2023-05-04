#' Multiple linear regression that uses Groups. No plotting 
#'
#' @param mat matrix of ion intensities
#' @param Group Group from the design file  
mlrGroup <- function(mat, Group) {
  Group = as.factor(Group)
  
  if (length(Group) != ncol(mat)) stop("Matrix and group sizes don't match for mlr norm.");
  
  lev.idx = lapply(levels(Group), FUN=function(g){which(Group == g)})
  norm.list = lapply(lev.idx, FUN = function(i){mlrrep(mat[,i])});
  
  # check same colnames?
  norm.mat = matrix(NA, nrow(mat), ncol(mat))
  colnames(norm.mat) = colnames(mat)
  rownames(norm.mat) = rownames(mat)
  
  for(i in 1:length(lev.idx)) norm.mat[,lev.idx[[i]]] = as.matrix(norm.list[[i]]$mat.norm)
  mlrrep(norm.mat)
}
