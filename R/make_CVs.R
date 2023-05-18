#' TO DETERMINE: make_CVs
#'
#' @param df 
#' @param replicate 
#'
#' @return
#' @export
#'
#' @examples
make_CVs <- function(df, replicate) {
  cvs = matrix(NA, nrow=nrow(df), ncol=nlevels(as.factor(replicate) ))
  colnames(cvs) = levels(as.factor(replicate))
  rownames(cvs) = rownames(df)
  
  for(ii in 1:nrow(df))
    cvs[ii,] = aggregate(t(df[ii,]), by=list(replicate), function(x) sd(x)/mean(x))[,2]
  
  return(cvs)
}