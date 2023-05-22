#' TO DETERMINE: make_CVs
#'
#' @param data
#' @param replicate
#'
#' @examples
make_CVs <- function(data, replicate) {
  cvs = matrix(NA, nrow=nrow(data), ncol=nlevels(as.factor(replicate) ))
  colnames(cvs) = levels(as.factor(replicate))
  rownames(cvs) = rownames(data)

  for(i in 1:nrow(data))
    cvs[i,] = aggregate(t(data[i,]), by=list(replicate), function(x) sd(x)/mean(x))[,2]

  return(cvs)
}
