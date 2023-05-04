#' Compute PCA, no plots
#'
#' @param data The prot.ag dataframe. Long format intensity? data with protein name as first column.
#' @param scaleR TO BE DETERMINED - leave as FALSE as on GenePattern - manual row scaling?
#' @param scaleC Pass scaling argument to prcomp function for PCA. Default TRUE.
#' @param k Number of components to compute. Defaults to number of variables - 1.
#'
#' @return The components of the PCA that are labelled for easy plotting 
PCA_calc <- function(data, scaleR = FALSE, scaleC = TRUE, k = min(dim(data)) - 2) {
  if (k > min(dim(data) - 2))
    warning("The number of components was too large compared to the data and was adjusted accordingly")
  k <- min(k, min(dim(data)) - 2)
  data <- log(t(data[,-1]+1)) # removing protein column, transposing, adding 1 to avoid log(0)
  if (scaleR) { #TBD if worth using, APAFStats::PCA did this, but not APAFSwath::PCA
    row.nrm <- apply(data, 1, sd)
    row.nrm <- pmax(row.nrm, 1e-04)
    data <- sweep(data, 1, row.nrm, FUN = "/")
  }
  result <- try(prcomp(data, retx = TRUE, scale = scaleC), 
                silent = TRUE)
  if (inherits(result, "try-error")) 
    stop("Failed to Calculate Principal Components")
  components <- list(componentVariances = (result$sdev^2)[1:k], componentScores = result$x[, 1:k], 
                     componentLoadings = result$rotation[, 1:k], summary = summary(result))
  return(components)
}
