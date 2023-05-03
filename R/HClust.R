#' Clustering method. TO DO -- document later
#'
#' @param data
#' @param metric
#' @param scale
#' @param method
#' @param basefile
#' @param glabel
#' @param clabel
#' @param cutNumber
#' @param cutHeight
#' @param plot  #plotting should be pulled out. plotting is provided by plotColouredDendrogram()
#'
HClust <- function (data = data, metric = c("euclidean", "manhattan", "pearsonCorrelation"),
                    scale = FALSE, method = c("single", "complete", "average"),
                    basefile = "hclust", glabel = row.names(data), clabel = NULL,
                    cutNumber = NULL, cutHeight = NULL, plot = TRUE)
{
  if (nrow(data) < 3)
    stop("Need at least three samples for hierarchical clustering")
  if ((!is.null(cutNumber)) && ((cutNumber < 2) || (cutNumber >
                                                    nrow(data) - 1))) {
    cutNumber <- nrow(data) - 1
    if (cutNumber < 2) {
      stop("The number of clusters has to be at least two and the number of samples at least three")
    }
    else {
      warning("The selected number of clusters was adjusted to one less than the number of samples in the data")
    }
  }
  if (scale) {
    row.nrm <- apply(data, 1, sd)
    row.nrm <- pmax(row.nrm, 1e-04)
    data <- sweep(data, 1, row.nrm, FUN = "/")
  }
  if (metric == "pearsonCorrelation") {
    dd <- as.dist((1 - cor(t(data)))/2)
    result <- try(hclust(dd), silent = TRUE)
  }
  else {
    dd <- try(dist(data, method = metric), silent = TRUE)
    if (inherits(dd, "try-error"))
      Error("Failed to create distance Matrix")
    result <- try(hclust(dd, method = method), silent = TRUE)
  }
  if (inherits(result, "try-error"))
    Error("Failed to Calculate Cluster Tree")
  hplot <- paste(basefile, metric, method, sep = "-")
  if (plot) {
    plotColouredDendrogram(result, glabel = glabel, clabel = clabel,
                           sub = paste("method = ", method), xlab = "Assays")
  }
  clustID <- NULL
  clustres <- list(merge = result$merge, height = result$height,
                   order = result$order, labels = result$labels, method = result$method,
                   dist.method = result$dist.method)
  if (!is.null(cutHeight)) {
    clustID <- as.vector(cutree(result, h = cutHeight))
    names(clustID) <- rownames(data)
  }
  if (!is.null(cutNumber)) {
    clustID <- as.vector(cutree(result, k = cutNumber))
    names(clustID) <- rownames(data)
  }
  list(clustID = clustID, clustres = clustres, method = method,
       metric = metric)
}
