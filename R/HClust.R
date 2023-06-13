#' Clustering method. If plot == TRUE, it plots using APAFunctions::plotColouredDendrogram()
#'
#' @param exp
#' @param data
#' @param metric
#' @param scale
#' @param method
#' @param basefile the base name for the type of hclust method used: e.g. hclust-pearsonCorrelation-complete
#' @param glabel
#' @param clabel
#' @param cutNumber
#' @param cutHeight
#' @param plot uses APAFunctions::plotColouredDendrogram(), defaults to TRUE
#'
HClust <- function(data, metric = c("euclidean", "manhattan", "pearsonCorrelation"),
                    scale = FALSE, method = c("single", "complete", "average"),
                    glabel = row.names(data), clabel = NULL,
                    cutNumber = NULL, cutHeight = NULL, plot = TRUE) {

    if (nrow(data) < 3) { stop("Need at least three samples for hierarchical clustering") }
    if ((!is.null(cutNumber)) && ((cutNumber < 2) || (cutNumber > nrow(data) - 1))) {
      cutNumber <- nrow(data) - 1
      if (cutNumber < 2) { stop("The number of clusters has to be at least two and the number of samples at least three") }
      else { warning("The selected number of clusters was adjusted to one less than the number of samples in the data") }
    }

    if (scale == TRUE) {
      row.nrm <- apply(data, 1, sd)
      row.nrm <- pmax(row.nrm, 1e-04)
      data <- sweep(data, 1, row.nrm, FUN = "/")
    }

    if (metric == "pearsonCorrelation") {
      d <- as.dist((1 - cor(t(data)))/2)
    } else { # any other distance metric, these are already defined in the stats library
      d <- try(dist(data, method = metric), silent = TRUE) # stats::dist() allows the methods "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
      if (inherits(d, "try-error")) { Error("Failed to create distance Matrix") }
    }

    hclust_res <- try(hclust(d, method = method), silent = TRUE) # processing ultimately comes from stats::hclust()

    if (inherits(hclust_res, "try-error")) { stop("Failed to Calculate Cluster Tree") }

    if (plot == TRUE) {
      png(paste0(paste("hclust", metric, method, sep = "_"), ".png"), res = 300, width = 3000, height = 2000)
      plot(hclust_res, cex = 0.1, hang = -1)
      dev.off()
    }

    clustID <- NULL
    clustres_list <- list(merge = hclust_res$merge, height = hclust_res$height,
                          order = hclust_res$order, labels = hclust_res$labels, method = hclust_res$method,
                          dist.method = hclust_res$dist.method)
    if (!is.null(cutHeight)) {
      clustID <- as.vector(cutree(hclust_res, h = cutHeight))
      names(clustID) <- rownames(data)
    }
    if (!is.null(cutNumber)) {
      clustID <- as.vector(cutree(hclust_res, k = cutNumber))
      names(clustID) <- rownames(data)
    }
    return(list(clustID = clustID, clustres_list = clustres_list, method = method, metric = metric))
}


