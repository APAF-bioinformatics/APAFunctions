#' Any normalisation of the data matrix applied to APAF data
#'
#' @param data The un-normalised raw ion data matrix, as a list
#' @param normalisation comes from the --normalisation flag and defaults to "total"
#' @param Group
#'
#' @return
#'
normalise <- function(data, normalisation, Group){

  if((grepl("Total", normalisation) == TRUE)) {
    tot = apply(data, 2, FUN = function(v){sum(na.omit(v))})
    data <- sweep(data, 2, tot/max(tot), "/")
  }

  if ((grepl("Median", normalisation) == TRUE)) {
    tot = apply(data, 2, function(x) median(na.omit(x)))
    data <- sweep(data, 2, tot/max(tot), "/")
  }

  if((grepl("IRS", normalisation) == TRUE)) {
    list.exp = list()
    for(i in 1:length(list.dat)) {
      list.exp[[i]]=data[,grep(paste0("R",i), colnames(data))]
    }

    # Calculate the protein sum for each batch
    list.rowsum = lapply(list.exp, function(x) apply(x,1, function(y) (sum(na.omit(y)))))
    irs <- as.data.frame(do.call("cbind",list.rowsum))

    # convert 0 to NA
    # GS - TO DO: future problem. Applying stats to missing data
    irs[irs==0] = NA
    colnames(irs) <- paste0("sum_R", 1:ncol(irs))
    rowsum.average <- apply(irs, 1, function(x) exp(mean(log(na.omit(x)))))

    # compute the scaling factor vectors
    irs.fac = sweep(irs, 1, rowsum.average, "/")
    list.irs.scaled = lapply(1:length(list.exp), function(x) sweep(list.exp[[x]], 1, irs.fac[,x], "/"))

    # make new data frame with normalised data
    data <- list.irs.scaled[[1]]
    data <- as.data.frame(do.call("cbind", list.irs.scaled))
  }

  if((grepl("MLR", normalisation) == TRUE)) {

    Group = as.factor(Group)
    if (length(Group) != ncol(data)) {stop("Matrix and group sizes don't match for mlr norm.")}

    level.idx = lapply(levels(Group), FUN=function(g){which(Group == g)})
    norm.list = lapply(level.idx, FUN = function(i){mlrrep(data[,i])})

    # check same colnames?, this could be re-written
    norm.data = matrix(NA, nrow(data), ncol(data))
    colnames(norm.data) = colnames(data)
    rownames(norm.data) = rownames(data)

    for(i in 1:length(level.idx)) norm.data[,level.idx[[i]]] = as.matrix(norm.list[[i]]$data_norm)
    data <- mlrrep(norm.data)
  }
  return(data)
}
