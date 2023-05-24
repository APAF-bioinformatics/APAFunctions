#' Any normalisation of the data matrix applied to APAF data
#'
#' @param exp
#' @param data_raw The un-normalised raw ion data matrix, as a list
#' @param normalization comes from the --normalization flag and decfults to "total"
#' @param Group
#'
#' @return data_norm the normalised data. The type of normalisation depends on the experiment and the normalization environment variable set through --normalization
#'
normalize <- function(exp, data_raw, normalization, Group){
  ## KR - I don't think it make senses to me to do multiple normalisations using, e.g., --normalize="Total+IRS", but leaving for now.

  if((grepl("Total", normalization) == TRUE)) {
    tot = apply(data_raw, 2, FUN = function(v){sum(na.omit(v))})
    data <- sweep(data_raw, 2, tot/max(tot), "/")
  }


  if ((grepl("Median", normalization) == TRUE)) {
    tot = apply(data_raw, 2, function(x) median(na.omit(x)))
    data <- sweep(data_raw, 2, tot/max(tot), "/")
  }


  if((grepl("IRS", normalization) == TRUE)) {
    list.exp = list()
    for(i in 1:length(list.dat)) {
      list.exp[[i]]=data[,grep(paste0("R",i), colnames(data))]
    }

    # Calculate the protein sum for each batch
    list.rowsum = lapply(list.exp, function(x) apply(x,1, function(y) (sum(na.omit(y)))))
    irs = as.data.frame(list.rowsum[[1]])
    if(length(list.rowsum) > 1){
      for(i in 2:length(list.rowsum)) {
        irs = cbind(irs, as.data.frame(list.rowsum[[i]]))
      }
    }

    # convert 0 to NA
    # GS - TODO: future problem. Applying stats to missing data
    irs[irs==0] = NA
    colnames(irs) <- paste0("sum", 1:ncol(irs))
    rowsum.average <- apply(irs, 1, function(x) exp(mean(log(na.omit(x)))))

    # compute the scaling factor vectors
    irs.fac = sweep(irs, 1, rowsum.average, "/")
    list.irs.scaled = lapply(1:length(list.exp), function(x) sweep(list.exp[[x]], 1, irs.fac[,x], "/") )

    # make new data frame with normalized data
    data <- list.irs.scaled[[1]]
    if(length(list.irs.scaled) > 1){
      for(i in 2:length(list.irs.scaled)) {
        data = cbind(data, list.irs.scaled[[i]])
      }
    }

    # in case of 0 - GS: redundant if no 0s in dataset?
    data = data+1
  }


  if((grepl("MLR", normalization) == TRUE)) {

    Group = as.factor(Group)
    if (length(Group) != ncol(data_raw)) {stop("Matrix and group sizes don't match for mlr norm.")}

    level.idx = lapply(levels(Group), FUN=function(g){which(Group == g)})
    norm.list = lapply(level.idx, FUN = function(i){mlrrep(data_raw[,i])});

    # check same colnames?, this could be re-written
    norm.data = matrix(NA, nrow(data_raw), ncol(data_raw))
    colnames(norm.data) = colnames(data_raw)
    rownames(norm.data) = rownames(data_raw)

    for(i in 1:length(level.idx)) norm.data[,level.idx[[i]]] = as.matrix(norm.list[[i]]$data_norm)
    data <- mlrrep(norm.data)
  }

  if (exp == "SWATH") { # KR - not sure if this is still necessary
    data <- format(round(colSums(na.omit(data)), digits = 0), big.mark = ",")
  }
  data_norm <- data
  return(data_norm)
}
