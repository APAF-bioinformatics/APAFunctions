#' TMT normalisation containing both SampleLoadNorm and IRS. To be merged eventually
#'
#' @param SampleLoadNorm comes from the --SampleLoadNorm flag and decfaults to "total"
#' @return data_irs. This used for all later ion data in TMT at the moment. It shouldn't be called data_irs. It should be data_processed
TMT_normalisation <- function(SampleLoadNorm){
  ### KR - to eventually be merged within and all-encompassing APAFunctions::normalize()
  # Next week: to contain function arguments


  #################################
  # Sample loading normalistation
  #################################

  # Sample loading median or total normalisation
  if(tolower(SampleLoadNorm) == "total") {
    tot = apply(data_raw, 2, function(x) sum(na.omit(x)))
  } else {
    tot = apply(data_raw, 2, function(x) median(na.omit(x)))
  }

  data_sl = sweep(data_raw, 2, tot/max(tot), "/")
  format(round(colSums(na.omit(data_sl)), digits = 0), big.mark = ",")


  #########
  # IRS     KR -- This should be a new addition to APAFunctions
  #########

  list.exp = list()
  for(i in 1:length(list.dat)) list.exp[[i]]=data_sl[,grep(paste0("R",i), colnames(data_sl))]

  # Calculate the protein sum for each batch
  list.rowsum = lapply(list.exp, function(x) apply(x,1, function(y) (sum(na.omit(y)))))

  irs = as.data.frame(list.rowsum[[1]])
  if(length(list.rowsum) > 1){
    for(i in 2:length(list.rowsum)) irs = cbind(irs, as.data.frame(list.rowsum[[i]]))}

  # convert 0 to NA
  # GS - TODO: future problem. Applying stats to missing data
  irs[irs==0] = NA
  colnames(irs) <- paste0("sum", 1:ncol(irs))
  rowsum.average <- apply(irs, 1, function(x) exp(mean(log(na.omit(x)))))

  # compute the scaling factor vectors
  irs.fac = sweep(irs, 1, rowsum.average, "/")
  list.irs.scaled = lapply(1:length(list.exp), function(x) sweep(list.exp[[x]], 1, irs.fac[,x], "/") )

  # make new data frame with normalized data
  data_irs <- list.irs.scaled[[1]]
  if(length(list.irs.scaled) > 1){
    for(i in 2:length(list.irs.scaled)) data_irs = cbind(data_irs, list.irs.scaled[[i]])}

  # in case of 0 - GS: redundant if no 0s in dataset?
  data_irs = data_irs+1
  format(round(colSums(na.omit(data_irs)), digits = 0), big.mark = ",")

  #########
  # END IRS
  ########
  return(list(data_sl=data_sl, data_irs=data_irs))
}
