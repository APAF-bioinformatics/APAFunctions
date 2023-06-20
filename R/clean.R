#' TO DO: make a clean function that can clean data based on its type. Blank for now
#' --clean argument should have a flag for the search engine used, e.g. MaxQuant or Protein Discoverer. Version numbers matter as well.
#'
#' @param experiment
#' @param data
#' @param filelist
#'
clean <- function(experiment=c("SWATH", "TMT"), data, filelist, searchengine, version) {


  # TO DO:
  # writing cleaning functions for:
  # ProteinDiscoverer v2, v2.2

  # if (experiment == "TMT"){
  #   file.list = lapply(file.list, function(x) {dat1 = x[!duplicated(x$Accession),]
  #   if(!'Master' %in% colnames(dat1) || !'Protein.FDR.Confidence' %in% colnames(dat1) )
  #     stop('Must have Master and Protein.FDR.Confidence columns');
  #   dat1 = dat1[dat1$Master == MasterFilter & dat1$Protein.FDR.Confidence == "High",];
  #   dat1  # The dat1 variable isn't used right now, it should be returned from clean in APAFunctions::clean()
  #   })
  # }

}

