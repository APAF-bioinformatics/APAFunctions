#' Read mass spec input files and design files for analysis and writes outputs to global environment
#'
#' @param filename Path to data file
#' @param designfile Path to design file
#'
#' @return
#' @export
#'
#' @examples
readInputFiles <- function(filename, designfile){
  if (grepl(".xlsx", filename)) {
    experiment <<- "SWATH"
    ions <<- readWorkbook(filename, 1)
    proteins <<- readWorkbook(filename, 3)
    design <<- readWorkbook(designfile, 1)
    comps <<- readWorkbook(designfile, 2)
  } else if (grepl(".tsv", filename)) {
    experiment <<- "DIANN"
    ions <<- read.delim(filename, check.names = F)
    if (colnames(ions)[1] == "Genes"){
      diann_inf <<- "genes"
    } else if (colnames(ions)[1] == "Protein.Group") {
      diann_inf <<- "proteins"
    } else {stop("This may not be DIANN - please recheck")}
    design <<- readWorkbook(designfile, 1)
    comps <<- readWorkbook(designfile, 2)
  } else if (grepl(".zip", filename)) {
    experiment <<- "TMT"
    files <- unzip(filename, junkpaths=TRUE)
    if (length(files) < 1) { stop("The zip file is empty") }
    file.list <<- lapply(files, FUN=read.delim, as.is=T)
  }
}
