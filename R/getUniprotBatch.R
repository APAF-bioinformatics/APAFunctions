#' Get UniProt annotations
#'
#' @param values IDList, ideally as sp|UniprotID|organism
#' @param attributes UniProt information to obtain. Default obtains accession, protein name and GO ID
#'
getUniprotBatch = function(values=IDList, attributes=c("accession", "protein_name", "go_id")) {
  if (grepl("refGene", IDList[1]) == TRUE){

    ID_edit <- paste0(sapply(strsplit(IDList, split = "[_]"), function(x) x[4]), "_",
                      sapply(strsplit(IDList, split = "[_]"), function(x) x[5]))
    res_final <- setNames(as.data.frame(matrix(ncol = 4)),
                          c('accession', 'protein_name', 'go_id', 'query'))

    for (i in 1:length(ID_edit)) {
      query = paste("http://rest.uniprot.org/uniprotkb/search?query=", ID_edit[i],
                    "&fields=", paste(attributes, collapse=","), "&format=tsv", sep="")
      res <- data.table::fread(text = getURI(query), header = T, encoding = "UTF-8")
      res <- as.data.frame(res)
      if (nrow(res) == 0) {
        print(paste("No UniProt entry for", ID_edit[i]))
        res_final[i,] <- c(NA, NA, NA, IDList[i])
      } else {
        # query - taking hit with most GO terms
        res <- res[which.max(nchar(res$`Gene Ontology IDs`)),] # GO terms always same length, choose most information. TO DO: warn user to double-check what they want
        res_final[i,] <- c(as.character(res), as.character(IDList[i]))
      }
    }
  } else {   #This if/else could be a switch() if there were more cases than 'RefGene (NCBI) or NOT RefGene (UniProt)'
    ID_edit <- sapply(strsplit(IDList, split = "[|]"), function(x) x[2])
    ID_edit_l <- split(ID_edit, ceiling(seq_along(ID_edit)/500)) # Uniprot API accepts 500 as a request limit for now

    res_final <- data.frame()
    for (i in 1:length(ID_edit_l)) {
      query = paste("http://rest.uniprot.org/uniprotkb/search?query=",
                    paste(paste("accession_id:", ID_edit_l[[i]], sep=""), collapse="+OR+"),
                    "&fields=", paste(attributes, collapse=","), "&format=tsv&size=500", sep="")
      res <- data.table::fread(text = getURI(query), header = T, encoding = "UTF-8")

      # If UniProt has decided that an ID represent the same protein as another, or is two proteins, the API will just return '(de)merged' with no further details.
      # Start a new query and find new entry, remerge with res_final
      if (any(grep("merged", res$`Protein names`))){ #grep will pick up demerged as well, and idmapping will handle it
        ids <- res$Entry[grep("merged", res$`Protein names`)]
        # Set the URL and parameters for the POST request
        url <- "http://rest.uniprot.org/idmapping/run"
        params <- list(
          from = "UniProtKB_AC-ID",
          to = "UniProtKB",
          ids = ids)  #AC-ID -> UniProt will handle re-directs that the Uniprot websites provides: https://www.uniprot.org/help/difference_accession_entryname

        # Send a POST request to the server
        response <- postForm(url, .params = params)
        jobId <- stringr::str_match(response, "\"jobId\":\"([^\"]*)\"")[, 2]
        url <- paste0("https://rest.uniprot.org/idmapping/uniprotkb/results/stream/", jobId)
        Sys.sleep(5) # TO DO: This sleep time is arbitrary, there is a way to check if the request is done https://www.uniprot.org/help/id_mapping [Fetching details about a job]
        out <- fromJSON(url)
        acc <- out$results$to$primaryAccession
      }
      res_final <- rbind(res_final, res)
    }
  }
  data.frame(res_final)
}
