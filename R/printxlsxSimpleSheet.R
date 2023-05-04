#' Print xlsx simple page information key
#'
#' @param wb Keep the same workbook to maintain the styles
#' @param tabName One of 'Key', 'Design', 'Comparisons', or 'InputParameters'
#' @param file The file associated with the output for the tab. If tabName = 'Design', design. If tabName = 'Comparisons', comps. If tabName = 'InputParameters', params. Not required for 'Key'.
#'
printxlsxSimpleSheet <- function(wb, tabName, file) {
  #Alternative: source Columns and Explanations from a large Explanations.R file in APAFunctions, but here is also fine 
  
  addWorksheet(wb, sheet = tabName)
  header <- createStyle(fgFill = "lightblue")
  firstCol <- createStyle(fgFill = "gray90")
  
  if (tabName == "Key"){
    Columns = c("Uniprot/Protein fields", "Samples", "Group Averages", "MaxFC", "Anova", 
                "Anova.adj", "Anova.idx", "Number Peptides", "Order", "Gene ontology",
                "FCProteins", "PvalProteins", "FCPeptides", "PValPeptides", "CritProt", "CritPep")
    Explanations = c("Protein identifiers", "Individual sample quantitation", "Average quantitation for each group of samples identified", "Ratio of largest / lowest group average", "ANOVA p-value", "Anova p-value adjusted for multiple testing by Benjamini- Hochberg fdr adjustment", "TRUE if the protein is differentially expressed based on fold change and p-value cutoffs", 
                     "Number of peptides identified for the protein", "The order of SWATH extraction", "GO information from Uniprot - if available for the organism", "Fold change Group 1/Group 2 for each pairwise comparison from the Comparisons tab", 
                     "Unpaired t-test p-value for each pairwise comparison from the Comparisons tab",
                     "Fold change Group 1/Group 2 for each pairwise comparison from the Comparisons tab determined based on peptide level calculations",
                     "T-test p-value for each pairwise comparison from the Comparisons tab based on peptide level calculations",
                     "TRUE if differentially expressed based on protein-level cutoffs",
                     "TRUE if differentially expressed based on peptide-level cutoffs") 
    writeData(wb, sheet = tabName, x = data.frame(Columns, Explanations), keepNA = FALSE)
    addStyle(wb, sheet = tabName, style = header, rows = 1, cols = 1:2, gridExpand=T)
    addStyle(wb, sheet = tabName, style = firstCol, rows = 2:(length(Columns)+1), cols = 1)
  } else if (tabName == "Design" | tabName == "Comparisons" | tabName == "InputParameters"){
    writeData(wb, sheet = tabName, x = file, keepNA = FALSE)
    addStyle(wb, sheet = tabName, style = header, rows = 1, cols = 1:ncol(file), gridExpand=T)
    addStyle(wb, sheet = tabName, style = firstCol, rows = 2:(nrow(file)+1), cols = 1)
  } else print("Warning message here that Keiran can specify")  #TO DO: make this a a set of if else if states. 
}
