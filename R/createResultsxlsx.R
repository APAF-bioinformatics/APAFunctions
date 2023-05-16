#' Creates an Excel workbook all results. Later it should format based upon experiment. TO DO: parameter values like normalization etc should be passed in
#'
#' @param res.list List containing every results dataframe
#' @param Anova.table Dataframe that contains the ANOVA results
#' @param design Experimental design file
#' @param comps Comparison between groups

createResultsxlsx <- function(res.list, Anova.table, design, comps){ #Currently no defaults, you must supply all. Can change to arg=arg defaults later
  wb <- openxlsx::createWorkbook("Results.xlsx")
  printxlsInfoSheet(wb, tabName = "Key")
  for (i in 1:length(res.list)) {
    printDataSheet(data = res.list[[i]], FCcol = grep("FC", names(res.list[[i]])), pvalcol = grep("pval", tolower(names(res.list[[i]]))), wb, tabName=comps[i,1])
  }
  printDataSheet(Anova.table, FCcol = grep("MaxFC", names(Anova.table)),pvalcol = grep("Anova", names(Anova.table))[1:2], wb, tabName="AllProteinsNorm")
  printxlsInfoSheet(wb, "Design", design)
  printxlsInfoSheet(wb, "Comparisons", comps)
  params <- data.frame(Parameters = c("Normalization", "Fold change cutoff", "P-value cutoff", 'FileName', 'DesignFile', 'Date'), Values = c(normalization, FCcutoff, pvalcutoff, basename(filename), basename(designfile), date()))
  printxlsInfoSheet(wb, "InputParameters", params)
  openxlsx::saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
  for (s in 1:length(getSheetNames("Results.xlsx"))) {
    setColWidths(wb, sheet = s, cols = 1:25, widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
}
