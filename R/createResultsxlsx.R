#' Creates an Excel workbook all results. Later it should format based upon experiment. TO DO: parameter values like normalization etc should be passed in
#'
#' @param exp Either SWATH or TMT
#' @param list A list of data frames containing data for excel output tabs
createResultsxlsx <- function(exp = c("SWATH, TMT"), list = inputlist){ #Currently no defaults, you must supply all. Can change to arg=arg defaults later
  if (exp == "SWATH"){
    wb <- openxlsx::createWorkbook("Results.xlsx")
    printxlsInfoSheet(wb, tabName = "Key")
    for (i in 1:length(inputlist[["res.list"]])) {
      res.list <- inputlist[["res.list"]]
      printDataSheet(data = res.list[[i]], FCcol = grep("FC", names(res.list[[i]])), pvalcol = grep("pval", tolower(names(res.list[[i]]))), wb, tabName=comps[i,1])
    }
    Anova.table <- inputlist[["Anova.table"]]
    printDataSheet(Anova.table, FCcol = grep("MaxFC", names(Anova.table)),pvalcol = grep("Anova", names(Anova.table))[1:2], wb, tabName="AllProteinsNorm")
    printxlsInfoSheet(wb, "Design", inputlist[["design"]])
    printxlsInfoSheet(wb, "Comparisons", inputlist[["comps"]])
    printxlsInfoSheet(wb, "InputParameters", inputlist[["params"]])
    openxlsx::saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
    for (s in 1:length(getSheetNames("Results.xlsx"))) {
      setColWidths(wb, sheet = s, cols = 1:25, widths = "auto")
    }
    openxlsx::saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
  } else if (exp == "TMT") {
    print("Nothing quite yet")
  }
}
