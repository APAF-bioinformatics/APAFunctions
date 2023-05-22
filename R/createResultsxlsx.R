#' Creates an Excel workbook all results. Later it should format based upon experiment. TO DO: parameter values like normalization etc should be passed in
#'
#' @param exp Either SWATH or TMT
#' @param list A list of data frames containing data for excel output tabs
#'
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

    # wb <- createWorkbook()
    # full.res[full.res=='NaN'] = NA
    #
    # # add parameter cutoffs
    # dat.para <- data.frame(Parameter=c('P value', 'Fold change', 'Clean', 'FileName', 'DesignFile', 'Date'),
    #                        Cutoff=c(pvalcutoff, FCCutoff, as.character(Clean), basename(filename),
    #                                 basename(designfile), date()))
    # APAFunctions::printxlsInfoSheet(wb, 'InputParameters', dat.para)
    #
    # ps <- try(printxlsxDataSheet(data = full.res, FCcol=grep("MaxFC", names(full.res)),
    #                              pvalcol=c(grep("Anova", names(full.res))),
    #                              wb = wb,
    #                              tabName = "AllData", hiCutoff=FCCutoff, lowCutoff=1/FCCutoff, pvalCutoff=pvalcutoff) )
    #
    # if(inherits(ps, 'try-error') ) warning('Error with print overall xlsx file')
    #
    # ps <- try(printxlsInfoSheet(wb, tabName = "PCAScores", file = data.frame(rownames(pca.res$componentScores),pca.res$componentScores)))
    #
    # if(inherits(ps, 'try-error') ) warning('Error with print overall PCA component scores tab')
    #
    # ps <- try(printxlsInfoSheet(wb, tabName = "PCALoadings", file = data.frame(rownames(ld), ld)))
    #
    # if(inherits(ps, 'try-error') ) warning('Error with print overall PCA loading tab')
    #
    # ps <- try(printxlsInfoSheet(wb, tabName = "SampleGroup", file = dat.samplegroup))
    #
    # if(inherits(ps, 'try-error') ) warning('Error with print sample group tab')
    #
    # saveWorkbook(wb, file="ResultsOverall.xlsx", overwrite=TRUE)
  }
}
