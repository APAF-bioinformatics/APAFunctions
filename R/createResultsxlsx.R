#' Creates an Excel workbook all results. Later it should format based upon experiment. TO DO: parameter values like normalization etc should be passed in
#'
#' @param exp Either SWATH or TMT
#' @param list A list of data frames containing data for Excel output tabs
#'
createResultsxlsx <- function(exp = c("SWATH, TMT"), inputlist){ #Currently no defaults, you must supply all.
  wb <- createWorkbook("Results.xlsx")

  if (exp == "SWATH"){
    printxlsInfoSheet(wb, tabName = "Key")
    for (i in 1:length(inputlist[["res.list"]])) {
      res.list <- inputlist[["res.list"]]
      printDataSheet(wb, data=res.list[[i]], FCcol=grep("FC", names(res.list[[i]])), pvalcol=grep("pval", tolower(names(res.list[[i]]))), tabName=comps[i,1])
    }
    Anova.table <- inputlist[["Anova.table"]]
    printDataSheet(wb, Anova.table, FCcol = grep("MaxFC", names(Anova.table)), pvalcol = grep("Anova", names(Anova.table))[1:2], tabName="AllProteinsNorm")
    printxlsInfoSheet(wb, "Design", inputlist[["design"]])
    printxlsInfoSheet(wb, "Comparisons", inputlist[["comps"]])
    printxlsInfoSheet(wb, "InputParameters", inputlist[["params"]])
    saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
    for (s in 1:length(getSheetNames("Results.xlsx"))) {
      setColWidths(wb, sheet = s, cols = 1:25, widths = "auto")
    }
    saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
  } else if (exp == "TMT") {
    full.res[full.res=='NaN'] = NA

    ps <- try(printxlsxDataSheet(wb, data = full.res, FCcol=grep("MaxFC", names(full.res)), pvalcol=c(grep("Anova", names(full.res))), tabName = "AllData", hiCutoff=FCCutoff, lowCutoff=1/FCCutoff, pvalcutoff))
    if(inherits(ps, 'try-error') ) warning('Error with print overall xlsx file')
    ps <- try(printxlsInfoSheet(wb, tabName = "PCAScores", file = data.frame(rownames(pca.components$componentScores), pca.components$componentScores)))
    if(inherits(ps, 'try-error') ) warning('Error with print overall PCA component scores tab')
    ps <- try(printxlsInfoSheet(wb, tabName = "PCALoadings", file = data.frame(rownames(pca.components$componentLoadings), pca.components$componentLoadings)))
    if(inherits(ps, 'try-error') ) warning('Error with print overall PCA loading tab')
    ps <- try(printxlsInfoSheet(wb, tabName = "SampleGroup", file = dat.samplegroup))
    if(inherits(ps, 'try-error') ) warning('Error with print sample group tab')
    printxlsInfoSheet(wb, "Design", inputlist[["design"]])
    printxlsInfoSheet(wb, "Comparisons", inputlist[["comps"]])
    printxlsInfoSheet(wb, "InputParameters", inputlist[["params"]])

    saveWorkbook(wb, file="ResultsOverall.xlsx", overwrite=TRUE)

    printxlsInfoSheet(wb, 'InputParameters', dat.para)

     for(idx.comp in 1:length(tarcompres.list)) {
      dat.tmp = tarcompres.list[[idx.comp]][[1]]
      dat.tmp[dat.tmp=='NaN'] = NA
      colnames(dd)[1] = 'Accession'
      # sort by significant
      dat.tmp = dat.tmp[order(dat.tmp$Significant, decreasing=TRUE), ]
      printxlsxDataSheet(wb, data = dat.tmp, FCcol = grep('FC', names(tarcompres.list[[idx.comp]][[1]])),
                         pvalcol = grep('TwoSplTTest', names(tarcompres.list[[idx.comp]][[1]])),
                         tabName = tarcompres.list[[idx.comp]]$ComparisonName, hiCutoff = FCCutoff,
                         lowCutoff = 1/FCCutoff, pvalcutoff)
    }

    addWorksheet(wb, sheet='images')
    startCol = 1
    for(idx.comp in 1:length(tarcompres.list)) {
      if(file.exists(paste('Volcano plot for targeted', dat.comparisons[idx.comp,1], '.png', sep=''))) {
        insertImage(wb, sheet='images', file=paste('Volcano plot for targeted', dat.comparisons[idx.comp,1], '.png', sep=''),
                    width=12, height=15, startRow=2, startCol=startCol, units='cm')
        startCol = startCol + 10
      }
    }
    printxlsInfoSheet(wb, tabName = 'Comparisons', dat.comparisons)
    saveWorkbook(wb, file="ResultsTargeted2tail.xlsx", overwrite=TRUE)
  }

  saveWorkbook(designSheets, file='Design.xlsx', overwrite=TRUE)
}
