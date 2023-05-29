#' Creates an Excel workbook all results. Later it should format based upon experiment. TO DO: parameter values like normalization etc should be passed in
#'
#' @param exp Either SWATH or TMT
#' @param inputlist A list of environment variables that containing info for Excel output tabs
#'
createResultsxlsx <- function(exp = c("SWATH, TMT"), inputlist){ #Currently no defaults, you must supply all.

  if (exp == "SWATH") {
    wb <- createWorkbook("Results.xlsx")
    printxlsxInfoSheet(wb, tabName = "Key")
    for (i in 1:length(inputlist[["res.list"]])) {
      res.list <- inputlist[["res.list"]]
      printxlsxDataSheet(wb, data=res.list[[i]], FCcol=grep("FC", names(res.list[[i]])), pvalcol=grep("pval", tolower(names(res.list[[i]]))), tabName=comps[i,1])
    }
    Anova.table <- inputlist[["Anova.table"]]
    printxlsxDataSheet(wb, Anova.table, FCcol = grep("MaxFC", names(Anova.table)), pvalcol = grep("Anova", names(Anova.table))[1:2], tabName="AllProteinsNorm")
    printxlsxInfoSheet(wb, "Design", inputlist[["design"]])
    printxlsxInfoSheet(wb, "Comparisons", inputlist[["comps"]])
    printxlsxInfoSheet(wb, "InputParameters", inputlist[["params"]])
    saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
    for (s in 1:length(getSheetNames("Results.xlsx"))) { setColWidths(wb, sheet = s, cols = 1:25, widths = "auto") }
    saveWorkbook(wb, file="Results.xlsx",  overwrite=T)
  }

  if (exp == "TMT") {
    full.res[full.res=='NaN'] = NA

    # TO DO: The rowname changes from Accession to Sample (with multiple types of naming). Decide on which to use
    wb <- createWorkbook("ReultsOverall.xlsx")
    ps <- try(printxlsxDataSheet(wb, data = full.res, FCcol=grep("MaxFC", names(full.res)), pvalcol=c(grep("Anova", names(full.res))), tabName = "AllData", hicutoff=FCcutoff, lowcutoff=1/FCcutoff, pvalcutoff))
    if(inherits(ps, 'try-error') ) warning('Error with print overall xlsx file')
    ps <- try(printxlsxInfoSheet(wb, tabName = "PCAScores", file = data.frame(rownames(pca.components$componentScores), pca.components$componentScores)))
    if(inherits(ps, 'try-error') ) warning('Error with print overall PCA component scores tab')
    writeData(wb, sheet = "PCAScores", "Sample", startCol = 1, startRow = 1) # Otherwise gives ugly col heading of rownames.pca.components.componentScores.
    ps <- try(printxlsxInfoSheet(wb, tabName = "PCALoadings", file = data.frame(rownames(pca.components$componentLoadings), pca.components$componentLoadings)))
    writeData(wb, sheet = "PCALoadings", "Accession", startCol = 1, startRow = 1) # Otherwise gives ugly col heading of rownames.pca.components.componentLoadings.
    if(inherits(ps, 'try-error') ) warning('Error with print overall PCA loading tab')
    ps <- try(printxlsxInfoSheet(wb, tabName = "SampleGroup", file = dat.samplegroup))
    if(inherits(ps, 'try-error') ) warning('Error with print sample group tab')
    printxlsxInfoSheet(wb, "Design", inputlist[["design"]])
    printxlsxInfoSheet(wb, "Comparisons", inputlist[["comps"]])
    printxlsxInfoSheet(wb, "InputParameters", inputlist[["params"]])
    saveWorkbook(wb, file="ResultsOverall.xlsx", overwrite=TRUE)
    for (s in 2:length(getSheetNames("ResultsOverall.xlsx"))) { setColWidths(wb, sheet = s, cols = 1:2, widths = "auto") }
    saveWorkbook(wb, file="ResultsOverall.xlsx", overwrite=TRUE)

    wb <- createWorkbook("ResultsTargeted2tail.xlsx")
    for(idx.comp in 1:length(tarcompres.list)) {
      dat.tmp = tarcompres.list[[idx.comp]][[1]]
      dat.tmp[dat.tmp=='NaN'] = NA
      colnames(dat.tmp)[1] = 'Accession'
      dat.tmp = dat.tmp[order(dat.tmp$Significant, decreasing=TRUE), ] # sort by significant
      printxlsxDataSheet(wb, data = dat.tmp, FCcol = grep('FC', names(tarcompres.list[[idx.comp]][[1]])),
                         pvalcol = grep('TwoSplTTest', names(tarcompres.list[[idx.comp]][[1]])),
                         tabName = tarcompres.list[[idx.comp]]$ComparisonName, hicutoff = FCcutoff,
                         lowcutoff = 1/FCcutoff, pvalcutoff)
    }

    addWorksheet(wb, sheet='images') # TMT images sheet to put volcano plots into
    startCol = 1
    for(idx.comp in 1:length(tarcompres.list)) { # KR - on 24/05/2023 plotVolcano does not generate these file names. TO DO: make sure it does
      if(file.exists(paste('Volcano plot for targeted', dat.comparisons[idx.comp,1], '.png', sep=''))) {
        insertImage(wb, sheet='images', file=paste('Volcano plot for targeted', dat.comparisons[idx.comp,1], '.png', sep=''), width=12, height=15, startRow=2, startCol=startCol, units='cm')
        startCol = startCol + 10
      }
    }
    saveWorkbook(wb, file="ResultsTargeted3tail.xlsx", overwrite=TRUE)
  }

}

