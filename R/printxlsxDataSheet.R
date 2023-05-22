#' Set openxlsx style for comparisons and ANOVA table
#'
#' @param data If more than sheet to be added, needs to be in for loop
#' @param FCcol Column index for Fold Change values
#' @param pvalcol Column index for pvalues
#' @param wb Keep the same workbook to maintain the styles
#' @param tabName Tab name, specify each time to avoid overwriting previous tabs
#' @param hiCutoff Default 1.5
#' @param lowCutoff Default 0.67
#' @param pvalCutoff Default 0.05
#'
printxlsxDataSheet <- function(data, FCcol, pvalcol, wb, tabName = "results", hiCutoff = 1.5, lowCutoff=0.67, pvalCutoff=0.05) {
#TO DO: colour the ID column in grey and header row in blue like in SimpleSheet()
  addWorksheet(wb, sheet=tabName)
  header <- createStyle(fgFill = "lightblue")
  firstCol <- createStyle(fgFill = "gray90")

  upReg <- createStyle(fgFill = "tomato")
  downReg <- createStyle(fgFill = "seagreen3")
  sigStyle <- createStyle(fgFill = "lightgoldenrod1")

  # put data in the sheet
  writeData(wb, tabName, data, keepNA=FALSE)

  # style the sheet
  for (ratio in FCcol) {
    up.idx <- which(!is.na(data[, ratio]) & (data[, ratio] > hiCutoff))
    if (length(up.idx) > 1)
      addStyle(wb, tabName, style=upReg, rows = 1 + up.idx, cols = ratio, gridExpand=T)

    down.idx <- which(!is.na(data[, ratio]) & (data[, ratio] < lowCutoff))
    if (length(down.idx) > 1)
      addStyle(wb, tabName, style=downReg, rows = 1 + down.idx, cols = ratio, gridExpand=T)
  }
  for (pval in pvalcol) {
    sig.idx <- which(!is.na(data[, pval]) & (data[, pval] < pvalCutoff))
    if (length(sig.idx) > 1)
      addStyle(wb, tabName, style=sigStyle, rows = 1 + sig.idx, cols = pval, gridExpand=T)
  }
  # Consistent with SimpleSheet
  addStyle(wb, sheet = tabName, style = header, rows = 1, cols = 1:ncol(data), gridExpand=T)
  addStyle(wb, sheet = tabName, style = firstCol, rows = 2:(nrow(data)+1), cols = 1)
}
