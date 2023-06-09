#' Set openxlsx style for comparisons and ANOVA table
#'
#' @param wb Keep the same workbook to maintain the styles
#' @param data If more than sheet to be added, needs to be in for loop
#' @param FCcol Column index for Fold Change values
#' @param pvalcol Column index for pvalues
#' @param tabName Tab name, specify each time to avoid overwriting previous tabs
#' @param hicutoff Default 1.5
#' @param lowcutoff Default 0.67
#' @param pvalcutoff Default 0.05
#' @param protect Should this sheet be protected from editing by the user?
#' @param pokemonGO to colour the GO terms based on a random Pokemon. Like we're back in the craze of 2016.
#'
printxlsxDataSheet <- function(wb, data, FCcol, pvalcol, tabName="results", hicutoff=1.5, lowcutoff=0.67, pvalcutoff=0.05, protect=FALSE, pokemonGO=FALSE) {

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
    up.idx <- which(!is.na(data[, ratio]) & (data[, ratio] > hicutoff))
    if (length(up.idx) > 1)
      addStyle(wb, tabName, style=upReg, rows = 1 + up.idx, cols = ratio, gridExpand=T)

    down.idx <- which(!is.na(data[, ratio]) & (data[, ratio] < lowcutoff))
    if (length(down.idx) > 1)
      addStyle(wb, tabName, style=downReg, rows = 1 + down.idx, cols = ratio, gridExpand=T)
  }
  for (pval in pvalcol) {
    sig.idx <- which(!is.na(data[, pval]) & (data[, pval] < pvalcutoff))
    if (length(sig.idx) > 1)
      addStyle(wb, tabName, style=sigStyle, rows = 1 + sig.idx, cols = pval, gridExpand=T)
  }
  # Consistent with SimpleSheet
  addStyle(wb, sheet = tabName, style = header, rows = 1, cols = 1:ncol(data), gridExpand=T)
  addStyle(wb, sheet = tabName, style = firstCol, rows = 2:(nrow(data)+1), cols = 1)

  if ( protect == TRUE ){
    protectWorksheet(wb, sheet = tabName)
  }

  if ( pokemonGO == TRUE ){ # Easter egg
    pokemonidx <- sample.int(151, 1) # The original 151 are close to my heart
    pokemonpalette <- pokepal(pokemon = pokemonidx, spread = 6)         # Some pokemon have small colourspaces
    pokemonGOcolor <- createStyle(fgFill = pokemonpalette[sample.int(6, 1)])  # createStyle can only use one cell fg colour at a time.

    GO_col_idx <- grep("GO", data[1,], value = FALSE)
    addStyle(wb, sheet = tabName, style = pokemonGOcolor, rows = 1:nrow(data), cols = GO_col_idx, gridExpand=T)
  }

}
