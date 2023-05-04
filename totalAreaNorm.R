#' normalise ion intensities by total area
#' NB: this isn't used anywhere else, maybe put back into SWATHANOVA.R 
#'
#' @param mat Matrix of ion intensities 
#'
totalAreaNorm <- function(mat) {
   tot = apply(mat, 2, FUN = function(v){sum(na.omit(v))})
   mat.norm = sweep(mat, 2, tot/max(tot), FUN="/")
   mat.norm
}