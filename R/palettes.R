#' Colour gradient ramps
#'
#' The object contains functions produced by the \code{\link[grDevices:colorRamp]{colorRampPalette}} function.
#' 
# \cr
#' \code{showPal} can be used to display the available palettes. You can use \code{pal = "all"} or \code{pal=""} if you want to look at all the available palettes. 
#' You can also view single palettes individually. The following colour palettes are implemented:
#' \itemize{
#' \item \code{gradinv()}: inverse heatmap.
#' }
#' 
#' @return A function producing a colour gradient ramp.
#' @name ramps
#' @param n (\code{numeric}) Number of different colors to generate from the palette
#' 
NULL


#' inverse heatmap
#' @rdname ramps
#' 
#' @export 
gradinv <- grDevices::colorRampPalette(c("#33358a", "#76acce", "#fff99a",  "#e22c28", "#690720"))
