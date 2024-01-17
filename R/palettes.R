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


#' Calibrated topographic color palettes
#'
#' The object contains \code{data.frame}-class objects.
#'
#' @format A \code{list} with 2 \code{data.frame}s:
#' \describe{
#' \item{\code{jakarta}: The "Jakarta" theme.}
#' \item{\code{havanna2}: The "Havanna-2" theme.}
#' \item{\code{tokio1}: The "tokio-1" theme.}
#' \item{\code{zagreb}: The "zagreb" theme.}
#' }
#' @usage data(topos)
#' 
"topos"


#' Create color ramp palette from fixed color objects
#'
#' Function to create ramp palettes from fixed color positions
#'
#' @param x A \code{data.frame} object with two columns: \code{color} for hexadecimal color values, \code{z} for their position.
#' @param n Single integer number
#'
#' @return A \code{list}-class object, with three elements: \code{$col} hexadecimal color values, \code{$mid} z-values of midpoints (one for every color, \code{$breaks} separator borders between color values.
#' @export
expandramp <- function(x, n){
	# separate data
	color <- x$color
	z <- x$z

	# generate sequence between minimum and maximum
	centers <- seq(min(z), max(z), length.out=n)
	# difference between the bins
	di <- diff(centers)[1]

	# the breaks (ready for return)
	breaks <- c(centers-di/2, centers[length(centers)]+di/2)

	# find the closest midpoint to every given z value
	col <- rep(NA, n)

	# where are the given color values
	index <- rep(NA, length(z))
	for(i in 1:length(z)){
		# differences
		absDiffs <- abs(z[i] - centers)
		index[i] <- which(min(absDiffs)==absDiffs)[1]
	}

	# construct the ramps
	for(i in 2:length(index)){
		# the indices where this should go
		fit <- index[i-1]:index[i]
		# generate ramp generator function
		thisChunkFunc <- grDevices::colorRampPalette(c(color[i-1], color[i]))
		# construct this part of the ramp
		colorSubset <- thisChunkFunc(length(fit))

		# fit this bit into the complete palette
		col[fit] <- colorSubset
	}
	
	y<- list(
		col=col,
		breaks=breaks,
		mid=centers
	)

	return(y)
}
