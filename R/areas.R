#' Calculate areas inside an arbitrarily defined unit
#' 
#' @param x A spatial object, either an sf, or sfc, or SpatRaster or RasterArray class object. 
#' @param lat Latitudinal extent. A numeric vector of two values.  
#' @param s2 Should S2 be turned off for the area calculations?  
#' @param ... additional arguments passed to class-specific methods.
#' @return An numeric area.
#' @exportMethod areas
#' 
#' @rdname areas
setGeneric("areas", function(x,...) standardGeneric("areas"))

#' @rdname areas
#' @export areas
setMethod(
	"areas",
	signature="sfc",
	function(x, lat=NULL,plot=FALSE,s2=FALSE, ...){
		
		# ensure S2 spherical geometry is turned off
		if(!s2){
			reset <- FALSE
			if(sf::sf_use_s2()) reset <- TRUE
			suppressMessages(sf::sf_use_s2(FALSE))
			if(reset) on.exit(suppressMessages(sf::sf_use_s2(TRUE)))
		}
			
		if(!is.null(lat)){
			# make sure that the values are ascending
			lat <- sort(lat)

			# get a simple matrix with  the latitude values
			window <- rgplates::mapedge(ymin=min(lat), ymax=max(lat))
			suppressMessages(res <- st_intersection(st_union(x), window))


			# if asked to plot, do it
			if(plot){
				plot(x)
				plot(window, add=TRUE, col="#00660055", border="#006600")
				plot(res, add=TRUE, col="#66000055", border="#660000")
			}
			# calculate the result
			sf::st_area(res)

		}

	} 
)

# examples
## x <- chronosphere::fetch("NaturalEarth")$geometry
## lat <- c(-30,90)
