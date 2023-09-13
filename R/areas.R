#' Calculate areas inside an arbitrarily defined unit
#' 
#' @param x A spatial object, either an sf, or sfc, or SpatRaster or RasterArray class object. 
#' @param lat Latitudinal extent. A numeric vector of two values.  
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
	function(x, lat=NULL,plot=FALSE,...){
			
		if(!is.null(lat)){
			# make sure that the values are ascending
			lat <- sort(lat)

			# get a simple matrix with  the latitude values
			window <- rgplates::mapedge(ymin=min(lat), ymax=max(lat))

			# turn off spherical geometry
			suppressMessages(sf::sf_use_s2(FALSE))
			suppressMessages(res <- st_intersection(st_union(x), window))

			# turn on spherical geometry
			suppressMessages(sf::sf_use_s2(TRUE))

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
