#' @title Spatial location of datapoints
#' 
#' @description Returns spatial attributes of coordinates (longitude and latitude)
#' given spatial polygons.
#' 
#' @param lon A numerical vector
#' @param lat A numerical vector
#' @param map Normally a spatialPolygonDataFrame
#' @param variable The variable name, stored in the map attribute table to be
#' returned. If missing (default) only boolean vector is returned indicating
#' if coordinates are inside or outside any region.
#'
#' @export
#'
geo_inside <- function(lon, lat, map, variable) {
  
  # Here we could try to pass the stuff to geo_inside2
  # The problem is that it does not work if we have holes
  if(class(map) == "data.frame") {
    
    message("The map is a data.frame, tryp geo_inside2")
    return(NULL)
    
  }
  
  x <- sp::SpatialPoints(data.frame(long = lon, lat = lat))
  sp::proj4string(x) <- sp::proj4string(map)
  
  x <- sp::over(x, map)
  
  if(!missing(variable)) {
    if(is.factor(x[, variable])) {
      return(as.character(x[,variable]))
    } else {
      return(x[, variable])
    }
  } else {
    
    return(!is.na(x[,1]))
    
  }
}
