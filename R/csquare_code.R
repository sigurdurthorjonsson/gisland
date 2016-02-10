#' @title Returns a triplet C-square code
#' 
#' @description Internal function, used in \code{csquare}.
#'
#' @param lat A vector of the "remainder" of the lat degrees
#' @param lon A vector of the "remainder" of the lon degrees
#'
#' @return A triplet character vector of numbers
#'
#' @examples
code_triplet <- function(lat, lon) {
  digit1 <- (2*trunc(lat * 0.2)) + trunc(lon *0.2) + 1
  digit2 <- trunc(lat)
  digit3 <- trunc(lon)
  return(paste0(digit1,digit2,digit3))
}

#' @title Calculate the C-squares from degrees longitudes and latitudes
#' 
#' @description C-square: A notation system of c-squares provides a compact 
#' encoding of latitude and longitude coordinates into a machine- and 
#' human-readable code. See https://en.wikipedia.org/wiki/C-squares
#' 
#' @param lat A vector or decimal degrees latitude
#' @param lon A vector of cecimal degrees longitude
#' @param resolution A value specifying of the returned resolution of C-squares: 
#' 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 in degree units
#'
#' @return A character vector
#' @export
#'
#' @examples
# csquare(lat = c(62.457822, 66.991222), lon = c(-18.223499, -20.996655), resolution = 0.05)
#lat <-  62.457822
#lon <- -18.223499
#csquare(lat = lat, lon = lon, resolution = 0.01)



csquare <- function(lat, lon, resolution) {
  
  if(length(lon) != length(lon)) stop("length of longitude not equal to length of latitude")
  if(!resolution %in% c(10,5,1,0.5,0.1,0.05,0.01)) stop("degrees specified not in range: c(10,5,1,0.5,0.1,0.05,0.01)")
  
  lat.abs <- abs(lat)
  lon.abs <- abs(lon)
  
  # 10 degree square - first 4 characters
  g = 4-(((2*trunc(1 + (lon/200)))-1) * ((2 * trunc(1 + (lat/200)))+1))
  y = lat.abs%/%10 #trunc(lat.abs/10)
  x = lon.abs%/%10 # trunc(lon.abs/10)
  csquare <- paste0(g,y*10,x)
  if(resolution == 10) return(csquare)
  
  #  5 degree square
  csquare <- paste0(csquare,":",code_triplet(lat.abs%%10,lon.abs%%10))
  if(resolution == 5) {
    n = nchar(csquare) - 2
    return(substr(csquare,1,n))
  }
  #  1 degree square
  if(resolution == 1) return(csquare)

  #   0.5 degree square
  csquare <- paste0(csquare,":",code_triplet(lat.abs%%1 * 10,lon.abs%%1 * 10))
  if(resolution == 0.5) {
    n = nchar(csquare) - 2
    return(substr(csquare,1,n))
  }
  # 0.1 degree square
  if(resolution == 0.1) return(csquare)
  
  # 0.05 degree square
  csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.1 * 100,lon.abs%%0.1 * 100))
  if(resolution == 0.05) {
    n = nchar(csquare) - 2
    return(substr(csquare,1,n))
  }
  # 0.01 degree square
  if(resolution == 0.01) return(csquare)
  
  # ... on and on:
  #csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.01 * 1000,lon.abs%%0.01 * 1000))
  #csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.001 * 10000,lon.abs%%0.001 * 10000))
  #csquare <- paste0(csquare,":",code_triplet(lat.abs%%0.0001 * 100000,lon.abs%%0.0001 * 100000))
  #return(csquare)
}
