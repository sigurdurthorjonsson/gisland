# csquare functions
#   csquare_encode
#   csquare_triplet - helper function
#   csquare_area
#   csquare_decode


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



csquare_encode <- function(lat, lon, resolution) {
  
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


#' @title Calculate csquare area
#' 
#' @description Internal function as of now. Think there is a
#' function in DATRAS that does things better.
#'
#' @param x A c-square 
#' @param method Default ("geo") implemented
#'
csquare_area <- function(x, method = "geo") {
  
  #x <- "7601:131:141:1"
  
  # center point
  x <- x %>%
    csquares(inverse = T) %>%
    dplyr::select(x, lon6, lat6) %>%
    rename(csquare = x, lon = lon6, lat = lat6)
  # create a "polygon" for one csquare
  x <- data.frame(lon = c(x$lon-0.025,x$lon-0.025,x$lon+0.025,x$lon+0.025,x$lon-0.025),
                  lat = c(x$lat-0.025,x$lat+0.025,x$lat+0.025,x$lat-0.025,x$lat-0.025))
  # calculate area
  if(method == "geo") {
    x <- geo::geoarea(x)
    return(x)
  }
  
  
  if(method != "geo") {
    x <- Polygon(x[,c("lon","lat")]) %>%
      list() %>%
      Polygons(ID = "1") %>%
      list() %>%
      SpatialPolygons(proj4string = PRO) %>%
      geo_area()
    return(x)
  }
}

#' @title csquares
#'
#' @description XXX https://en.wikipedia.org/wiki/C-squares
#'
#' @param x a string of csquares
#' @param baf a value if default (0) no adjustment made. May only be of use for boundary values (-180/180 and -90/90).

csquares_decode <- function(x, baf = 0) {
  
  
  # brute force code - the may be more elegant ways
  # todo: no need to do via dplyr, just vectors
  
  if(inverse) {
    
    d <- data.frame(x = x, stringsAsFactors = FALSE)
    d <-
      d %>%
      mutate(n      = nchar(x),                  # length of character determines resolution
             r      = 10^(1 - trunc(n-4)/4)-((round((n-4)/4,1)-trunc((n-4)/4))*10^(1-trunc((n-4)/4))),
             g1     = as.integer(substr(x,1,1)),
             g1lat  = as.integer(substr(x,2,2)),
             g1lon  = as.integer(substr(x,3,4)),
             g2     = as.integer(substr(x,6,6)),
             g2lat  = as.integer(substr(x,7,7)),
             g2lon  = as.integer(substr(x,8,8)),
             g2lat2 = round(g2*2,-1)/10,
             g2lon2 = (round((g2-1)/2,1) - trunc((g2-1)/2)) * 2,
             g3     = as.integer(substr(x,10,10)),
             g3lat  = as.integer(substr(x,11,11)),
             g3lon  = as.integer(substr(x,12,12)),
             g3lat2 = round(g3*2,-1)/10,
             g3lon2 = (round((g3-1)/2,1) - trunc((g3-1)/2)) * 2,
             g4     = as.integer(substr(x,14,14)),
             g4lat  = as.integer(substr(x,15,15)),
             g4lon  = as.integer(substr(x,16,16)),
             g4lat2 = round(g4*2,-1)/10,
             g4lon2 = (round((g4-1)/2,1) - trunc((g4-1)/2)) * 2,
             signY  = (round(abs(g1 - 4) * 2,-1)/5)-1,
             signX  = ((2 * (round(g1,-1)/10)) - 1) * -1,
             # central position
             lat1 = ((g1lat*10) + 5) * signY,
             lon1 = ((g1lon*10) + 5) * signX,
             lat2 = ((g1lat*10) + (g2lat2 * 5) + 2.5) * signY,
             lon2 = ((g1lon*10) + (g2lon2 * 5) + 2.5) * signX,
             lat3 = ((g1lat*10) + g2lat + 0.5) * signY,
             lon3 = ((g1lon*10) + g2lon + 0.5) * signX,
             lat4 = ((g1lat*10) + g2lat + (g3lat2 * 0.5) + 0.25) * signY,
             lon4 = ((g1lon*10) + g2lon + (g3lon2 * 0.5) + 0.25) * signX,
             lat5 = ((g1lat*10) + g2lat + (g3lat * 0.1) + 0.05) * signY,
             lon5 = ((g1lon*10) + g2lat + (g3lon * 0.1) + 0.05) * signX,
             lat6 = ((g1lat*10) + g2lat + (g3lat * 0.1) + (g4lat2 * 0.05) + 0.025) * signY,
             lon6 = ((g1lon*10) + g2lon + (g3lon * 0.1) + (g4lon2 * 0.05) + 0.025) * signX,
             lat7 = ((g1lat*10) + g2lat + (g3lat * 0.1) + (g4lat * 0.01) + 0.005) * signY,
             lon7 = ((g1lon*10) + g2lon + (g3lon * 0.1) + (g4lon * 0.01) + 0.005) * signX)
    return(d)
  }
}


