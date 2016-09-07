# csquare functions
#   encode_csquare
#   csquare_triplet - helper function
#   csquare_area
#   decode_csquare
#   decode_csquare_lat - internal function, used by csquare_lat
#   csquare_lat
#   decode_csquare_lon - internal function, used by csquare_lon
#   csquare_lon


#' @title Encode coordinates to C-square
#'
#' @description C-square: A notation system of c-squares provides a compact
#' encoding of latitude and longitude coordinates into a machine- and
#' human-readable code. See https://en.wikipedia.org/wiki/C-squares
#'
#' @param lat A vector or decimal degrees latitude
#' @param lon A vector of cecimal degrees longitude
#' @param resolution A value specifying the returned resolution of C-squares:
#' 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 in degree units
#'
#' @return A character vector
encode_csquare <- function(lon, lat, resolution) {

  if(length(lon) != length(lon))
    stop("length of longitude not equal to length of latitude")

  if(!resolution %in% c(10,5,1,0.5,0.1,0.05,0.01))
    stop("resolution not in range: c(10,5,1,0.5,0.1,0.05,0.01)")

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
#' @export
#'
#' @param x A c-square
#' @param method Default ("geo") implemented
#'
csquare_area <- function(x, method = "geo") {

  # center point
  x <-
    dplyr::data_frame(sq = decode(x, type = "csquare")) %>%
    tidyr::separate(sq, c("lon", "lat"), sep = ":", convert = TRUE)
  # create a "polygon" for one csquare

  # calculate area
  if(method == "geo") {
    x <- data.frame(id = rep(1:nrow(x), each = 5),
                    lon = c(x$lon-0.025,x$lon-0.025,x$lon+0.025,x$lon+0.025,x$lon-0.025),
                    lat = c(x$lat-0.025,x$lat+0.025,x$lat+0.025,x$lat-0.025,x$lat-0.025))
    x <- geo::geoarea(x)
    return(x)
  }


  #if(method != "geo") {
  #  x <- sp::Polygon(x[,c("lon","lat")]) %>%
  #    list() %>%
  #    Polygons(ID = "1") %>%
  #    list() %>%
  #    SpatialPolygons(proj4string = PRO) %>%
  #    geo_area()
  #  return(x)
  #}
}


#' @title Calculate the C-square resolution
#'
#' @description Calculates the resolution of a C-square code
#'
#' @export
#'
#' @param csquare A string of csquare codes
#'
csquare_resolution <- function(csquare) {
  n <- nchar(csquare)  # length of character determines resolution
  r <- 10^(1 - floor((n-4)/4))  -
    ((round((n-4)/4,1) - floor((n-4)/4)) * 10^(1-floor((n-4)/4)))
  return(r)
}



#' @title Decode C-squares
#'
#' @description C-square: A notation system of c-squares provides a compact
#' encoding of latitude and longitude coordinates into a machine- and
#' human-readable code. See https://en.wikipedia.org/wiki/C-squares
#'
#' @return A data frame with longitude (lon) and latitudes (lat)
#'
#' @param x A string of csquare codes
#' @param resolution A value specifying the returned resolution of C-squares:
#' 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 in degree units
#' @param baf a value if default (0) no adjustment made. May only be of use for boundary values (-180/180 and -90/90).

decode_csquare <- function(x, resolution, baf = 0) {

  if(missing(resolution)) {
    resolution <- unique(csquare_resolution(x))
  }

  if(length(resolution) > 1) stop("more than one csquare resolution")

  if(!resolution %in% c(10,5,1,0.5,0.1,0.05,0.01)) stop("resolution not in range: c(10,5,1,0.5,0.1,0.05,0.01)")

  ### --------------------------------------------------------------------------
  # second trial

  # put in check here if .e.g. mismatch between res and resolution

  g1     = as.integer(substr(x,1,1))
  g1lat  = as.integer(substr(x,2,2))
  g1lon  = as.integer(substr(x,3,4))

  g2     = as.integer(substr(x,6,6))
  g2lat  = as.integer(substr(x,7,7))
  g2lon  = as.integer(substr(x,8,8))
  g2lat2 = round(g2*2,-1)/10
  g2lon2 = (round((g2-1)/2,1) - trunc((g2-1)/2)) * 2

  g3     = as.integer(substr(x,10,10))
  g3lat  = as.integer(substr(x,11,11))
  g3lon  = as.integer(substr(x,12,12))
  g3lat2 = round(g3*2,-1)/10
  g3lon2 = (round((g3-1)/2,1) - trunc((g3-1)/2)) * 2

  g4     = as.integer(substr(x,14,14))
  g4lat  = as.integer(substr(x,15,15))
  g4lon  = as.integer(substr(x,16,16))
  g4lat2 = round(g4*2,-1)/10
  g4lon2 = (round((g4-1)/2,1) - trunc((g4-1)/2)) * 2

  signY  = (round(abs(g1 - 4) * 2,-1)/5)-1
  signX  = ((2 * (round(g1,-1)/10)) - 1) * -1

    # central position
  if(resolution == 10) {
    lat1 = ((g1lat*10) + 5) * signY
    lon1 = ((g1lon*10) + 5) * signX
    return(paste(lon1, lat1, sep = ":"))
  }
  if(resolution == 5) {
    lat2 = ((g1lat*10) + (g2lat2 * 5) + 2.5) * signY
    lon2 = ((g1lon*10) + (g2lon2 * 5) + 2.5) * signX
    return(paste(lon2, lat2, sep = ":"))
  }
  if(resolution == 1) {
    lat3 = ((g1lat*10) + g2lat + 0.5) * signY
    lon3 = ((g1lon*10) + g2lon + 0.5) * signX
    return(paste(lon3, lat3, sep = ":"))
  }
  if(resolution == 0.5) {
    lat4 = ((g1lat*10) + g2lat + (g3lat2 * 0.5) + 0.25) * signY
    lon4 = ((g1lon*10) + g2lon + (g3lon2 * 0.5) + 0.25) * signX
    return(paste(lon4, lat4, sep = ":"))
  }
  if(resolution == 0.1) {
    lat5 = ((g1lat*10) + g2lat + (g3lat * 0.1) + 0.05) * signY
    lon5 = ((g1lon*10) + g2lon + (g3lon * 0.1) + 0.05) * signX
    return(paste(lon5, lat5, sep = ":"))
  }
  if(resolution == 0.05) {
    lat6 = ((g1lat*10) + g2lat + (g3lat * 0.1) + (g4lat2 * 0.05) + 0.025) * signY
    lon6 = ((g1lon*10) + g2lon + (g3lon * 0.1) + (g4lon2 * 0.05) + 0.025) * signX
    return(paste(lon6, lat6, sep = ":"))
  }
  if(resolution == 0.01) {
    lat7 = ((g1lat*10) + g2lat + (g3lat * 0.1) + (g4lat * 0.01) + 0.005)  * signY
    lon7 = ((g1lon*10) + g2lon + (g3lon * 0.1) + (g4lon * 0.01) + 0.005)  * signX
    return(paste(lon7, lat7, sep = ":"))
  }
}

# internal - used in csquare_lat
decode_csquare_lat <- function(x, resolution, baf = 0) {
  decode_csquare(x = x, resolution = resolution)$lat
}

#' Rounded latitude given csquare resolution
#'
#' @param lat Degrees latitude
#' @param lon Degrees longitude
#' @param resolution A value specifying the C-square resolution.
#' Valid values are 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 degree units
#'
#' @return A vector of rounded latitudes
#' @export
#'
csquare_lat <- function(lat, lon, resolution) {
  sq <- encode_csquare(lat = lat, lon = lon , resolution = resolution)
  lat <- decode_csquare_lat(sq, resolution = resolution)
  return(lat)
}

# internal - unsed in csquare_lon
decode_csquare_lon <- function(x, resolution, baf = 0) {
  decode_csquare(x = x, resolution = resolution)$lon
}

#' Rounded longitude given csquare resolution
#'
#' @param lat Degrees latitude
#' @param lon Degrees longitude
#' @param resolution A value specifying the C-square resolution.
#' Valid values are 10, 5, 1, 0.5, 0.1, 0.05 and 0.01 degree units
#'
#' @return A vector of rounded longitudes
#' @export
#'
csquare_lon <- function(lat, lon, resolution) {
  sq <- encode_csquare(lat = lat, lon = lon , resolution = resolution)
  lon <- decode_csquare_lon(sq, resolution = resolution)
  return(lon)
}
