#' @title Generate a rectangle code from longitudes and latitudes
#' 
#' @description The function is primarily aimed at generation of user defined
#' arbritrary statistical rectangles. The longitudinal and latitudinal resolution
#' can be specified by the user, e.g. dx = 1 and dy = 0.5 is equivalent to the
#' ices statistical square resolution. The character code returned is of the form
#' lon:lat, the numerical specifying the central positions.
#' 
#' @param x A vector of decimal degrees longitude.
#' @param y A vector or decimal degrees latitude.
#' @param dx Rectangle width in decimal degrees.
#' @param dy Rectangle height in decimal degrees.
#' @param invalids A boolean, if TRUE (default) returns NA's if
#' x outside range of -180 to 180 (longitudes) and if y outside
#' range of -90 to 90 (latitudes).
#'
#' @return A character vector
#' @export
#' 
square_encode <- function(x, y, dx = 1, dy = 0.5,
                           invalids = TRUE) {
  
  x.brks <- seq(floor(min(x)),ceiling(max(x)),dx)
  x.ints <- findInterval(x, x.brks, all.inside = TRUE)
  x <- (x.brks[x.ints] + x.brks[x.ints + 1]) / 2
  
  y.brks <- seq(floor(min(y)),ceiling(max(y)),dy)
  y.ints <- findInterval(y, y.brks, all.inside = TRUE)
  y <- (y.brks[y.ints] + y.brks[y.ints + 1]) / 2
  
  if(invalids) {
   x <- ifelse(x >= -180 & x <= 180, x, NA)
   y <- ifelse(y >= -90  & y <= 90 , y, NA) 
  }
  
  return(paste(round(x,6), round(y,6), sep = ":"))
  
}