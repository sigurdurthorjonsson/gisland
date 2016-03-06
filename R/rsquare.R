#' @title An arbritrary rectangle from degrees longitudes and latitudes
#' 
#' @description Put some readable text here.
#' 
#' @param x A vector of decimal degrees longitude.
#' @param y A vector or decimal degrees latitude.
#' @param dx, dy Rectangle width and height in decimal degrees.
#' @param invalids A boolean, if TRUE (default) returns NA's if
#' x outside range of -180 to 180 (longitudes) and if y oustride
#' reange of -90 to 90 (latitudes).
#'
#' @return A character vector
#' @export
#' 
rsquare_encode <- function(x, y, dx = 1, dy = 0.5,
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
  return(paste(x, y, sep = ":"))
  
}