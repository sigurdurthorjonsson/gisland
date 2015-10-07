# ---------------------------------------------------------
# Stuff stolen from the geo-package
#  Functions are amended to be more in flow with the 
#  dplyr syntax

#' Converts degree-minutes-seconds to degree decimals
#'
#' This is the same function as \code{geo::geoconvert.1} , except function does not
#' return a printout of "error > 60 min"
#'
#' @export
#'
#' @author Hoskuldur Bjornsson <hoski@@hafro.is>
#' 
#' @param x Vector of the form DDMMSS (degree-minutes-seconds)
#' @param inverse Which conversion should be undertaken, default from 
#' degrees-minutes-decimal minutes (DDMMmm) to decimal degrees (DD.dd)
geo_convert <- function (x, inverse = FALSE)
{
  if(!inverse) {
  i <- sign(x)
  x <- abs(x)
  # x1 <- x%%10000
  # k <- c(1:length(x1))
  # k <- k[x1 > 5999 & !is.na(x1)]
  # if (length(k) > 0)
  #   print(paste("error > 60 min nr", k, x[k]))
  min <- (x/100) - trunc(x/10000) * 100
  return((i * (x + (200/3) * min))/10000)
  } else {
    i <- sign(x)
    x <- abs(x)
    p1 <- floor(x)
    p2 <- floor((x - p1) * 60)
    p3 <- round((x - p1 - p2/60) * 100 * 60)
    return(i * (p1 * 10000 + p2 * 100 + p3))
  }
}



