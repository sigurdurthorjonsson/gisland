#' Title
#'
#' @param from XXX
#' @param to XXX
#' @param step XXX
#' @param ... XXX
#'
#' @export

scale_longitude <- function(from=-180, to=180, step=1, ...) {
  x <- breaks(from, to, step)
  x$txt <- ifelse(x$Sign < 0, paste(x$txt,"W"), paste(x$txt,"E"))
  return(scale_x_continuous(breaks = x$value, labels = x$txt, expand = c(0, 0), ...))
}


#' Title
#'
#' @param ymin XXX
#' @param ymax XXX
#' @param step XXX
#' @param ... XXX
#'
#' @export
#'
scale_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  x <- breaks(from, to, step)
  x$txt <- ifelse(x$Sign < 0, paste(x$txt,"S"), paste(x$txt,"N"))
  return(scale_y_continuous(breaks = x$value, labels = x$txt, expand = c(0, 0), ...))
}

breaks <- function(from = -180, to = 180, step = 1) {
  x <- seq(from, to, step)
  Sign <- sign(x)
  dd <- trunc(x)
  j <- length(unique(dd)) != length(dd)
  dd <- abs(dd)
  txt <- paste(dd, "°", sep = "")
  mm <- x%%1
  mm <- round(mm * 60, 2)
  i <- mm > 0
  mm <- ifelse(mm > 9, mm, paste0("0",mm))
  j <- length(unique(dd)) != length(dd)
  if(any(i) | any(j)) txt <- paste(txt, mm, "'", sep = "")
  return(list(value = x,txt=txt, Sign=Sign))
}