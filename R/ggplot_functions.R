#' Labels maps with degree and minutes
#' 
#' @description Labels maps with degree and minutes.
#'
#' @param from Minimum longitude label
#' @param to Maximum longitude label
#' @param by Breaks in decimal degrees
#' @param letter Flag which controls if lable should include letters, i.e. E or W. The
#' is set to FALSE
#' @param ... other arguments passed to scale_x_continuous
#'
#' @export

scale_longitude <- function(from=-180, to=180, by=1, letter = FALSE, ...) {
  x <- breaks(from, to, by)
  if(letter) x$txt <- ifelse(x$Sign < 0, paste0(x$txt,"W"), paste0(x$txt,"E"))
  return(ggplot2::scale_x_continuous(breaks = x$value, labels = x$txt, expand = c(0, 0), ...))
}


#' Labels maps with degree and minutes
#' 
#' @description Labels maps with degree and minutes.
#'
#' @param from Minimum latitude label
#' @param to Maximum latitude label
#' @param by Breaks in decimal degrees
#' @param letter Flag which controls if lable should include letters, i.e. N or S. The
#' is set to FALSE
#' @param ... other arguments passed to scale_x_continuous
#'
#' @export
scale_latitude <- function(from=-90, to=90, by=0.5, letter = FALSE, ...) {
  x <- breaks(from, to, by)
  if(letter) x$txt <- ifelse(x$Sign < 0, paste0(x$txt,"S"), paste0(x$txt,"N"))
  return(ggplot2::scale_y_continuous(breaks = x$value, labels = x$txt, expand = c(0, 0), ...))
}

breaks <- function(from = -180, to = 180, step = 1) {
  x <- seq(from, to, step)
  Sign <- sign(x)
  dd <- trunc(x)
  j <- length(unique(dd)) != length(dd)
  dd <- abs(dd)
  #sprintf("%X", as.integer(charToRaw("°")))
  txt <- paste(dd, "\u00B0", sep = "")
  mm <- x%%1
  mm <- round(mm * 60, 2)
  i <- mm > 0
  mm <- ifelse(mm > 9, mm, paste0("0",mm))
  j <- length(unique(dd)) != length(dd)
  if(any(i) & any(j)) txt <- paste(txt, mm, "'", sep = "")
  return(list(value = x,txt=txt, Sign=Sign))
}