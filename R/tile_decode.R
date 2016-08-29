#' Convert rectangle code to lon:lat code convention
#'
#' @param x Rectangle code vector
#' @param type An atomic character vector containng encoding type. Any of
#' "ices", "csquare" or "iceland".
#'
#' @return A character vector of lon:lat notational format
#'
#' @note Within the piped tidyverse approach the returned character vector
#' can be split using the \code{tidyr::separate} function.
#'
#' @export
#'
decode <- function(x, type) {

  # figure out type automatically
  if(missing(type)) {

    if(is.numeric(x)) return(decode_iceland(x))

    if(any(unique(stringr::str_sub(x, 3, 3)) %in% LETTERS[c(1:8, 10:13)]))
      return(decode_ices(x))

    return(decode_csquare(x))

  }

  if(type == "ices")    return(decode_ices(x))

  if(type == "csquare") return(decode_csquare(x))

  if(type == "iceland") return(decode_iceland(x))

}

#' @title Convert ICES rectangle to lon:lat
#'
#' @description Convert ICES rectangle code to rectangle center coordinates.
#' Returns a character vector where the coordinates are separated by ":".
#'
#' @note This is a modified version of the \code{geo:ir2d} function that
#' returns a single character vector rather than a \code{data.frame}.
#'
#' Within the piped tidyverse approach the returned character vector
#' can be split using the \code{tidyr::separate} function.
#'
#' @author Höskuldur Björnsson
#'
#' @references \href{http://ices.dk/marine-data/maps/Pages/ICES-statistical-rectangles.aspx}{ICES statistical rectangles}
#'
#' @param x ICES rectangle code, e.g. 37F3
#' @param useI A boolean, if FALSE (default) the letter I is not used.
#'
decode_ices <- function (x, useI = FALSE)  {

  # tests
  nc <- unique(nchar(x))
  if(length(nc) != 1) stop("rectangle code needs to be of unique length")

  if(!nc %in% c(4, 5)) stop("rectangle code should be either 4 or 5 characters")

  # ICES rectangle
  if(nc == 4) {
    lat <- substring(x, 1, 2)
    lat <- as.numeric(lat)
    lat <- (lat + 71)/2 + 0.25
    lon1 <- substring(x, 3, 3)
    lon1 <- toupper(lon1)
    lon1 <- match(lon1, LETTERS)
    if (!useI)
      lon1 <- ifelse(lon1 > 8, lon1 - 1, lon1)
    lon1 <- lon1 - 2
    lon2 <- substring(x, 4)
    lon2 <- as.numeric(lon2)
    lon <- ifelse(lon1 < 0, -44 + lon2 + 0.5, -40 + 10 * lon1 +
                    lon2 + 0.5)

    return(paste(lon, lat, sep = ":"))

  # ICES subrectangle (3 x 3)
  } else {

    stop("Not implemented yet")

  }

}

#' @title Convert Icelandic rectangles to lon:lat
#'
#' @description Convert Icelandic rectangle notation to rectangle center
#' coordinates notation
#'
#' Returns a character vector where the coordinates are separated by ":".
#'
#' @note This is a modified version of the \code{geo:r2d} function that
#' returns a single character vector rather than a \code{data.frame}.
#'
#' Within the piped tidyverse approach the returned character vector
#' can be split using the \code{tidyr::separate} function.
#'
#' @author Höskuldur Björnsson
#'
#' @param x Icelandic rectangle code, e.g. 522
#'
decode_iceland <- function(x) {

  # rectangle ("reitur")
  if(max(x) < 1000) {

    lat <- floor(x/100)
    lon <- (x - lat * 100)%%50
    halfb <- (x - 100 * lat - lon)/100
    lon <- -(lon + 0.5)
    lat <- lat + 60 + halfb + 0.25

    return(paste(lon, lat, sep = ":"))

    # subrectangle ("smareitur")
  } else {

    x2 <- floor(x/10)
    x <- x - x2 * 10
    lat <- floor(x2/100)
    lon <- (x2 - lat * 100)%%50
    halfb <- (x2 - 100 * lat - lon)/100
    lon <- -(lon + 0.5)
    lat <- lat + 60 + halfb + 0.25
    l1.lat <- c(0, 0.125, 0.125, -0.125, -0.125)
    l1.lon <- c(0, -0.25, 0.25, -0.25, 0.25)
    lat <- lat + l1.lat[x + 1]
    lon <- lon + l1.lon[x + 1]

    return(paste(lon, lat, sep = ":"))

  }
}
