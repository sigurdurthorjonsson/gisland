#' @title Encode coordinates to tile codes
#'
#' @description xxx.
#'
#' @details xxx.
#'
#' @param x longitudinal decimal degrees
#' @param y latitudinal decimal degrees
#' @param resolution A numeric vector or length 1 or 2 or an character
#' vector ("large" or "small"). The meaning and usage depends on type - see
#' further in Details.
#' @param type Encoding type, any of "zchords" (default) or "csquare", "ices" or
#' "iceland".
#'
#' @return A character vector so same lenght as lon and lat
#'
#' @export
#'
encode <- function(x, y, resolution, type = "zchords") {

  if(missing(resolution)) resolution = c(1, 0.5)

  if(length(resolution) == 1 & is.numeric(resolution))
    resolution <- c(resolution, resolution)

  if(type == "zchords")
    return(encode_zchords(x, y,
                         dx = resolution[1],
                         dy = resolution[2]))

  if(type == "ices")
    return(encode_ices(x, y, resolution))

  if(type == "csquare")
    return(encode_csquare(x, y, resolution))

  if(type == "iceland")
    return(encode_iceland(x, y, resolution))



}





#' @title Encode coordinates to zchords
#'
#' @description The function is primarily aimed at generation of user defined
#' arbritrary statistical rectangles. The longitudinal and latitudinal resolution
#' can be specified by the user, e.g. dx = 1 and dy = 0.5 is equivalent to the
#' ices statistical square resolution. The character code returned is of the form
#' lon:lat, the numerical specifying the central positions.
#'
#' @details The tile encoding system is inspired by the code \code{geo::d2dr}
#' created by Sigurður Þór Jónsson. In acknowledgment to the code author and David,
#' this tile convention is referred to as zchords (Z(yggy's)-chords).
#' It has the advantages over the ICES statistical rectangle encoding and the
#' Icelandic native's "Tilkynningaskyldureitur" that it is global. In addition
#' it has the advantage over the csquare encoding in that the resolution
#' can be anything and is not limited by having equal decimal longitude and
#' latitude degree-resolution. The zchords is actually all inclusive,
#' encompassing all the above encoding system. As an added bonus the code
#' nomenclature is not chipered.
#'
#' @param x A vector of decimal degrees longitude.
#' @param y A vector or decimal degrees latitude.
#' @param dx Rectangle width in decimal degrees.
#' @param dy Rectangle height in decimal degrees
#' @param invalids A boolean, if TRUE (default) returns NA's if
#' x outside range of -180 to 180 (longitudes) and if y outside
#' range of -90 to 90 (latitudes).
#'
#' @return A character vector
#'
encode_zchords <- function(x, y, dx = 1, dy = 0.5,
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


#' Encode coordinates to ICES rectangle
#'
#' @param x lon
#' @param y lat
#' @param resolution If missing (default) or "large" returns the standard
#' ICES rectangle code. If "small" returns ICES subrectangle code.
#' @param useI Sometimes used
#'
#' @return A character vector
#'
#'
encode_ices <- function(x, y, resolution, useI = FALSE) {

  if(missing(resolution) | resolution == "large" | resolution == c(1, 0.5)) {

    y <- y + 1e-06
    x <- x + 1e-06

    outside <- y < 36 | y >= 85.5 | x <= -44 | x > 68.5
    if (any(outside))
      warning("Positions outside of ICES statistical area")
    y <- floor(y * 2) - 71
    y <- ifelse(y < 10, paste("0", y, sep = ""), y)
    if (useI)
      lettersUsed <- LETTERS[1:12]
    else lettersUsed <- LETTERS[c(1:8, 10:13)]
    x1 <- lettersUsed[(x + 60)%/%10]
    x2 <- ifelse(x1 == "A", floor(x%%4), floor(x%%10))
    ir <- paste(y, x1, x2, sep = "")
    ir[outside] <- NA

    return(ir)

  } else {

    stop("Not implemented yet")

  }

}

#' Encode coordinates to Icelandic rectangle
#'
#' @param x lon
#' @param y lat
#' @param resolution If missing (default) returns the standard (large)
#' rectangle. If "sr" returns subrectangle.
#'
#' @return A character vector
#'
encode_iceland <- function(x, y, resolution) {

  if(missing(resolution) | resolution == "large" | resolution == c(1, 0.5)) {

    y <- y + 1e-06
    x <- x - 1e-06
    x <- -x

    sq <- (floor(y) - 60) * 100 + floor(x)

    sq <- ifelse(y - floor(y) > 0.5, sq + 50, sq)

    return(sq)

  } else { # need a more proper test

    y <- y + 1e-06
    x <- x - 1e-06
    x <- -x
    r <- (floor(y) - 60) * 100 + floor(x)
    r <- ifelse(y - floor(y) > 0.5, r + 50, r)

    deg <- geo::r2d(r)

    x <- -x
    dy <- -(y - deg$lat)
    dx <- -(x - deg$lon)
    dl <- sign(dy + 1e-07) + 2 * sign(dx + 1e-07) + 4
    sr <- c(2, 0, 4, 0, 1, 0, 3)
    sr <- sr[dl]
    sr <- floor(r * 10 + sr)

    return(sr)

  }

}

