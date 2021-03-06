% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defunct_square_encode.R
\name{square_encode}
\alias{square_encode}
\title{Generate a rectangle code from longitudes and latitudes}
\usage{
square_encode(x, y, dx = 1, dy = 0.5, invalids = TRUE)
}
\arguments{
\item{x}{A vector of decimal degrees longitude.}

\item{y}{A vector or decimal degrees latitude.}

\item{dx}{Rectangle width in decimal degrees.}

\item{dy}{Rectangle height in decimal degrees.}

\item{invalids}{A boolean, if TRUE (default) returns NA's if
x outside range of -180 to 180 (longitudes) and if y outside
range of -90 to 90 (latitudes).}
}
\value{
A character vector
}
\description{
The function is primarily aimed at generation of user defined
arbritrary statistical rectangles. The longitudinal and latitudinal resolution
can be specified by the user, e.g. dx = 1 and dy = 0.5 is equivalent to the
ices statistical square resolution. The character code returned is of the form
lon:lat, the numerical specifying the central positions.
}

