% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tile_decode.R
\name{decode_ices}
\alias{decode_ices}
\title{Convert ICES rectangle to lon:lat}
\usage{
decode_ices(x, useI = FALSE)
}
\arguments{
\item{x}{ICES rectangle code, e.g. 37F3}

\item{useI}{A boolean, if FALSE (default) the letter I is not used.}
}
\description{
Convert ICES rectangle code to rectangle center coordinates.
Returns a character vector where the coordinates are separated by ":".
}
\note{
This is a modified version of the \code{geo:ir2d} function that
returns a single character vector rather than a \code{data.frame}.

Within the piped tidyverse approach the returned character vector
can be split using the \code{tidyr::separate} function.
}
\author{
Höskuldur Björnsson
}
\references{
\href{http://ices.dk/marine-data/maps/Pages/ICES-statistical-rectangles.aspx}{ICES statistical rectangles}
}

