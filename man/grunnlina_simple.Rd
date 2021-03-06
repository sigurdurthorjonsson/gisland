% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_log.R
\docType{data}
\name{grunnlina_simple}
\alias{grunnlina_simple}
\title{Grunnlína - simple}
\format{\code{SpatialPolygonsDataFrame}}
\usage{
grunnlina_simple
}
\description{
Línur sem eru lagðar til grundvallar á útreikningi á landhelgi (12 sjómílur) og
efnhagslögsögu Íslands sbr. 
 \href{http://www.althingi.is/altext/lagas/141b/1979041.html}{Lög um landhelgi, efnahagslögsögu og landgrunn}
}
\details{
Línur eru byggðar á grunnpunktum, sjá \code{\link{grunnpunktar}}.
}
\examples{
sp::plot(iceland, col="grey90")
sp::plot(grunnlina_simple, border="yellow", lwd=2, add=TRUE)
sp::plot(grunnpunktar, col="red", add=TRUE)

}
\author{
Einar Hjorleifsson <einar.hjorleifsson@gmail.com>
}
\keyword{datasets}

