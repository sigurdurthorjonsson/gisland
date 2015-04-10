#' Icelandic shorelines - mainland and islands
#'
#' The original data from Icelandic National Land Survey (Landmælingar
#' Íslands - LMÍ).  The original contains 6249 separate Polygons (island and scerries)
#' with a total of 266502 coordinate points.
#'
#' @format \code{SpatialPolygonsDataFrame}
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @source \url{http://www.lmi.is}
"biceland"


#' Icelandic shorelines - mainland and islands
#'
#' A thinned version of the original data from Icelandic National Land Survey (Landmælingar
#' Íslands - LMÍ).  The original contains 6249 separate Polygons (island and scerries)
#' with a total of 266502 coordinate points. This thinned version contains 508 separate Polygons (islands) with a total of
#' 75086 coordinate points.
#'
#' The alorithm used results in some x4 reduction in resolution without
#' much loss in quality, at least in overall fisheries context. The object is though
#' still x3 times as 'rich' as `geo::bisland` and includes the major islands.
#'
#' @format \code{SpatialPolygonsDataFrame}
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @source \url{http://www.lmi.is}
"iceland"
