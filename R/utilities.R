#' Projection
#'
#' The conventional projection
#'
#' @format \code{sp::CRS} object
"PRO"

#' Projection
#'
#' Projection used by LMI
#'
#' @format \code{sp::CRS} object
"ISN93"

#' Converts degree-minutes-seconds to degree decimals
#'
#' This is the same function as \code{geo::geoconvert.1} , expect function does not
#' return a printout of "error > 60 min"
#'
#' @export
#'
#' @param x Vector of the form DDMMSS (degree-minutes-seconds)
geo_convert <- function (x)
{
  i <- sign(x)
  x <- abs(x)
  # x1 <- x%%10000
  # k <- c(1:length(x1))
  # k <- k[x1 > 5999 & !is.na(x1)]
  # if (length(k) > 0)
  #   print(paste("error > 60 min nr", k, x[k]))
  min <- (x/100) - trunc(x/10000) * 100
  return((i * (x + (200/3) * min))/10000)
}


#' degrees
#' 
#' Af function that adds the suffix degree to object. Used e.g.
#' in \code{pretty_coordinates}
#' 
#' @param x A value or text
degrees <- function(x) paste0(x,"\u00B0")


#' Pretty coordinates
#'
#' Creates pretty coordinates for printing.
#'
#' @export
#'
#' @param x A vector contain data of the format degree-minutes-second (ddmmss)
#' @param suffix A character, e.g. N, E, W, S
pretty_coordinates <- function(x,suffix="") {
  x <- paste0(degrees(stringr::str_sub(x,1,2)),stringr::str_sub(x,3,4),"'",stringr::str_sub(x,5,6),suffix,sep="")
  return(x)
}

#' Convert spatial object to data.frame that can be plotted in the geo-package
#'
#' Extracts coordinaes from each spatial object into a \code{data.frame} and
#' separates each with a row of NA's ("lifts the pen" effect).
#' 
#' Can take some time (function currently based on a for-loop)
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @export
#'
#' @return A \code{data.frame} containg columns lat and lon
#' 
#' @param x Typically a spatial object
#' @param hole A boolean (TRUE or FALSE). If not specified (default) then
#' everything is returned.
#' @examples
#' d <- sp_to_geo(landhelgi, hole=FALSE)
#' require(geo)
#' geoplot()
#' geolines(d,col="red")

sp_to_geo <- function(x, hole) {

  x <- ggplot2::fortify(x)
  cn <- c('lat','long')
  blank <- data.frame(lat=NA,long=NA)
  
  if(!missing(hole)) {
  x <- x[x$hole == hole,] 
  }
  
  
  groups <- unique(x$group)
  
  

  for (i in 1:length(groups)) {
    x1 <- x[x$group == groups[i],cn]
    if(i == 1) {
      d <- x1
    } else {
      d <- rbind(d,blank,x1)
    }
  }

  names(d) <- c('lat','lon')

  return(d)
}

#' @title Expand a spatial polygon by certain seamiles
#'
#' @description A simpler wrapper around the \code{rgeos::gBuffer} for objects
#' whose projection is in a non-coordinate format.
#'
#' @export
#'
#' @param x A SpatialPolygon object
#' @param miles Distance in miles
#' @param quadsegs Number of line segments to use to approximate a quarter circle.
#' @examples
#' require(rgdal)
#' i <- iceland$SHAPE_Area == max(iceland$SHAPE_Area)
#' three_miles_from_mainland <- expand_sp(iceland[i,], miles=3)
#' sp::plot(three_miles_from_mainland, col="red")
#' sp::plot(iceland, col="grey90", add=TRUE)
expand_sp <- function(x, miles, quadsegs = 20) {
  x  <- sp::spTransform(x,gisland::ISN93)
  x  <- rgeos::gBuffer(x,width =  miles * 1852,quadsegs=quadsegs)
  x  <- sp::spTransform(x,gisland::PRO)
  return(x)
}

#' @title Converts \code{data.frame} to SpatialPolygonsDataFrame
#'
#' @description Takes a data frame containing lon, lat, a grouping id (here
#' depth) and then an id for each segment within the group and returns a
#' Spatial Polygons Data Frame.
#'
#' @export
#'
#' @param df A data.frame containing lon, lat, group and segment.id
#' @param col.names A vector specifying the names of longitude, latitude, group and segment id

df_2_spdf <- function(df,col.names=c("lon","lat","group","id")) {

  if (any(is.na(match(col.names, names(df))))) {
    cat(paste("Columns", col.names, "do not exist"))
    return(invisible())
  }

  df <- df[,col.names]
  names(df) <- c("x","y","group","id")

  group.names <- unique(df$group)

  ncounter <- 1
  polygons.list=list()

  for (j in 1:length(group.names)) {

    d <- df[df$group == group.names[j],c("x","y","id")]
    ids <- unique(d$id)

    for (i in 1:length(ids)) {
      xy <- d[d$id == ids[i],c("x","y")]
      xy <- rbind(xy,xy[1,])
      xy <- sp::Polygons(list(sp::Polygon(xy)),ID=as.character(ncounter))
      polygons.list[[ncounter]] <- xy
      if(ncounter == 1) {
        df <- data.frame(ID=as.character(ncounter),
                         group=group.names[j])
      } else {
        df <- rbind(df,
                    data.frame(ID=as.character(ncounter),
                               group=group.names[j]))
      }
      ncounter <- ncounter + 1
    } # next segment
  } # next group
  d.sp <- sp::SpatialPolygons(polygons.list,proj4string=gisland::PRO)
  d.spdf <- sp::SpatialPolygonsDataFrame(d.sp, df, match.ID = TRUE)
  return(d.spdf)
}

#' Calculate area in square kilometers
#' 
#' XXXX
#' 
#' @export
#' 
#' @param x Spatial object
geo_area <- function(x) {
  return(rgeos::gArea(sp::spTransform(x,gisland::ISN93))/1e6)
}
