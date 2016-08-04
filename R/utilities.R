#' @title Returns the lon for a statistical square (tilkynningaskildureitur)
#'
#' @description A dplyr-friendly variant of the geo::r2d, returning only a
#' vector of longitudes. To be used inside mutate.
#'
#' @export
#'
#' @param x Numerical vector containing statistical square
r2lon <-  function(x) {
  -((x-floor(x/100)*100)%%50 + 0.5)
}

#' @title Returns the latitude for a statistical square (tilkynningaskildureitur)
#'
#' @description A dplyr-friendly variant of the geo::r2d, returning only a
#' vector of latitudes. To be used inside mutate.
#'
#' @export
#'
#' @param x Numerical vector containing statistical square
r2lat <- function(x) {
  lat <- floor(x/100)
  lon <- (x - lat * 100)%%50
  halfb <- (x - 100 * lat - lon)/100
  lat <- lat + 60 + halfb + 0.25
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
#' Can take some time (function currently based on a for-loop) if object large.
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
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
#' @param x A SpatialPolygon object
#' @param miles Distance in miles
#' @param quadsegs Number of line segments to use to approximate a quarter circle.
#' @examples
#' require(rgdal)
#' i <- iceland$flaki %in% "mainland"
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

# ------------------------------------------------------------------------------
# Need to clean up these next two functions
#
# Also add posibilities to tile area from x, y, dx and dy


#' Calculate area in square kilometers
#' 
#' A simpler wrapper around the \code{rgeos::gArea} for calculating area taking
#' care of the projection of the object.
#' 
#' @export
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @param x Spatial object
#' @param group_variable character vector specifying variable that separates
#' polygons
#' 
#' @examples
#' require(rgdal)
#' geo_area(iceland)
#' geo_area(eez)
#' geo_area(skipaflokkur3)
geo_area <- function(x, group_variable) {
  
  # note there are more sp class objects that may work
  if(class(x) %in% c("SpatialPolygonsDataFrame")) {
    if(sp::proj4string(x) == gisland::PRO@projargs) {
      return(rgeos::gArea(sp::spTransform(x,gisland::ISN93))/1e6)
    } else {
      if(sp::proj4string(x) == gisland::ISN93@projargs)
        return(rgeos::gArea(x)/1e6)
    }
  }
  
  if(class(x) %in% c("data.frame")) {
    
    return(geoarea2(x, group_variable = group_variable))
    
  }
  
}


geoarea2 <- function (data, Projection = "Lambert", group_variable, old.method = F, ngrdpts = 2000, 
          robust = T) 
{
  
  area <- 0
  
  if(missing(group_variable)) {
    data <- geo::geo.Split.poly(data)
  } else {
    # need to use group_variable
    data <- data  %>%  dplyr::group_by_(group_variable) %>% tidyr::nest()
    data <- data$data
  }
  if (old.method) {
    for (i in 1:length(data)) area <- area + geoarea.old(data[[i]], 
                                                         ngrdpts, robust)
  }
  else {
    area <- 0
    for (i in 1:length(data)) {
      if (Projection == "Lambert") 
        data[[i]] <- geo::lambert(data[[i]]$lat, data[[i]]$lon, 
                             mean(data[[i]]$lat), mean(data[[i]]$lon), mean(data[[i]]$lat))
      else data[[i]] <- geo::mercator(data[[i]]$lat, data[[i]]$lon, 
                                 b0 = mean(data[[i]]$lat))
      data[[i]] <- data.frame(x = data[[i]]$x, y = data[[i]]$y)
      n <- nrow(data[[i]])
      area <- area + abs(sum(data[[i]]$x[1:(n - 1)] * data[[i]]$y[2:n] - 
                               data[[i]]$x[2:n] * data[[i]]$y[1:(n - 1)], na.rm = T)/2)
    }
  }
  return(area)
} 

#' Calculate area of a tile given resolution
#'
#' @param x tile horizontal midpoint
#' @param y tile vertical midpoint
#' @param dx tile horizontal resulution
#' @param dy tile vertical resolution
#'
#' @return a numeric vector of length 1
#' @export
#'
#' @examples
# tile_area(-20, 0, 1, 0.5)
# tile_area(-20, 64, 1, 0.5)
# tile_area(-20, 85, 1, 0.5)
# df <- data_frame(x = seq(0, 20, by = 1),
#                  y = seq(65, 75, by = 0.5))
# df %>%
#  rowwise() %>%
#  dplyr::mutate(area = tile_area(x, y, dx = 1, dy = 0.5))
tile_area <- function(x, y, dx, dy) {
  
  dx <- abs(dx/2)  # half the distance
  dy <- abs(dy/2)  # ...
  d <- data.frame(lon = c(x - dx, x - dx, x + dx, x + dx, x - dx),
                  lat = c(y - dy, y + dy, y + dy, y - dy, y - dy))
  # calculate area
  d <- geo_area(d)
  
  # this does not work:
  # d <-
  #  d %>%
  #  rowwise()
  # d <- geo_area(d)
  
  return(d)
  
}
