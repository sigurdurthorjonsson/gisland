#' @title Get an ArcGIS map
#' 
#' @description get_arcgis get_arcgismap accesses an online tile server
#' (http://server.arcgisonline.com/arcgis/rest/services) and 
#' downloads/stitches map tiles/formats a map image. 
#' 
#' @export
#' 
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat,
#'   upperrightlon, upperrightlat).
#' @param r Extention of the bounding box. Default is 1 (Not implemented yet)
#' @param zoom a zoom level
#' @param maptype a character containing any of the following: "World_Imagery" 
#' (default), "Ocean_Basemap", "NatGeo_World_Map", "World_Physical_Map",
#' "World_Shaded_Relief", "World_Street_Map", "World_Terrain_Base" and
#' "World_Topo_Map".
#' @param crop crop raw map tiles to specified bounding box
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param color only "color" implemented
#' @param force if the map is on file, should a new map be looked up?
#' @param where where should the file drawer be located (without terminating "/")
#' @param gg Boolean, if TRUE (default) returns a ggmap object with stripped axes
#' information
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @seealso \code{\link{ggmap}}
#'
get_arcgismap <-
function (bbox = c(left = -30, bottom = 62.5, right = -10, top = 67.5),
          r = 1,
          zoom = 6,
          maptype = c("World_Imagery", "Ocean_Basemap", "NatGeo_World_Map",
                      "World_Physical_Map", "World_Shaded_Relief", 
                      "World_Street_Map", "World_Terrain_Base",
                      "World_Topo_Map"), 
          crop = TRUE, 
          messaging = FALSE,
          urlonly = FALSE,
          color = c("color"), 
          force = FALSE, 
          where = tempdir(), 
          gg = TRUE,
          ...) 
{
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)
  if ("bbox" %in% argsgiven) {
    if (!(is.numeric(bbox) && length(bbox) == 4)) {
      stop("bounding box improperly specified.  see ?get_openstreetmap", 
           call. = F)
    }
  }
  if ("zoom" %in% argsgiven) {
    if (!(is.numeric(zoom) && length(zoom) == 1 && zoom == 
          round(zoom) && zoom >= 0 && zoom <= 18)) {
      stop("scale must be a postive integer 0-18, see ?get_stamenmap.", 
           call. = F)
    }
  }
  if ("messaging" %in% argsgiven) 
    stopifnot(is.logical(messaging))
  if ("urlonly" %in% argsgiven) 
    stopifnot(is.logical(urlonly))
  if ("checkargs" %in% argsgiven) {
    .Deprecated(msg = "checkargs argument deprecated, args are always checked after v2.1.")
  }
  maptype <- match.arg(maptype)
  color <- match.arg(color)
  if (is.null(names(bbox))) 
    names(bbox) <- c("left", "bottom", "right", "top")
  if (maptype %in% c("terrain", "terrain-background", "watercolor")) {
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }
  fourCorners <- expand.grid(lon = c(bbox["left"], bbox["right"]), 
                             lat = c(bbox["bottom"], bbox["top"]))
  fourCorners$zoom <- zoom
  row.names(fourCorners) <- c("lowerleft", "lowerright", "upperleft", 
                              "upperright")
  # The ggmap::LonLat2XY uses the google map tile reference system
  fourCornersTiles <- apply(fourCorners, 1, function(v) ggmap::LonLat2XY(v[1], 
                                                                  v[2], v[3]))
  xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
                                                        function(df) df$X)))))
  numXTiles <- length(xsNeeded)
  ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, 
                                                        function(df) df$Y)))))
  numYTiles <- length(ysNeeded)
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if (nrow(tilesNeeded) > 40) {
    message(paste0(nrow(tilesNeeded), " tiles needed, this may take a while ", 
                   "(try a smaller zoom)."))
  }
  
  # convert from google tile reference system to TMS format
  tilesNeeded_google <- tilesNeeded
  tilesNeeded$y_tms = (2^zoom) - tilesNeeded$y - 1
  
  #base_url <- "http://tile.stamen.com/"
  base_url <- "http://server.arcgisonline.com/ArcGIS/rest/services/"
  base_url <- paste(base_url, maptype, "/MapServer/tile/", zoom, sep = "")
  
  urls <- paste(base_url, apply(tilesNeeded[,c("y","x")], 1, paste, collapse = "/"), 
                sep = "/")
  urls <- paste(urls, filetype, sep = ".")
  if (messaging) 
    message(length(urls), " tiles required.")
  if (urlonly) 
    return(urls)
  count <- 0
  nTiles <- nrow(tilesNeeded)
  listOfTiles <- lapply(split(tilesNeeded[,c("x","y")], 1:nrow(tilesNeeded)), 
                        function(v) {
                          v <- as.numeric(v)
                          #get_stamenmap_tile(maptype, zoom, v[1], v[2], force = force,
                          get_arcgis_tile(maptype, zoom, v[1], v[2], force = force,
                                             messaging = messaging)
                        })
  map <- ggmap:::stitch(listOfTiles)
  if (!crop) {
    attr(map, "source") <- "arcgis"
    attr(map, "maptype") <- maptype
    attr(map, "zoom") <- zoom
    if(gg) {
      map <- ggmap::ggmap(map) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.margin = grid::unit(0, "lines"),
            axis.ticks.length = grid::unit(0, "cm"))
    }
    return(map)
  }
  if (crop) {
    mbbox <- attr(map, "bb")
    size <- 256 * c(length(xsNeeded), length(ysNeeded))
    slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1])
    slat <- vector("double", length = 256 * length(ysNeeded))
    for (k in seq_along(ysNeeded)) {
      slat[(k - 1) * 256 + 1:256] <- sapply(as.list(0:255), 
                                            function(y) {
                                              ggmap::XY2LonLat(X = xsNeeded[1], Y = ysNeeded[k], 
                                                        zoom, x = 0, y = y)$lat
                                            })
    }
    slat <- rev(slat)
    keep_x_ndcs <- which(bbox["left"] <= slon & slon <= bbox["right"])
    keep_y_ndcs <- sort(size[2] - which(bbox["bottom"] <= 
                                          slat & slat <= bbox["top"]))
    croppedmap <- map[keep_y_ndcs, keep_x_ndcs]
  }
  croppedmap <- as.raster(croppedmap)
  class(croppedmap) <- c("ggmap", "raster")
  attr(croppedmap, "bb") <- data.frame(ll.lat = bbox["bottom"], 
                                       ll.lon = bbox["left"], ur.lat = bbox["top"], ur.lon = bbox["right"])
  attr(croppedmap, "source") <- "arcgis"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom") <- zoom
  if(gg) {
    croppedmap <- ggmap::ggmap(croppedmap) +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.margin = grid::unit(0, "lines"),
                     axis.ticks.length = grid::unit(0, "cm"))
  }
  croppedmap
}


get_arcgis_tile <- 
function (maptype, zoom, x, y, force = FALSE, messaging = TRUE, 
          where = tempdir()) 
{
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
                                                                     round(x)) < tol
  stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
  stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
  stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
  if (maptype %in% c("terrain", "terrain-background", "watercolor")) {
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }
  #base_url <- paste(base_url, maptype, "/MapServer/tile/", zoom, sep = "")
  url <- paste0(paste0(c("http://server.arcgisonline.com/ArcGIS/rest/services", maptype, 
                         "MapServer/tile", zoom, y, x), collapse = "/"), ".", filetype)
  tile <- ggmap:::file_drawer_get(url)
  if (!is.null(tile) && !force) 
    return(tile)
  tmp <- tempfile()
  download.file(url, destfile = tmp, quiet = !messaging, mode = "wb")
  if (messaging) 
    message(paste0("Map from URL : ", url))
  #if (maptype %in% c("World_Imagery")) {
    tile <- jpeg::readJPEG(tmp)
  #} else {
  #  tile <- png::readPNG(tmp)
  #}
  if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
                     "terrain-labels", "terrain-lines")) {
    tile <- t(apply(tile, 1:2, function(x) rgb(x[1], x[2], 
                                               x[3], x[4])))
  } else {
    tile <- t(apply(tile, 2, rgb))
  }
  lonlat_upperleft <- ggmap::XY2LonLat(x, y, zoom)
  lonlat_lowerright <- ggmap::XY2LonLat(x, y, zoom, 255, 255)
  bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
            right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
  bb <- data.frame(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
                   ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
  class(tile) <- c("ggmap", "raster")
  attr(tile, "bb") <- bb
  ggmap:::file_drawer_set(url, tile)
  tile
}