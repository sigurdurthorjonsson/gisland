#' Get Sjókort
#'
#' \code{get_sjokort} accesses a tile server for an Icelandic Sea chart and
#' downloads/stitches map tiles/formats a map image. 
#' 
#' @export
#' 
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat,
#'   upperrightlon, upperrightlat).
#' @param r Extention of the bounding box. Default is 1
#' @param zoom a zoom level
#' @param maptype only one type implemented here "sjm"
#' @param crop crop raw map tiles to specified bounding box
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param color only "color" implemented
#' @param force if the map is on file, should a new map be looked up?
#' @param where where should the file drawer be located (without terminating "/")
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @seealso \code{\link{ggmap}}
#'
get_sjokort <- function(
  bbox = c(left = -30, bottom = 62.5, right = -10, top = 67.5),
  r = 1,
  zoom = 6,
  maptype = c("sjm","norwegian"),
  crop = TRUE,
  messaging = FALSE,
  urlonly = FALSE,
  color = c("color"),
  force = FALSE,
  where = tempdir(),
  ...)
  {
  
  # enumerate argument checking (added in lieu of checkargs function)
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)
  
  if("bbox" %in% argsgiven){
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      stop("bounding box improperly specified.  see ?get_openstreetmap", call. = F)
    }
  }
  
  if("zoom" %in% argsgiven){
    if(!(is.numeric(zoom) && length(zoom) == 1 &&
         zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
      stop("scale must be a postive integer 0-18, see ?get_stamenmap.", call. = F)
    }
  }
  
  if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))
  
  if("urlonly" %in% argsgiven) stopifnot(is.logical(urlonly))
  
  
  # color arg checked by match.arg
  
  if("checkargs" %in% argsgiven){
    .Deprecated(msg = "checkargs argument deprecated, args are always checked after v2.1.")
  }
  
  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])
  #if(checkargs) get_stamenmap_checkargs(args)
  maptype <- match.arg(maptype)
  
  maptype <- ifelse(maptype == "norwegian", "S300", "IS_25_26")
  
  color <- match.arg(color)
  
  if(is.null(names(bbox))) names(bbox) <- c("left","bottom","right","top")
  
  # set image type (stamen only)
  if(maptype %in% c("not implemented yet")){
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }
  
  x <- c(bbox[1],bbox[3])
  x <- mean(x) + r * (x - mean(x))
  y <- c(bbox[2],bbox[4])
  y <- mean(y) + r * (y - mean(y))
  
  bbox = c(left = min(x),
             bottom = min(y),
             right = max(x),
             top = max(y))
  
  # determine tiles to get
  fourCorners <- expand.grid(
    lon = c(bbox["left"], bbox["right"]),
    lat = c(bbox["bottom"], bbox["top"])
  )
  fourCorners$zoom <- zoom
  row.names(fourCorners) <- c("lowerleft","lowerright","upperleft","upperright")
  
  # The ggmap::LonLat2XY uses the google map tile reference system
  fourCornersTiles <- apply(fourCorners, 1, function(v) ggmap::LonLat2XY(v[1],v[2],v[3]))

  xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$X)))))
  numXTiles <- length(xsNeeded)
  ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$Y)))))
  numYTiles <- length(ysNeeded)
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if(nrow(tilesNeeded) > 40){
    message(paste0(nrow(tilesNeeded), " tiles needed, this may take a while ",
                   "(try a smaller zoom)."))
  }
  
  # convert from google tile reference system to TMS format
  tilesNeeded_google <- tilesNeeded
  tilesNeeded$y_tms = (2^zoom) - tilesNeeded$y - 1
  
  
  # make urls - e.g. http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
  
  base_url <- "http://www.hafro.is/~einarhj/tiles"
  base_url <- paste(base_url, maptype, zoom, sep = "/")
  urls <- paste(base_url,
                apply(tilesNeeded[,c("x","y_tms")], 1, paste, collapse = "/"), sep = "/")
  urls <- paste(urls, filetype, sep = ".")
  if(messaging) message(length(urls), " tiles required.")
  if(urlonly) return(urls)
  
  # check what tiles are available
  for(i in 1:length(urls)) {
    if(i == 1) {
      x <- RCurl::url.exists(urls[i])
    } else {
      x <- c(x,RCurl::url.exists(urls[i]))
    }
  }
  tilesNeededStored <- tilesNeeded
  tilesNeeded <- tilesNeeded[x,]

  
  listOfTiles <- lapply(split(tilesNeeded, 1:nrow(tilesNeeded)), function(v){
    v <- as.numeric(v)
    get_sjokort_tile(maptype, zoom, v[1], v[2], v[3], force = force, messaging = messaging)
  })
  
  
  # stitch tiles together
  map <- ggmap:::stitch(listOfTiles)
  
  
  # format map and return if not cropping
  if(!crop) {
    # additional map meta-data
    attr(map, "source")  <- "stamen"
    attr(map, "maptype") <- maptype
    attr(map, "zoom")    <- zoom
    
    # return
    return(map)
  }
  
  
  # crop map
  if(crop){
    mbbox <- attr(map, "bb")
    
    # A fix, if tiles do not exist
    xsNeeded <- sort(unique(tilesNeeded$x))
    ysNeeded <- sort(unique(tilesNeeded$y))
    
    size <- 256 * c(length(xsNeeded), length(ysNeeded))
    
    # slon is the sequence of lons corresponding to the pixels
    # left to right
    slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1])
    
    # slat is the sequence of lats corresponding to the pixels
    # bottom to top
    # slat is more complicated due to the mercator projection
    slat <- vector("double", length = 256*length(ysNeeded))
    for(k in seq_along(ysNeeded)){
      slat[(k-1)*256 + 1:256] <-
        sapply(as.list(0:255), function(y){
          ggmap::XY2LonLat(X = xsNeeded[1], Y = ysNeeded[k], zoom, x = 0, y = y)$lat
        })
    }
    slat <- rev(slat)
    ##slat <- seq(mbbox$ll.lat, mbbox$ur.lat, length.out = size[2])
    
    keep_x_ndcs <- which(bbox["left"] <= slon & slon <= bbox["right"])
    keep_y_ndcs <- sort( size[2] - which(bbox["bottom"] <= slat & slat <= bbox["top"]) )
    
    #keep_x_ndcs <- keep_x_ndcs[keep_x_ndcs <= ncol(map)]
    croppedmap <- map[keep_y_ndcs, keep_x_ndcs]
  }
  
  
  # format map
  croppedmap <- as.raster(croppedmap)
  class(croppedmap) <- c("ggmap","raster")
  attr(croppedmap, "bb") <- data.frame(
    ll.lat = bbox["bottom"], ll.lon = bbox["left"],
    ur.lat = bbox["top"], ur.lon = bbox["right"]
  )
  
  # additional map meta-data
  attr(croppedmap, "source")  <- "stamen"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom")    <- zoom
  
  # return
  croppedmap
}




get_sjokort_tile <- function(maptype, zoom, x, y, y_tms, force = FALSE, messaging, where = tempdir()){
  
  # check arguments
  is.wholenumber <-
    function (x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  
  stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
  stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
  stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
  
  # format url http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
  if(maptype %in% c("terrain","terrain-background","watercolor")){
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }
  
  url <- paste0(paste0(c("http://www.hafro.is/~einarhj/tiles", maptype, zoom, x, y_tms), collapse = "/"), ".", filetype)
  
  # lookup in archive
  tile <- ggmap:::file_drawer_get(url)
  if (!is.null(tile) && !force) return(tile)
  
  # grab if not in archive
  tmp <- tempfile()
  download.file(url, destfile = tmp, quiet = !messaging, mode = "wb")
  if(messaging) message(paste0("Map from URL : ", url))
  
  # read in
  #if(maptype %in% c("terrain","terrain-background","watercolor")){
  #  tile <- readJPEG(tmp)
  #} else {
  tile <- png::readPNG(tmp)
  #}
  
  
  # convert to colors
  # toner-lines treated differently for alpha
  if(maptype %in% c("toner-hybrid", "toner-labels", "toner-lines",
                    "terrain-labels", "terrain-lines")){
    tile <- t(apply(tile, 1:2, function(x) rgb(x[1], x[2], x[3], x[4])))
  } else {
    tile <- t(apply(tile, 2, rgb))
  }
  
  
  # determine bbox of map. note : not the same as the argument bounding box -
  # the map is only a covering of the bounding box extent the idea is to get
  # the lower left tile and the upper right tile and compute their bounding boxes
  # tiles are referenced by top left of tile, starting at 0,0
  # see http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  
  # Einar: Note that the XY2LonLat works on the google tile map system
  lonlat_upperleft <- ggmap::XY2LonLat(x, y, zoom)
  lonlat_lowerright <- ggmap::XY2LonLat(x, y, zoom, 255, 255)
  bbox <- c(
    left = lonlat_upperleft$lon,
    bottom = lonlat_lowerright$lat,
    right = lonlat_lowerright$lon,
    top = lonlat_upperleft$lat
  )
  bb <- data.frame(
    ll.lat = unname(bbox["bottom"]),
    ll.lon = unname(bbox["left"]),
    ur.lat = unname(bbox["top"]),
    ur.lon = unname(bbox["right"])
  )
  
  # format
  class(tile) <- c("ggmap", "raster")
  attr(tile, "bb") <- bb
  
  # store
  ggmap:::file_drawer_set(url, tile)
  
  # return
  tile
}
