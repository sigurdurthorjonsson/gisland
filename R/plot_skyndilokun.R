#' @title Plotta skyndilokun
#' 
#' @description XXX
#' 
#' @export
#' 
#' @param coodinates A dataframe containing lon and lat coordinates
#' @param r Extention of the bounding box. Default is 5

plot_skyndilokun <- function(d, r = 5) {
  
  d$lon <- -geo_convert(d$lon)
  d$lat <-  geo_convert(d$lat)
  
  x <- mean(d$lon) + r * (d$lon - mean(d$lon))
  y <- mean(d$lat) + r * (d$lat - mean(d$lat))
  
  my_box = c(left = min(x),
             bottom = min(y),
             right = max(x),
             top = max(y))
  
  
  m <- get_sjokort(bbox = my_box, zoom = 9)
  
  # close the box
  d <- rbind(d,d[1,])
  
  ggmap::ggmap(m) + 
    ggplot2::geom_polygon(data = d, ggplot2::aes(lon, lat),
                          colour = "red",
                          fill = "red",
                          alpha = 0.25) +
    ggplot2::labs(x = NULL, y = NULL)
}




