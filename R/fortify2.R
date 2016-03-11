#' Title
#'
#' @param spdf A spatial polygon or line dataframe
#'
#' @return dataframe
#' @export
#'
fortify2 <- function(spdf) {
  
  long <- NULL # Dummy
  
  if(!class(spdf) %in% c("SpatialPolygonsDataFrame",
                        "SpatialLinesDataFrame")) {
    message("Object not of class SpatialPolygonsDataFrame or SpatialLinesDataFrame")
    stop()
  }
  d <- spdf@data
  names(d) <- stringr::str_to_title(names(d))
  d$id <- rownames(d)
  s <- ggplot2::fortify(spdf) %>% 
    dplyr::rename(lon = long) %>% 
    dplyr::left_join(d, by = "id")
  return(s)
}
