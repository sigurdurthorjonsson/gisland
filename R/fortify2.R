#' Title
#'
#' @param sp A spatial polygon or line dataframe
#'
#' @return dataframe
#'
#' @export
#'
tidy2 <- function(sp) {

  long <- NULL # Dummy

  if(!class(sp) %in% c("SpatialPolygonsDataFrame",
                        "SpatialLinesDataFrame")) {
    message("Object not of class SpatialPolygonsDataFrame or SpatialLinesDataFrame")
    stop()
  }
  d <- sp@data
  #names(d) <- stringr::str_to_title(names(d))
  d$id <- rownames(d)
  d <- broom::tidy(sp) %>%
    dplyr::inner_join(d, by = "id")
  return(d)
}
