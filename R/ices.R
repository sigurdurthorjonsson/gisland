#' ggplot ICES ecoregion shapfile 
#'
#' @description A function to generate a base map of ICES ecoregions
#' 
#' @param region Any (single or combination of) "Greenland Sea", "Bay of Biscay and the Iberian Coast",
#' "Azores, "Western Mediterranean Sea", "Ionian Sea and the Central Mediterranean Sea",
#' "Black Sea", "Adriatic Sea", "Aegean-Levantine Sea", "Celtic Seas", "Baltic Sea",
#' "Greater North Sea", "Arctic Ocean", "Iceland Sea", "Barents Sea", "Faroes",
#' "Norwegian Sea", "Oceanic Northeast Atlantic"
#' @param map Normally use ecoregion
#' 
#'
#' @export
#'
# map_ecoregion(c("Iceland Sea","Greenland Sea", "Faroes"))
map_ecoregion <- function(region, map) {
  
  egos <- c("Greenland Sea",
            "Bay of Biscay and the Iberian Coast",
            "Azores",
            "Western Mediterranean Sea",
            "Ionian Sea and the Central Mediterranean Sea",
            "Black Sea",
            "Adriatic Sea",
            "Aegean-Levantine Sea",
            "Celtic Seas",
            "Baltic Sea",
            "Greater North Sea",
            "Arctic Ocean",
            "Iceland Sea",
            "Barents Sea",
            "Faroes",
            "Norwegian Sea",
            "Oceanic Northeast Atlantic")
  if(missing(region)) {
    region <- egos
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = map %>% filter(hole == FALSE, ecoregion %in% region),
                          ggplot2::aes(long, lat, group = group, fill = id),  col = "grey90")
  
  hole <- map %>% dplyr::filter(hole == TRUE, ecoregion %in% region)
  
  if(nrow(hole) > 0) {
    p <- p +
      ggplot2::geom_polygon(data = hole,
                            ggplot2::aes(long, lat, group = group),
                            fill = "grey90")
  }
  p <- p +
    ggplot2::coord_map() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.margin = grid::unit(0, "lines"),
                   axis.ticks.length = grid::unit(0, "cm"))
  return(p)
}


