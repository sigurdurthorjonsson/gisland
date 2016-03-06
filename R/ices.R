#' @title Create a layer of ICES map backgrounds
#' 
#' @description TO DO: use some nice mapproj from the ggalt package
#' 
#' @param base A map dataframe containing long, lat and group
#' @param region map region
#' @param projection projection to use
#' @param fill fill colour
#' @param colour line colour
#' @param lwd line width
#' @param islands.fill fill colour for islands
#' @param ... other argument passed on to geom_polygon
#'
#' @export

icesmap <- function(base = ecoregion, region, projection = "ortho", fill = "white", 
                    colour = "grey50", lwd = 0.5, islands.fill = "grey90", ...) {
  
  if(!missing(region))  {
    
    
    # Shit mix for ices areas
    #   Not elegant but works
    if(base$Region[1] == "10_A_2") {
      
      pattern <- c("1_A", "1_B", "10_A_1", "10_A_2", "10_B", "12_A_1", "12_A_2",
                   "12_A_3", "12_A_4", "12_B", "12_C", "14_A", "14_B_1", "14_B_2",
                   "2_A_1", "2_A_2", "2_B_1","2_B_2","3_A","3_B_23",
                   "3_C_22","3_D_24","3_D_25","3_D_26","3_D_27","3_D_28_1",
                   "3_D_28_2","3_D_29","3_D_30","3_D_31","3_D_32","4_A","4_B",
                   "4_C","5_A_1","5_A_2","5_B_1_A","5_B_1_B","5_B_2","6_A",
                   "6_B_1","6_B_2","7_A","7_B","7_C_1","7_C_2","7_D","7_E",
                   "7_F","7_G","7_H","7_J_1","7_J_2","7_K_1","7_K_2","8_A",
                   "8_B","8_C","8_D_1","8_D_2","8_E_1","8_E_2","9_A","9_B_1","9_B_2")
      
      for(i in 1:length(region)) {
        if(nchar(region[i]) == 1) region[i] <- paste0(region[i],"_")
        x <- !is.na(stringr::str_match(region[i], stringr::str_sub(pattern,1,nchar(region[i]))))
        if(i == 1) {
          reg <- pattern[x]
        } else {
          reg <- c(reg, pattern[x])
        }
      }
      region <- reg
      
    }
    
  base <- base %>% dplyr::filter(Region %in% region)
  }
  
  
  hole <- base$hole == TRUE
  
  list(
    ggplot2::geom_polygon(data = base[base$hole == FALSE,],
                 ggplot2::aes_(~long, ~lat, group = ~group), 
                 fill = fill, colour = colour, lwd = lwd, ..., 
                 inherit.aes = FALSE, show.legend = FALSE),
    if(any(hole)) {
      ggplot2::geom_polygon(data = base[base$hole == TRUE,],
                   ggplot2::aes_(~long, ~lat, group = ~group), 
                   fill = islands.fill, colour = colour, lwd = lwd, ..., 
                   inherit.aes = FALSE, show.legend = FALSE)
      },
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.margin = grid::unit(0, "lines"),
                   axis.ticks.length = grid::unit(0, "cm"))#,
    #coord_map(coord)
  )
}


