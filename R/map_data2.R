map_data2 <- function(map = "world", region = c("North Sea")) {
  
  if(region == "North Sea") {
    reg <- c("Belgium", "Denmark", "France", "Germany", "Luxembourg",
             "Netherlands", "Norway", "Poland", "Sweden", "UK")
    d <- ggplot2::map_data(map, reg) %>% 
      dplyr::filter(!subregion %in% c("Jan Mayen", "Svalbard", "Corsica",
                                      "Isle of Wight", "Northern Ireland"))
    return(d)
  }
  
  if(region == "Irish Sea") {
    reg <- c("Ireland", "UK")
    d <- ggplot2::map_data(map, reg)
    return(d)
  }
  
  if(region == "English Channel") {
    reg <- c("France", "UK")
    d <- ggplot2::map_data(map, reg) %>% 
      dplyr::filter(!subregion %in% c("Corsica", "Scotland", "Northern Ireland"))
    return(d)
  }
  
  if(region == "Baltic Sea") {
    reg <- c("Denmark", "Sweden", "Finland", "Russia", "Latvia", "Lithuania",
             "Estionia", "Poland", "Germany", "Norway")
    d <- ggplot2::map_data(map, reg)
    return(d)
  }
  
}
