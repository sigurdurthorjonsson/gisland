#' @title plot_catch
#' 
#' @description Plots the 2013 catch distribution for selected stocks
#' 
#' @param r a raster object
#' @param species a numerical denoting species
#' @param brks a numerical vector denoting breaks
plot_catch <- function(r,species=1,brks=c(0.01,0.1,1,10,50,500)) {
  
  iceland <- sp::spTransform(iceland,ISN93)
  
  r <- raster::subset(r,paste("s",species,sep=""))
  e <- raster::extent(r)
  
  #depths <- sp::spTransform(gbdypi.sldf,ISN93)
  #depths <- depths[depths@data$depth %in% c(200,500),]
  
  r <- cbind(sp::coordinates(r),values(r))
  colnames(r) <- c("x","y","z")
  r <- as.data.frame(r)
  r$z <- cut(r$z,breaks=brks)
  p <- ggplot2::ggplot(r) + 
    ggplot2::element_blank() +
    ggplot2::theme_bw() +
    ggplot2::geom_raster(ggplot2::aes(x,y,fill=z)) +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(brks)-1,"YlOrRd")) +
    ggplot2::geom_path(data=iceland,ggplot2::aes(long,lat,group=group),lwd=0.25) +
    #ggplot2::geom_path(data=depths,ggplot2::aes(long,lat,group=group),lwd=0.25,col="grey") +
    ggplot2::coord_equal() +
    ggplot2::labs(x="",y="",fill = "tonnes km-2") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          legend.position=c(0.52,0.45),
          legend.text = ggplot2::element_text(size=rel(0.65))) +
    ggplot2::xlim(e@xmin,e@xmax) + ggplot2::ylim(e@ymin,e@ymax)
  return(p)
}

#' @title Creates and plot a raster of effort or catch
#' 
#' @description XXX
#' 
#' @param xy A dataframe or matrix that contains longitudinal (x) and latitudinal (y)
#' coordinates.
#' @param z A numeric vector containing catch. If missing (default) the value assign
#' to z is 1, this being equivalent to effort. length of z must be the same as 
#' length of x and y.
#' @param Resolution Resolution in km of the raster to be generated
#' @param Levels The cut points (binwidths) of the values to display in the plot
#' @param plotit A boolean, if TRUE (default) plots a base graph. If FALSE
#' returns a list containing raster- and ggplot-object
#' @param xlim XXX
#' @param ylim XXX

raster_logbook <- function(xy,
                           z,
                           Resolution = 1,
                           Levels = c(5,50,100,250,500,1000,1500),
                           plotit = TRUE,
                           xlim = c(-30,-10),
                           ylim =  c(62.25,67.75)) {
    
  i <- xy[,1] >= xlim[1] & xy[,1] <= xlim[2] & xy[,2] >= ylim[1] & xy[,2] <= ylim[2]
  xy <- as.matrix(xy[i,1:2])
  if(missing(z)) {
    z <- rep(1,nrow(xy))
    ggLabel <-  paste("Per",Resolution,"km^2")
  } else {
    z <- z[i]
    z <- z/1e3
    ggLabel <-  paste("Tonn per",Resolution,"km^2")
  }
  
  xy <- sp::SpatialPoints(xy,proj4string = PRO,sp::bbox(xy))
  xy <- sp::coordinates(sp::spTransform(xy,ISN94))
  
  xlim <- sp::bbox(xy)[1,]
  ylim <- sp::bbox(xy)[2,]
  
  r <- raster::raster(xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2])
  raster::res(r) <- c(Resolution*1e3,Resolution*1e3)
  r <- raster::rasterize(xy,r,z,fun=sum)
  
  iceland <- sp::spTransform(iceland,ISN94)
  
  
  if(plotit) plot(r,legend=FALSE)
  
  if(!plotit) {
    r2 <- cbind(sp::coordinates(r),values(r))
    colnames(r2) <- c("x","y","z")
    r2 <- as.data.frame(r2)
    r2$z2 <- cut(r2$z,breaks=Levels)
    p <- ggplot2::ggplot(r2) + 
      ggplot2::element_blank() +
      ggplot2::theme_bw() +
      ggplot2::geom_raster(ggplot2::aes(x,y,fill=z2)) +
      ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(Levels)-1,"YlOrRd")) +
      ggplot2::geom_path(data=iceland,ggplot2::aes(long,lat,group=group)) +
      ggplot2::coord_equal() +
      ggplot2::labs(x="",y="",fill = ggLabel) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     legend.position=c(0.5,0.4),
                     legend.text = ggplot2::element_text(size=rel(0.75)))
    return(list(r=r,p=p))
  }
}