devtools::install_github("einarhjorleifsson/gisland")

CP <- as(raster::extent(-24, -21.6, 64.8, 65.7), "SpatialPolygons")
sp::proj4string(CP) <- PRO
bfjord <- rgeos::gIntersection(biceland, CP, byid=TRUE)
sp::plot(bfjord,col="grey90")

dsn <- "/home/einarhj/stasi/gis/atlantis_iceland/ia_shapefiles/wgs84"
z <- rgdal::readOGR(dsn,'iceland_contoursWGS84_noland')
i <- z$level %in% c(-3500,-3000,-2500,-2000,-1500,seq(-1000,-100,by=100))
depth <- z[i,]
depth$level <- -depth$level
devtools::use_data(depth)
