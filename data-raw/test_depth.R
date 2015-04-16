
require(sp)
require(raster)
data(meuse.grid)
sgdf <- SpatialPixelsDataFrame(points=meuse.grid[c("x", "y")], data=meuse.grid)


r <- raster(sgdf, layer=6)
r.poly <- rasterToPolygons(r, fun=function(x) {x == 1}, dissolve=TRUE ) 
class(r.poly) 
plot(r.poly)

ncfname <- "/home/einarhj/stasi/gis/GEBCO/gebco_08_-50_55_0_75.nc"
r <- raster(ncfname)
r

#rp <- rasterToPolygons(tmpin, fun=function(x) {x == 1}, dissolve=TRUE ) 
rp <- rasterToPolygons(tmpin, fun=function(x) {x == -500}, dissolve=FALSE ) 
plot(rp)
