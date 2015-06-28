# source: http://stackoverflow.com/questions/29421257/extracting-the-parent-polygon-from-a-nested-spatialpolygonsdataframe-or-dissolv
# Would be nice if this could be part of the gisland package

setGeneric('drop_hole',def=function(poly, ...){
  standardGeneric('drop_hole')
})

# Drop a single sp::Polygon if it is holey
setMethod('drop_hole',signature=signature('Polygon'),
          def=function(poly) {
            #return only Polygons which are not holes
            if (poly@hole) NULL else poly
          }
)

# Drop holey sp::Polygon entries in the @Polygons list of an sp::Polygons class
setMethod('drop_hole', signature = signature('Polygons'),
          def = function(poly) {
            noHoles <- lapply(poly@Polygons, drop_hole)
            #Remove the NULL entries from the list
            noHoles <- Filter(Negate(is.null), noHoles)
            # Turn back into a (single) Polygons
            # The generator function (sp::Polygons) fills in the other slots!
            # return the new sp:Polygons object
            sp::Polygons(noHoles, ID = poly@ID)
          }
)

# Drop holey parts of sp::Polygons in the @polygons list 
# of an sp::SpatialPolygonsDataFrame
setMethod('drop_hole', signature = signature('SpatialPolygonsDataFrame'),
          def = function(poly) {
            noHoles <- lapply(poly@polygons, drop_hole)
            # Put the un holey Polygons list back into the @polygons slot 
            poly@polygons <- noHoles
            #return the modified SpatialPolygonsDataFrame 
            poly
          }
)

#landhelgi_nohole <- drop_hole(landhelgi)
