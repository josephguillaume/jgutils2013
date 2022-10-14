## pt.r is a matrix of coordinates or any Spatial Object (that has a coordinates method)
## d is the distance of the buffer around the points, in metres
## returns a spatialpolygons object with polygons around every point using vertices every 45 degrees
bufferPoints<-function(pt.r,d){
  library(geosphere)
  if (inherits(pt.r,"Spatial")) pt.r<-coordinates(pt.r)
  buff <- apply(pt.r,1,function(p)
                list(matrix(sapply(c(seq(0,360,by=45),0),destPoint,p=p,d=d),ncol=2,byrow=T))
                )
  poly.buff <- lapply(buff,Polygon)
  poly.buff <- lapply(1:length(poly.buff),function(id) Polygons(poly.buff[id],id))
  SpatialPolygons(poly.buff)
}##bufferPoints