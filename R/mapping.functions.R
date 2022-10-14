## Mapping utility functions
## Philosophy:
## - every additional object is also a spatial object, and has the same reference system as the map
##    they could be imported from shapefile and appropropriately transformed
## - the properties of the object are determined once the main plot has been created
##    map properties can be used to make dynamically created objects pretty
##  each object can be placed separately from its creation - it is shifted using an affine transformation
##  objects can be placed manually, and the coordinates kept for future use (e.g.creation of pngs)

##epsg <- make_EPSG()
##epsg[grep("Lambert",epsg$note),]$note
## For australia: 3112, GDA94 lambert
##na.omit(epsg$prj4[epsg$code==3786])

library(rgdal)

## Currently EPSG:3786 World Equidistant Cylindrical (Sphere)
default.projection <- CRS("+proj=eqc +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs ")

north.arrow <- function(in.CRS=default.projection){

  if (class(in.CRS)=="character") in.CRS <- CRS(in.CRS)
  if (inherits(in.CRS,"Spatial")) in.CRS <- CRS(proj4string(in.CRS))

  dd <- data.frame(
                   label=c("N","",""),
                   stringsAsFactors=FALSE)
  row.names(dd) <- c("Up","left","right")
  height <- strheight("M")*2
  tip <- strheight("e")
  sb <- SpatialLinesDataFrame(
                              SpatialLines(list(
                                                Lines(list(Line(matrix(c(0,0,0,height),nrow=2))),ID="Up"),
                                                Lines(list(Line(matrix(c(0,
                                                                       -tip*sin(pi/4),
                                                                       height,
                                                                       height-tip*cos(pi/4)),
                                                                       nrow=2)
                                                           )),
                                                           ID="left"),
                                                Lines(list(Line(matrix(c(0,
                                                                       tip*sin(pi/4),
                                                                       height,
                                                                       height-tip*cos(pi/4)),
                                                                       nrow=2)
                                                           )),
                                                           ID="right")

                                                ),
                                           proj4string=in.CRS),dd
                              )
  return(sb)
  
}##north.arrow


## Line scale bar with tics at required locations
## @param tics location of tics on scale bar
## @param in.CRS input units also takes numeric epsg code, or an object
## @param out.CRS output units as for in.CRS defaults to same as input
## @returns SpatialLinesDataFrame object with tic labels in label column
scale.bar <- function(tics=c(0,10),units="km",multiplier.units.CRS=1e3,
                      in.CRS=default.projection,out.CRS=NULL,
                      height=NULL
                      ){ 
  ## convert in.CRS and out.CRS
  if (class(in.CRS)=="character") in.CRS <- CRS(in.CRS)
  if (inherits(in.CRS,"Spatial")) in.CRS <- CRS(proj4string(in.CRS))

  if (is.null(height)) height <- strheight("M")

  p <- strsplit(CRSargs(in.CRS),"\\+")[[1]]
  p.units <- strsplit(p[grep("units",p)],"=")[[1]][2]
  p.units <- gsub(" ","",p.units)
  labels.tics <- as.character(tics)
  labels.tics[length(labels.tics)] <- paste(labels.tics[length(labels.tics)],units)
  tics <- tics*multiplier.units.CRS

  tics <- tics
  dd <- data.frame(
                   label=c("",labels.tics),
                   "adj_x"=c(1,rep(0.5,length(tics)-1),0),
                   stringsAsFactors=FALSE)
  row.names(dd) <- c("horizontal",as.character(tics))

  sb <- SpatialLinesDataFrame(
                              SpatialLines(append(list(
                                                Lines(list(Line(cbind(range(tics),0))),
                                                      ID="horizontal")),
                                           lapply(tics,function(x) Lines(list(Line(
                                                               matrix(c(x,-height/2,
                                                                        x,height/2),
                                                                      nrow=2,byrow=TRUE))),
                                                                         ID=as.character(x)))
                                                             ),
                                           proj4string=in.CRS),dd
                              )
  if (!is.null(out.CRS)) {
    if (class(out.CRS)=="character") out.CRS <- CRS(out.CRS)
    if (inherits(out.CRS,"Spatial")) out.CRS <- CRS(proj4string(out.CRS))

    sb <- spTransform(sb,out.CRS)

    sb <- affineShift.SpatialLines(sb,-bbox(sb)[,"min"])
    
    sb@lines[[1]]@Lines[[1]]@coords[,2] <- 0
    
    for (l1 in 2:length(sb@lines)){
      sb@lines[[l1]]@Lines[[1]]@coords[,2] <- c(-height/2,height/2)
    } ##l1
    sb@bbox["y",] <- c(-height/2,height/2)
  }   #change CRS
  
  return(sb)
} ##scale.bar

## @param scale.bar=
place.item <- function(loc=NULL,item=scale.bar()){

  stopifnot(class(item) %in% c("SpatialLines","SpatialLinesDataFrame"))

  if (is.null(loc)||is.na(loc)) loc <- as.numeric(locator(1))

  ## TODO: allow polygons
  item <- affineShift.SpatialLines(item,offset=loc)

  plot(item,add=TRUE)

  ## TODO: generalise
  for (i in 1:length(item@lines)){
    adj <- c(0.5,-1) #centred above
    if ("adj_x" %in% names(item@data)) adj[1] <- item@data$adj_x[i]
    if ("adj_y" %in% names(item@data)) adj[2] <- item@data$adj_y[i]
    text(t(coordinates(item@lines[[i]])[[1]][2,]),item@data$label[i],adj=adj)
  }

  invisible(loc)
  
}##place.item

affineShift.SpatialLines <- function(obj,offset,scale=1,...){
  stopifnot(class(obj) %in% c("SpatialLines","SpatialLinesDataFrame"))
  
  if (length(offset) != 2) 
    stop("offset should have length 2")
  if (is.list(offset)) 
    offset = c(offset[[1]], offset[[2]])
  if (length(scale) == 1) 
    scale = rep(scale, 2)
      
  for (l1 in 1:length(obj@lines)){
    for (l2 in 1:length(slot(obj@lines[[l1]],"Lines"))){
      cc <- slot(obj@lines[[l1]],"Lines")[[l2]]@coords
      slot(obj@lines[[l1]],"Lines")[[l2]]@coords[,1] <- scale[1]*cc[,1]+offset[1]
      slot(obj@lines[[l1]],"Lines")[[l2]]@coords[,2] <- scale[2]*cc[,2]+offset[2]
    } ##l2
  }   ##l1

  return(obj)
}##affineShift

##text(coordinates(sb$obj),tics)

##      SpatialPolygonsRescale(
##                             layout.scale.bar(),
##                             offset=bbox(poly.pwa)[,"min"],
## #                            scale = 500,
##                             scale=0.25,
##                             fill=c("transparent","black"),
##                             plot.grid=FALSE
##                             )


## based on (Tanimura et al, 2007)
scalebar <- function(loc=as.numeric(locator(1)),length,unit="km",division.cex=.8,...) {
x <- c(0,length/c(4,2,4/3,1),length*1.1)+loc[1]
y <- c(0,length/(10*3:1))+loc[2]
cols <- rep(c("black","white"),2)
for (i in 1:4) rect(x[i],y[1],x[i+1],y[2],col=cols[i])
for (i in 1:5) segments(x[i],y[2],x[i],y[3])
labels <- x[c(1,3)]-loc[1]
labels <- append(labels,paste(x[5]-loc[1],unit))
text(x[c(1,3,5)],y[4],labels=labels,adj=.5,cex=division.cex)
}

