\name{map-elements}
\alias{mapping}
\alias{cartography}
\title{
Mapping utility functions
}
\description{
Add cartographical elements to a map plot
}
\details{
Mapping utility functions
Philosophy:
- every additional object is also a spatial object, and has the same reference system as the map
    they could be imported from shapefile and appropropriately transformed
- the properties of the object are determined once the main plot has been created
    map properties can be used to make dynamically created objects pretty
  each object can be placed separately from its creation - it is shifted using an affine transformation
  objects can be placed manually, and the coordinates kept for future use (e.g.creation of pngs)
}
\author{
Joseph Guillaume
}
\seealso{
\code{\link{north.arrow}}
\code{\link{scale.bar}}
\code{\link{place.item}}
}
\examples{

## Load example dataset
library(maptools)
data(wrld_simpl)
## When set to NULL or NA, the location of the cartographical elements is interactively given by clicking on the plot
loc.sb <-NULL
loc.na <- NULL

plot(wrld_simpl)
box()
## A scale bar of 10,000 km split into 4 ticks, output into the CRS of the world dataset
sb <- scale.bar(tics=0:4/4*10,units="thousand km",multiplier.units.CRS=1e6,out.CRS=wrld_simpl)
## A north arrow in the CRS of the world dataset
na <- north.arrow(wrld_simpl)
## Place items. For each of them, click on the plot
loc.sb <- place.item(loc=loc.sb,item=sb)
loc.na <- place.item(loc=loc.na,item=na)
## When this plotting sequence is run a second time, the locations are remembered

}