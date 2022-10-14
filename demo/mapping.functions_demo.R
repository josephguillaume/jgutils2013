library(maptools)

data(wrld_simpl)
loc.sb <-NULL
loc.na <- NULL

source("mapping.functions.R")
plot(wrld_simpl)
box()
sb <- scale.bar(tics=0:4/4*10,units="thousand km",multiplier.units.CRS=1e6,out.CRS=wrld_simpl)
na <- north.arrow(wrld_simpl)
loc.sb <- place.item(loc=loc.sb,item=sb)
loc.na <- place.item(loc=loc.na,item=na)

png("mapping_test.png")
plot(wrld_simpl)
box()
sb <- scale.bar(tics=0:4/4*10,units="thousand km",multiplier.units.CRS=1e6,out.CRS=wrld_simpl)
na <- north.arrow(wrld_simpl)
loc.sb <- place.item(loc=loc.sb,item=sb)
loc.na <- place.item(loc=loc.na,item=na)
dev.off()
