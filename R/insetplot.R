#library(lattice)
#library(grid)

## The levels of the identifier column should be in the same order as the locations
## if identifier is numeric or character, sorting both should be enough

## also see arguments to viewport for vp. vp has priority over x,y,just,w,h
## esp, xscale,yscale
panel.insetplot1 <- function(x,
                             ..., ##args to panel.inset
                             group.number=1,
                             ##group.value,
                             locations,
                             prepanel.inset=prepanel.default.xyplot,
                             panel.inset=panel.xyplot,
                             w=unit(3,"char"),
                             h=unit(1,"char"),
                             just=c("right","centre"),
                             vp=list()
                             ){
  ## TODO: if locations is a matrix or data frame with columns x and y
  ## TODO: allow SpatialPointsDataFrame
  ## TODO: link using attributes rather than just position
  if (inherits(locations,"data.frame") |
      inherits(locations,"matrix")){
    xloc <- locations[group.number,1]
    yloc <- locations[group.number,2]
  } else if (inherits(locations,"numeric")){
    xloc <- locations[1]
    yloc <- locations[2]
  }
  ##
  xscale <- c(0,1)
  yscale <- c(0,1)
  if (!is.null(prepanel.inset)){
    aa <- prepanel.inset(x,...)
    xscale <- aa$xlim
    yscale <- aa$ylim
  }
  ##
  vp <- modifyList(list(
                        x=unit(xloc,"native"),
                        y=unit(yloc,"native"),
                        just=just,w=w,h=h,
                        xscale=xscale,yscale=yscale
                        ),vp)
  inset.viewport <- do.call("viewport",vp)

  pushViewport(inset.viewport)
  panel.inset(x,...)
  popViewport(1)
  
}

panel.insetplot <- function(...,groups=NULL){
  if (is.null(groups)) panel.insetplot1(...)
  else panel.superpose(...,groups=groups,panel.groups=panel.insetplot1)
}

## TODO: custom default prepanel to choose xlim,ylim using locations
insetplot <- function(x,data,...,locations){
  ##stopifnot(inherits(x,"formula"))
  xyplot(x,data=data,
         ...,
         panel=panel.insetplot,
         locations=locations)
}

