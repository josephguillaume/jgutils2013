
## lattice:::ltext.default
## lrect
textRect1 <- function(x,y,labels,lty=0,pos=NULL,adj=c(0.5,0.5),offset=0.5,cex=add.text$cex) {
  library(grid)
  ##cat(x,y,labels,"\n")
  add.text <- trellis.par.get("add.text")
  xy <- xy.coords(x, y, recycle = TRUE)
  if (length(xy$x) == 0) 
    return()
  ux <- unit(xy$x, "native")
  uy <- unit(xy$y, "native")
  if (!is.null(pos)) {
    if (pos == 1) {
      uy <- uy - unit(offset, "char")
      adj <- c(0.5, 1)
    }
    else if (pos == 2) {
      ux <- ux - unit(offset, "char")
      adj <- c(1, 0.5)
    }
    else if (pos == 3) {
      uy <- uy + unit(offset, "char")
      adj <- c(0.5, 0)
    }
    else if (pos == 4) {
      ux <- ux + unit(offset, "char")
      adj <- c(0, 0.5)
    }
    else stop("Invalid value of 'pos'")
  }
  panel.rect(x=ux,y=uy,
             width=stringWidth(labels),
             height=stringHeight(labels),
             just=adj,
             col="white",
             lty=lty,
             alpha=0.9,
             cex=cex
             ##,hjust=0
             )
}
textRect <- Vectorize(textRect1,c("x","y","labels"))


## xyplot(1:6~1:6,panel=function(x,y,...){
##   panel.xyplot(x,y,type="l")
##   panel.abline(h=2)
##   textRect(2:3,2,c("test","(1.23,2.324)"),lty=0,pos=4)
##   panel.text(2:3,2,c("test","(1.23,2.324)"),pos=4)
## })
