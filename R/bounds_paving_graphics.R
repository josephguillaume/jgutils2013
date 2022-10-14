
## TODO: cleanup 
plot.bounds_paving <- function(x,y,plot.vars=NULL,...){
  ##if (length(x$init)!) stop("Plotting only supported for 2D pavings")
  i <- plot.vars[[1]]
  j <- plot.vars[[2]]
  plot(NULL,xlim=x$init[[i]],ylim=x$init[[j]],
       xlab="dim1",ylab="dim2",...)
  for (box in x$set) rect(box[[i]][1],box[[j]][2],box[[i]][2],box[[j]][1],col="black")
  for (box in x$boundary) rect(box[[i]][1],box[[j]][2],box[[i]][2],box[[j]][1],col="red")
}


xyplot.bounds_paving <- function(x,data,...,
                                 prepanel=NULL,
                                 panel=panel.chull,
                                 scales=list(relation="free")
                                 ){
  if (is.null(prepanel)) prepanel <- function(x,y,paving) list(xlim=paving$init[[x]],
                                                               ylim=paving$init[[y]])
  ss <- as.data.frame(t(combn(length(x$init),2)))
  names(ss) <- c("y","x")
  names.init <- names(x$init)
  if (!is.null(names.init)) ss$w <- apply(ss,1,function(x) paste(names.init[x],collapse=","))
  else ss$w <- apply(ss,1,paste,collapse=",")
  xyplot(y~x|w,ss,...,panel=panel,paving=x,
         prepanel=prepanel,scales=scales
         )
}##xyplot.bounds_paving

panel.paving <- function (x, y,...,paving,set=F) 
{
  if (set){
    for (box in paving$set) panel.rect(box[[x]][1], box[[y]][2], box[[x]][2], 
        box[[y]][1], col = "black")
  }
    for (box in paving$boundary) panel.rect(box[[x]][1], box[[y]][2], box[[x]][2], 
        box[[y]][1], col = "red")
}

panel.chull <- function(x,y,...,paving,set="boundary"){
  ccc <- chull.bounds_paving(paving,x,y,set=set)
  panel.polygon(ccc)
  ls <- apply(ccc,1,function(l) sprintf(fmt="(%.2f,%.2f)",l[1],l[2]))
  textRect(ccc$Var1,ccc$Var2,ls,pos=4,lty=0,cex=0.7)
  panel.text(ccc$Var1,ccc$Var2,labels=ls,pos=4,cex=0.7)
}

chull.bounds_paving <- function(ppp,i,j,set="boundary"){
  ppp2 <- do.call(rbind,lapply(ppp[[set]],function(oo) expand.grid(oo[[i]],oo[[j]])))
  ppp2[chull(ppp2),]
}
