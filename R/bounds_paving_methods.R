
## TODO: names of parameters in paving
simulate.bounds_paving <- function(object,nsim=1,seed=NULL,...){
  vals <- t(replicate(nsim,{
    bb <- sample(1:length(object$bound),1)
    sapply(object$bound[[bb]],function(b) runif(1,min=b[1],max=b[2]))
  }))
  names(vals) <- names(paving$init)
  vals
}

inside <- function(paving,pts){
  if (!inherits(pts,"matrix")) pts <- matrix(pts,nrow=1)
  apply(pts,1,
        function(pt) {
          for (b in paving){
            if (all(pt>=sapply(b,function(x) x[[1]])) &
                all(pt<=sapply(b,function(x) x[[2]]))) return(T)
          }
          return(F)
        })
}##inside

print.bounds_paving <- function(x,...){
  cat("$init (Initial box)\n")
  cat(paste(sapply(x$init,function(b) paste("[",signif(b[1]),",",signif(b[2]),"]")),collapse="\n"))
  cat("\n$boundary\n length:",length(x$boundary))
  cat("\n$set\n length:",length(x$set))
  cat("\n$ntests (Number of calls of incl.fn):",x$ntests)
  cat("\n$termination:",x$termination)
  cat("\n$tol (Maximum box width):",x$tol)
  cat("\n")
}


#### ignores structure
square.bounds <- function(box.set){
  stopifnot(length(box.set)>0)
  dim.ranges <- lapply(1:length(box.set[[1]]),
                       function(d) range(unlist(lapply(box.set,function(b) b[[d]]))))
  aa <- as.data.frame(do.call(rbind,dim.ranges))
  aa$d <- 1:nrow(aa)
  names(aa) <- c("l","u","d")
  return(aa)
}

box.bounds <- function(box.set) {
  library(reshape)
  aa <- cast(melt(lapply(box.set,function(box) lapply(box,function(b) data.frame(l=b[1],u=b[2]))),measure.vars=1:2),formula = L1+L2 ~ variable)
  names(aa) <- c("b","d","l","u")
  return(aa)
}

##subset boxes
subset.box <- function(box.set,fn){
  box.set <- box.set[which(sapply(box.set,fn))]
  return(box.set)
}
