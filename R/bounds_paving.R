## boundary finding
## finding convex polyhedron containing T
## not quite vertex enumeration

## moore-1992
## jaulin-walter-1993
## bounds: list of lists of length 2 numeric vectors
## todo: keep ... in object - currently has to be given on update too
pavingBounds <- function(box.init,incl.fn,tol=1e-1,plot.vars=NA,max.tests=1e5,...){
  stopifnot(checkValidBox(box.init))
  obj <- list(
              init=box.init,
              boundary=list(box.init),
              incl.fn=incl.fn,
              tol=Inf,
              plot.vars=plot.vars,
              ntests=0,
              set=list()
              )
  class(obj) <- c("bounds_paving",class(obj))
  update(obj,max.tests=max.tests,tol=tol,...)
}##pavingBounds

update.bounds_paving <- function(object,...,max.tests=1e5,tol=1e-1){
  all.in <- object$set
  ntests<-object$ntests
  queue <- object$boundary
  incl.fn <- object$incl.fn
  plot.vars <- object$plot.vars
  stopifnot(tol<object$tol)
  stopifnot(max.tests>=ntests)
  if (!any(is.na(plot.vars))) plot(NULL,
                                   xlab=NA,
                                   ylab=NA,
                                   xlim=object$init[[plot.vars[1]]],
                                   ylim=object$init[[plot.vars[2]]])
  while(T){
    if (length(queue)==0) {
      object$termination <- "Empty queue"
      warning("Ran out of boxes: none of the boxes are on the boundary")
      break
    }
    box <- queue[[1]]
    queue[[1]] <- NULL
    if (!any(is.na(plot.vars))) rect(box[[plot.vars[1]]][1],box[[plot.vars[2]]][2],
                                     box[[plot.vars[1]]][2],box[[plot.vars[2]]][1])
    box.widths <- sapply(box,diff)
    box.width <- max(box.widths)
    if (box.width<tol) {
      object$termination <- "Tolerance reached"
      break
    }
    which.dim <- which.max(box.widths)
    box.lower <- box
    box.lower[[which.dim]] <- c(box.lower[[which.dim]][1],mean(box.lower[[which.dim]]))
    box.upper <- box
    box.upper[[which.dim]] <- c(mean(box.upper[[which.dim]]),box.upper[[which.dim]][2])

    ## Feasibility test
    for (box in list(box.upper,box.lower)) {
      is.in <- incl.fn(box,...)
      ntests<-ntests+1
      if (is.in==1) all.in <- c(all.in,list(box)) ##keep boxes that are completely in
      else if (is.in==0) queue <- c(queue,list(box)) ##keep boxes that are partly in
      else if (is.in!=-1) stop("Result of incl.fn was not in -1,0,1")
    }
    if (ntests>=max.tests) {
      object$termination <- "Max tests reached"
      break
    }
  }
  object$boundary <- queue
  object$set <- all.in
  object$ntests <- ntests
  object$termination
  object$tol <- box.width
  class(object) <- unique(c("bounds_paving",class(object)))
  invisible(object)
} ##update

test.vertices <- function(box,boolean.fn){
  xs <- do.call(expand.grid,box)
  is.in <- apply(xs,1,boolean.fn)
  if (all(is.in)) return(1)
  else if (any(is.in)) return(0)
  else return(-1)
}

test.bounds <- function(intervals,bounds){
  if (!inherits(intervals,"matrix")) intervals<-matrix(intervals,ncol=1)
  stopifnot(all(apply(intervals,2,function(x) x[1]<=x[2])))
  ##if(all(apply(intervals,2,function(ee) all(bounds[1] <= ee & ee <= bounds[2])))) return(1)
  if(all(apply(bounds[1]<=intervals & intervals<=bounds[2],2,all))) return(1)
  else if (any(apply(intervals,2,function(ee) all(ee < bounds[1]) | all(ee > bounds[2])))) return(-1)
  else return(0)
}

eval.interval <- function(fun,box,...){
  library(ipptoolbox)
  dsevalmc(function(m) fun(as.numeric(m),...),
           x=lapply(box,function(x) dsstruct(c(x,1))),
           10,optimizer=dsopt)[[1]][1:2]
}

eval.intervalD <- function(fun,box,...){
  aaa <- do.call(expand.grid,lapply(box,function(x) seq(x[1],x[2],length.out=10)))
  range(apply(aaa,1,function(p) fun(p,...)))
}

eval.intervalO <- function(fun,box,...){
  min <- optim(par=sapply(box,mean),fn=fun,...,method="L-BFGS-B",
               lower=sapply(box,function(x) x[[1]]),
               upper=sapply(box,function(x) x[[2]])
               )
  ##if (!is.null(min$message)) stop(min$message)
  if (min$convergence!=0) warning(min$message)
  max <- optim(par=sapply(box,mean),fn=fun,...,method="L-BFGS-B",
               lower=sapply(box,function(x) x[[1]]),
               upper=sapply(box,function(x) x[[2]]),
               control=list(fnscale=-1)
               )
  ##if (!is.null(max$message)) stop(max$message)
  if (max$convergence!=0) warnings(max$message)
  stopifnot(max$value>min$value)
  c(min$value,max$value)
}

eval.intervalLHS <- function(fun,box,samples,...){
 library(hydromad) 
 aaa<-parameterSets(box,samples)
 range(apply(aaa,1,function(p) fun(p,...)))
}

eval.intervalM <- function(fun,box,...){
 x<-fun(sapply(box,function(x) x[[1]]),...)
 y<-fun(sapply(box,function(x) x[[2]]),...)
 sort(c(x,y))
}

checkValidBox <-   function(box) all(sapply(box,function(x) x[2]>x[1]))

