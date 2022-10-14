##' Parse model, determine runorder and iterate \code{niter} times
##' @return state at each iteration in a list for flexibility
##' as.data.frame method converts single valued states to table format for ease of use
iterModel <- function(edges,niter,init.state=list(),pars=default.pars,
                      keep.timesteps=T,return.runorder=F,do.checks=T,
                      runorder=NULL
                      ){

  ## TODO: allow suppressing checks
  ## TODO: allow function calls to be passed as edges argument
  ## TODO: allow pars to be incorporated using modifyList?
  
  ## Process iterModel parameters
  timestep.fun <- NULL
  if (is.function(keep.timesteps)){
    timestep.fun <- keep.timesteps
    keep.timesteps <- T
  } else if (is.character(keep.timesteps)){
    wanted.elements <- keep.timesteps
    timestep.fun <- function(x) {
      if(!all(wanted.elements %in% names(x)))
        stop(sprintf("Requested to return unknown column(s): %s\nColumns available: %s",
                     paste(wanted.elements[!wanted.elements %in% names(x)],collapse=", "),
                     paste(names(x),collapse=",")
                     ))
      x[names(x) %in% wanted.elements]
    }
    keep.timesteps <- T
  }
  stopifnot(is.list(pars)) ##Environment not ok for with. use as.list instead?

  fun.calls <- parseCalls(edges)
  
  ## Obtain a list of the pars to be used with each function call
  req.pars <- lapply(names(fun.calls),function(n) {
    obj <- names(pars)
    attr(obj,"default") <- T
    if ("pars" %in% names(formals(get(n)))) {
      aaa <- try(eval(formals(get(n))$pars),silent=T)
      if (!inherits(aaa,"try-error")){
        obj <- names(aaa)
        attr(obj,"default") <- F
      }
    }
    obj
  })
  names(req.pars) <- names(fun.calls)
  req.pars.all <- sapply(req.pars,attr,which="default")

  ## Check that they are all available
  if(do.checks && !all(unlist(req.pars) %in% names(pars))) {
    fun.missing.pars <- lapply(names(fun.calls),
                               function(n) req.pars[[n]][!req.pars[[n]] %in% names(pars)])
    names(fun.missing.pars) <- names(fun.calls)
    fun.missing.pars <- fun.missing.pars[sapply(fun.missing.pars,length)>0]
    fun.missing.pars <- paste(sapply(names(fun.missing.pars),
                                     function(n) sprintf("%s: %s",
                                                         n,
                                                         paste(fun.missing.pars[[n]],collapse=",")
                                                         )),
                              collapse="\n")
    stop(sprintf("Functions are requesting missing parameters:\n%s",fun.missing.pars))
  }

  ## Check if the arguments of any function may be masked by parameters
  ## Excluding functions that specify their parameters (assume that function is sane)
  if (do.checks && any(unlist(fun.calls[req.pars.all] %in% names(pars)))) {
    pars.masking.inputs <- unique(unlist(fun.calls)) %in% names(pars)
    warning(sprintf("Functions may not work as expected. Some inputs are masked by parameters: %s",
                    paste(all.fun.args[pars.masking.inputs],sep=",",collapse=",")))
  }
  
  if (do.checks) checkEdges(fun.calls,throw.err=T)
  
  ## Setup runorder
  if(is.null(runorder)){
    current.state <- as.environment.list(init.state)
    current.state$iteration <- 1
    runorder <- getRunorder(fun.calls,current.state,pars)
  }
  
  ## Iterate
  results <- list()
  current.state <- as.environment.list(init.state)
  for (i in 1:niter){
    current.state$iteration <- i
    for (n in runorder) runFun(fun.calls[n],current.state,pars[req.pars[[n]]])
    if (keep.timesteps | i==niter){
      ## TODO: allow for thinned results
      ind <- ifelse(keep.timesteps,i,1)
      if (!is.null(timestep.fun)) results[[ind]] <- timestep.fun(as.list(current.state))
      else results[[ind]] <- as.list(current.state)
    }
  }

  class(results) <- c("imodel", class(results))
  if (return.runorder) list(results=results,runorder=runorder)
  else return(results)
    
} ##iterModel


as.data.frame.imodel <- function(x,row.names=NULL,optional=FALSE,...){
  if (!inherits(x[[1]],"list")) return(NextMethod())
  is.single.value <- sapply(x[[1]],function(i) length(i)<=1)
  if (any(!is.single.value)) warning(paste("Complex variables omitted:",paste(names(x[[1]])[!is.single.value],collapse=",")))
  x <- lapply(x,function(i) i[is.single.value])
  x <- lapply(x,function(i) {
    i[sapply(i,function(j) length(j)==0)] <- NA
    i
  })
  x <- do.call("rbind",lapply(x,as.data.frame))
  ##x <- cbind(iteration=row.names(x),x)
  x
}
