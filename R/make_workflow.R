## Functions for parsing edges, checking existence of values, obtaining values, running the model + utilities to the process


##' @par edges string or vector of strings with path of use of variables TODO: better expl
##' @return list of functions to be called with the inputs to use
parseCalls <- function(edges){
  ## Remove initial and final newline, "
  edges <- gsub("^\n","",edges)
  edges <- gsub("\n$","",edges)
  edges <- gsub('"',"",edges)
  ## If several newlines, split into vector
  edges <- unlist(strsplit(edges,"\n"))
  edges <- gsub("^ *","",edges) ##Initial spaces
  if (length(grep("^ *$",edges))>0) edges <- edges[-grep("^ *$",edges)] ##Empty linse
  if (length(grep("//",edges))>0) edges <- edges[-grep("//",edges)]
  ## Split calls into input and function  
  edges.l <- strsplit(edges,"->")
  edges.l <- lapply(edges.l,gsub,pattern=" ",replacement="")

  ##print(edges.l)
  
  ## Group by function
  ## Create function calls
  fun.calls <- list()
  for (e in edges.l){
    ## Non-participating independent function should still be included
    ## DEPRECATED - every function should have an input. iteration and pars are simply not represented in graph
    if (length(e)==1) e <- c("",e)
    ##Split into fun and arg
    fun.arg <- strsplit(e[2],":")[[1]]
    if (!fun.arg[1] %in% names(fun.calls)) fun.calls[[fun.arg[1]]] <- list()
    stopifnot(length(fun.arg)<=2)
    if (length(fun.arg)==2) fun.calls[[fun.arg[1]]][[fun.arg[2]]] <- e[1]
##    else if (length(fun.arg)==1 && !exists(fun.arg[)

    ## For functions that only appear on LHS
    ## i.e. have no arguments
    fun.argl <- strsplit(e[1],":")[[1]]
    if (length(fun.argl)==2){
      if (!fun.argl[1] %in% names(fun.calls)) fun.calls[[fun.argl[1]]] <- list()
       ## Where variable has same name as function or is out, it is an output -> ignored
       if (fun.argl[2]!=fun.argl[1] && !fun.argl[2] %in% names(fun.calls[[fun.argl[1]]]) && fun.argl[2]!="out") fun.calls[[fun.argl[1]]][[fun.argl[2]]] <- list()
     }
   }
   ## Check for missing inputs
   for (fn in names(fun.calls)){
     args.given <- names(fun.calls[[fn]])
     if (!exists(fn)) {
       warning(sprintf("Function %s doesn't exist",fn))
     } else {
       args.expected <- names(formals(get(fn)))
       ## TODO: got to be a better way of doing this
       args.expected.hasdefault <- sapply(formals(get(fn)),function(x) tryCatch(!is.null(eval(x)),error=function(e) F))

       args.expected.hasdefault <- args.expected.hasdefault[order(args.expected)]
       if (length(args.expected)>0) args.expected <- sort(args.expected)
       if (length(args.given)>0) args.given <- sort(args.given)
       ## Special variables
       args.expected.retained <- args.expected!="..."
       if ("iteration" %in% args.expected) {
         args.given <- c(args.given,"iteration")
         fun.calls[[fn]]$iteration <- "iteration:iteration"
       }
       if ("pars" %in% args.expected){
         args.given <- c(args.given,"pars")
         fun.calls[[fn]]$pars <- "pars"
       }
       ## Exclude arguments with default values that aren't given
       args.expected.retained <- args.expected.retained & (!args.expected.hasdefault | args.expected %in% args.given)
       ## Check
       args.expected <- args.expected[args.expected.retained]
       if (!identical(args.expected,args.given)) {
         if (any(!args.given %in% args.expected)) stop(sprintf("Unused arguments given to function %s: %s ",fn,paste(args.given[!args.given %in% args.expected],collapse=",")))
         if (any(!args.expected %in% args.given)) stop(sprintf("Missing arguments for function %s: %s ",fn,paste(args.expected[!args.expected %in% args.given],collapse=",")))
       }
     }
   } ## for fun.calls
   fun.calls
 }

 ##' @return current value of a variable, either from global env or output from a previous function run
 getCurrentValue <- function(var,current.state){
   var <- var[!var=="pars"]
   var <- lapply(var,strsplit,split=":")
   var <- lapply(var,function(v){
     v <- v[[1]]
     ##print(v)
     if (length(v)==1) {
       return(get(v,envir=current.state))
     } else {
       return(current.state[[v[[1]]]])
     }
   })
   var
 }

##' Check whether all variables are available
##' @return list of variables that are missing
findMissingVar <-  function(var,current.state){
  var <- var[!var=="pars"]
  varl <- lapply(var,strsplit,split=":")
  var <- var[!sapply(varl,function(v){
    v <- v[[1]]
    if (length(v)==1) return(exists(v,envir=current.state))
    else return(exists(v[[1]],envir=current.state,inherits=FALSE))
  })]
  var
}

checkHaveAll <- function(var,current.state){
  return(length(findMissingVar(var,current.state))==0)
}

## TODO: support non-atomic output to be input elsewhere
##' Run a function and retain its output
##' @par fun.call a list with as name the function to run, and with a list of named arguments to pass to that function
##'  i.e. a single element of the list returned by \link{\code{parseCalls}}
runFun <- function(fun.call,current.state,pars){
  stopifnot(inherits(fun.call,"list") && inherits(fun.call[[1]],"list"))
  fun <- names(fun.call)
  fun.call <- fun.call[[1]]
  fun.args <- getCurrentValue(fun.call,current.state)
  if (!"pars" %in% names(fun.args) && "pars" %in% names(formals(get(fun)))) fun.args$pars <- pars
  current.state[[fun]] <- do.call(fun,fun.args)
}

##' Automatically determine run order. To be run at first iteration
##' Iteratively run the first function that has all necessary variables available
## For all function calls: sapply(cc,checkHaveAll,current.state=current.state)
##' @return character vector with order of functions to run
getRunorder <- function(fun.calls,current.state,pars,return.output=F,trace=F){
  cc.runorder <- fun.calls
  runorder <- c()
  while(length(cc.runorder)>0){
    old.length <- length(cc.runorder)
    cc.ns <- names(cc.runorder)[which(sapply(cc.runorder,checkHaveAll,current.state=current.state))]
    if (trace) print(cc.ns)
    cc.n <- cc.ns[1]
    if (old.length>0 && is.na(cc.n)) stop(sprintf("Can't run any of remaining functions: %s. \ncheckHaveAll says missing variables are: %s",
                                                  paste(names(cc.runorder),collapse=","),
                                                  paste(sapply(cc.runorder,findMissingVar,current.state=current.state),collapse=",")
                                                  ))
    runorder <- c(runorder,cc.n)
    runFun(fun.calls[cc.n],current.state,pars)
    cc.runorder[[cc.n]] <- NULL
    if (length(cc.runorder)==old.length) stop(paste("Unrunnable functions remaining:",paste(names(cc.runorder),collapse=","))) ##Shouldn't be reached - because NA would catch it
  }
  if (return.output) return(list(runorder=runorder,current.state=current.state))
  else return(runorder)
}

## NOT a generic function, because as.environment is primitive
## Not exported
as.environment.list <- function(x,parent=globalenv()){
  env <- new.env(parent=parent)
  lapply(names(x),function(n) assign(n,x[[n]],envir=env))
  return(env)
}

## TODO: Check that function inputs and outputs match
checkEdges <- function(fun.calls,throw.err=T){

  ## Get units attribute of all functions & vars
  ##  list for functions, incl
  ## For each function call
  ##  check if units of given input matches required input
  ##  NA where missing
  ##if (throw.err && some falses) stop, extracting the function input
  #else F
  T
}## checkEdges


## TODO: create edges/function calls from function input and output types

## TODO: utilities to convert units where variable is the same &/or dimensions are the same


##' @par input data
##' @return function that returns value for a particular iteration
feedTimestepData <- function(input){
  return(
         function(iteration){
           stopifnot(length(iteration)==1)
           if (inherits(input,"list")) input[[iteration]]
           else if (is.null(dim(input))) input[iteration]
           else input[iteration,]
         })
}


## TODO: during run output functions
## create utility functions that add required monitoring to the existing edges or fun.calls
gradualplot <- function(x,xs){
  xs <- c(x,xs)
  plot(xs,xlim=c(0,nrow(rain)))
  return(xs)
}
'
//during run output functions
getYear:getYear -> print:x
hhead:hhead -> gradualplot:x
gradualplot:gradualplot -> gradualplot:xs
'

## Summarise self-documented functions
ls.fun <- function() ls(env=.GlobalEnv)[
                          sapply(ls(env=.GlobalEnv),function(x) inherits(get(x),"function") & !is.null(attr(get(x),"units")))
                          ]
##do.call(c,lapply(ls.fun(),function(x) eval(formals(x)$pars)))
## attr(yield,"units")
## eval(formals(yield)$pars)

## Create edges from analysis of function input and output variables and units
autoEdges <- function(fns,use.default=F){
  edges <- ""
  fns.units <- sapply(fns,function(x) attr(get(x),which="units"))
  for (fn in fns){
    fn.ins <- fns.units[[fn]][names(fns.units[[fn]])!="out"]
    for (fn.in.name in names(fn.ins)){
      fn.in <- fn.ins[[fn.in.name]]
      possible.inputs <- fns[fns!=fn][which(sapply(fns.units[fns!=fn],function(x) identical(x$out,fn.in)))]
      if (length(possible.inputs)==1) edges <- sprintf('%s"%s":out -> "%s":"%s"\n',edges,possible.inputs,fn,fn.in.name)
      else {
        if (length(possible.inputs)>1) msg <- "More than one option fitting possible input."
        else if (length(possible.inputs)==0) msg <- "No option available for specified input"
        warning(sprintf("%s
 function: %s
 input: %s
 type: %s
",
                        msg,fn,fn.in.name,paste(fn.in,collapse=", ")))
        if (use.default){
          edges <- sprintf('%s"%s" -> "%s":"%s"\n[color="red"]\n',edges,fn.in.name,fn,fn.in.name)
          ##edges <- sprintf('%s"%s" [color="red"]\n',edges,fn.in.name)
        }
      }
    } ##fn.ins
  }   ##fns
  edges
}     ##autoEdges




t.imodel <- function(res) {
  res2 <- lapply(names(res[[1]]),function(x) t(sapply(res,function(y) y[[x]])))
  names(res2) <- names(res[[1]])
  res2
}
