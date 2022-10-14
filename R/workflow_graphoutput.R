## Functions for graph output of integrated models
## Uses parseCalls


##' @return graphviz record node representation of a function with inputs
## For single output functions only
## TODO: templates as args
getFunctionRecord <- function(fname,html=T){
  if (!exists(fname)) return(sprintf('"%s" [shape=rect]',fname)) ##Don't need to warn, because parseCalls does

  inputs <- names(formals(get(fname)))
  if (html){
    template.function <- '"NAME" [shape=plaintext,label=<
<TABLE CELLSPACING="0">
CONTENTS
</TABLE>
>]'
    ##    contents <- paste('<TD PORT="',inputs,'"><FONT COLOR="">',inputs,"</FONT></TD>",sep="")
    contents <- paste('<TD PORT="',inputs,'">',inputs,"</TD>",sep="")
    ##contents[1] <- sprintf('<TD ROWSPAN="%d" PORT="%s">%s</TD>%s',length(inputs),fname,fname,contents[1])
    contents[1] <- sprintf('%s<TD ROWSPAN="%d" PORT="%s"><FONT COLOR="BLUE">%s</FONT></TD>',contents[1],length(inputs),fname,fname)
    record <- template.function
    record <- gsub("CONTENTS",paste("<TR>",contents,"</TR>",collapse=""),record)
    record <- gsub("NAME",fname,record)
    
  } else {
    template.function <- '"NAME" [shape="record",label="{{INPUT|<NAME> NAME}}"]'
    record <- gsub('NAME',fname,template.function)
    inputs <- sprintf('{%s}',paste(sprintf("<%s> %s",inputs,inputs),collapse='|'))
    record <- sub('INPUT',inputs,record)
  }
  return(record)
}



## TODO: make makeGraph a generic?

##' Create a graphviz file in file \code{name} with
##'  represenations of the functions given by \code{fun.names}
##'  and passing of variables given in \code{edges} in graphviz format
##' Requires all functions in fun.names to be defined.
makeGraph.single <- function(name,edges,fun.names=NULL,exec=T){

  ## if (inherits(name,"imodel.inheritance")) {

  ##   return(NULL)
  ## }
  
  if (is.null(fun.names)) {
    fun.names <- names(parseCalls(edges))
    cat("Using functions: ",fun.names,"\n")
  }
  stopifnot(length(fun.names)>0)

  edges <- strsplit(edges,"\n")[[1]]
  ## Don't show iteration and pars
  edges <- gsub("^ *iteration *->.*$","",edges)
  edges <- gsub("^ *pars *->.*$","",edges)
  ## Transform port out into name of the function
  edges <- gsub('(.+):out *->','\\1:\\1 ->',edges)
  
  edges <- paste(edges,sep="\n",collapse="\n")
  
  sink(name)
  cat('digraph dd {
rankdir=LR
')
  for (f in fun.names) cat(getFunctionRecord(f),"\n")
  cat(edges)
  cat('\n}')
  sink()

  if (exec) {
    system(sprintf('dot %s -O -Tpng',name))
    system(sprintf('cmd /c "START %s.png"',name))
  }
}

makeGraph <- Vectorize(makeGraph.single,c("name","edges","fun.names"),SIMPLIFY=T,USE.NAMES=F)

## TODO: makeGraph using Rgraphviz

## Graph all models
## Assumes list elements are templates, not characters
## TODO: detect list of characters too?
makeGraph.list <- function(models){
  sapply(names(models),function(x) {
    if(inherits(models[[x]],"template") && !models[[x]]$partial) {
      makeGraph(sprintf("%s.dot",x),edges=models[[x]]$edges)
      return(T)
    }
    return(F)
  })
} ##makeGraph.list
