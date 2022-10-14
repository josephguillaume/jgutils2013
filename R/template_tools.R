## proto package may provide an alternative
## TODO: remove edges naming?

##' Fill a template with placeholders of the shape {PLACEHOLDER.NAME} using named elements of \code{replacements}
fill.template <- function(template,replacements=list(),extra.edges="",use.default=T,keep.inheritance=T){

  if (inherits(template,"list")) {
    keep.inheritance <- T
    replacements <- modifyList(template$replacements,replacements)
    template <- c(template$template[1],template$edges)
  }
  
  edges <- template[1]
  edges <- paste(edges,extra.edges,sep="\n")

  remaining.replacements <- length(gregexpr("\\{(.*?)}",edges)[[1]])
  while(T){
    old.remaining.replacements <- remaining.replacements
    for (r in names(replacements)) {
      edges <- gsub(sprintf("{%s}",r),replacements[[r]],edges,fixed=T)
    }
    remaining.replacements <- length(gregexpr("\\{(.*?)}",edges)[[1]])
    if (remaining.replacements==old.remaining.replacements) break
  }

  ##Default values
  if (use.default) edges <- gsub("\\{(.*?)}",'"\\L\\1"',edges,perl=T)
  partial.replacement <- regexpr("\\{(.*?)}",edges)>0
  if (partial.replacement) warning("Not all placeholders were replaced")

  if(keep.inheritance) {
    obj <- list(
         template=template,
         edges=edges,
         replacements=replacements,
         partial=partial.replacement
         )
    class(obj) <- c("template", class(obj))
    obj
  } else {
    edges
  }
}


templateInheritanceGraph <- function(graph.names,name="template.inheritance.dot",exec=T){
  if (inherits(graph.names,"list")) {
    edges <- graph.names
    graph.names <- names(graph.names)
  } else {
    edges <- lapply(graph.names,get)
  }
  
  names(edges) <- graph.names
  final.edge <- lapply(edges,function(x) ifelse(inherits(x,"character"),x,x$edges))
  final.template <- lapply(edges,function(x) ifelse(inherits(x,"character"),"",last(x$template,1)))
  ## Find if template of one is the output of another
  sink(name)
  cat("digraph inheritance {\n")
  for (g1 in graph.names){
    does.inherit <- sapply(graph.names,function(g2) final.template[[g2]]==final.edge[[g1]])
    for (g2 in graph.names[does.inherit]) {
      cat(sprintf('"%s" -> "%s"\n',g1,g2))
    }
  }
  cat("\n}")
  sink()

  if (exec) {
    system(sprintf('dot %s -O -Tpng',name))
    system(sprintf('cmd /c "START %s.png"',name))
  }
}##templateInheritanceGraph
