## NEED TO: label.locs <- NULL

getLabelPos <- function(n){
  library(grid)
  if (n %in% names(label.locs)) label.locs[[n]]
  else {
    print(n)
    l <- grid.locator()
    l <- lapply(l,sub,pattern="native",replacement="")
    l <- lapply(l,as.numeric)
    label.locs[[n]] <<- l
    l
  }
}

placeLabel <- function(n,...){
  args <- list(...)
  args <- modifyList(args,getLabelPos(n))
  args$labels <- n
  do.call(panel.text,args)
}

placeLabels <- function(labels,...){
  extra <- list(...)
  for (i in 1:length(labels)){
    args <- modifyList(list(n=labels[i]),extra)
    ##TODO: expand to other arguments
    if ("col" %in% names(args)){
      if (length(args$col)>1) args$col <- args$col[i]
    }
    do.call(placeLabel,args)
  }
}
