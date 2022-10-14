merge.many <- function(xs,
                       by=intersect.list(lapply(xs,names)),
                       all=FALSE,
                       suffixes=sprintf(".%d",1:length(xs))
                       ){
  if (length(by)==1) by <- as.list(rep(by,length(xs)))
  if (is.logical(all)) all <- as.list(rep(all,length(xs)))
  
  if (length(xs)==2) {
    return(merge(xs[[1]],xs[[2]],
                 by.x=by[[1]],by.y=by[[2]],
                 all.x=all[[1]],all.y=all[[2]],
                 suffixes=suffixes))
  } else {
    return(merge(xs[[1]],
                 Recall(xs[2:length(xs)],
                        by=by[2:length(xs)],
                        all=all[2:length(xs)],
                        suffixes=suffixes[2:length(xs)]
                        ),
                 by.x=by[[1]],by.y=by[[2]],
                 all.x=all[[1]],all.y=all[[2]],
                 suffixes=suffixes[1:2]))
  }
}

intersect.list <- function(xs){
  if (length(xs)==2) return(intersect(xs[[1]],xs[[2]]))
  else return(intersect(xs[[1]],Recall(xs[2:length(xs)])))
}
