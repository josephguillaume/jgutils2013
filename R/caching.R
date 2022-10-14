concat <- function(...)paste(...,collapse=" ")

new.cache<- function(FUN,key.fun=concat){
  cache <- new.env(hash=T,parent=emptyenv(),size=20e3)
  return(function(...) {
    key <- key.fun(...)
    out <- cache[[key]]
    if (!is.null(out)) return(out)
    else {
      out <-FUN(...)
      cache[[key]]<-out
      return(out)
    } 
  })
} ##new.cache

time.cache <- function(FUN,...,key.fun=concat,trials=1e3){
  FUN.c <- new.cache(FUN,key.fun=key.fun)
  rbind(
        raw=system.time(replicate(trials,FUN(...))),
        cache=system.time(replicate(trials,FUN.c(...)))
        )
}##time.cache

get.cache <- function(cache){
  return(environment(cache)$cache)
}
