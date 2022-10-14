
isParetoOptimal <- function(x) {
  !apply(x,1,function(a) {
    any(apply(x,1,function(b) all(a<=b & !identical(a,b))))
  })
}