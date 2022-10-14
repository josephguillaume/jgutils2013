\name{caching}
\alias{caching}
\alias{new.cache}
\alias{test.cache}
\title{
Caching of function evaluations
}
\description{
Cache results of function evaluations in an environment using an
arbitrary key function
}
\usage{
new.cache(FUN,key.fun=concat)
time.cache(FUN,...,key.fun=digest,trials=1e3)
get.cache(cache)
concat(...)
}
\arguments{
  \item{FUN}{
    function for which to cache results
  }
  \item{key.fun}{
    function f(...) that returns a string suitable for use as a variable name:
    the key for a particular set of inputs to the function.
    \code{concat} uses paste to collapse a vector input using spaces
    \code{digest} in the \code{digest} package is more general, but
    incurs a larger overhead.
  }
  \item{...}{
    inputs to \code{FUN}
  }
  \item{trials}{
    number of replications of function evaluations/cache retrievals for
    timing of cache vs function evaluation speed
  }
  \item{cache}{
    a caching function, as returned by \code{new.cache}, for which to
    get the environment used as a cache
  }
}
\details{
  Caching is only worthwhile if:
  \itemize{
    \item it takes less time to retrieve values from cache then to
    recalculate them
    \item precise inputs are re-evaluated sufficiently often
    \item significant time is spent within the function that is cached
  }

  Use \code{time.cache} as a simple way to check whether caching can
  bring any speed gain for a representative case.

  \code{get.cache} can be used to examine function evaluations stored
  within the environment, if its key is known

}
\value{
  \code{new.cache} returns a wrapper function for \code{FUN} that
  retrieves values from its internal cache when possible, otherwise
  evaluating \code{FUN} and caching the result.

  \code{time.cache} returns a matrix showing the timings for running \code{FUN}
  through a cache and without one.

  \code{get.cache} returns an \code{\link{environment}} containing all
  function evaluations stored by their key, as calculated using \code{key.fun}
}
\author{
Joseph Guillaume
}
\seealso{
  Using \code{new.cache} to wrap evaluation functions may provide a significant
  speed gain for \code{\link{pavingBounds}}.
}
\examples{
## Time-expensive function
fun <- function(x) {
 Sys.sleep(1e-3)
 x
}
## Compare times using cached and non-cached function evaluations
time.cache(fun,trials=1e2)

## Wrap fun with a cache
c.fun <- new.cache(fun)

## Normal function call
fun(2)
## The wrapped function is called in the same way as the normal, and result is transparently cached
c.fun(2)
## Result is now drawn from cache
c.fun(2)

## List contents of the cache
ls(get.cache(c.fun))

## Manually extract cached result: the function evaluation that has the key "2"
get.cache(c.fun)[["2"]]
}