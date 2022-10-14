\name{pavingBounds}
\alias{pavingBounds}
\alias{plot.bounds_paving}
\alias{test.vertices}
\alias{test.bounds}
\alias{eval.interval}
\alias{eval.intervalD}
\alias{eval.intervalO}
\title{
  Bounds by paving
}
\description{
Determine set membership and boundary of real subspace by creating a
rectangular paving
}
\usage{
pavingBounds(box.init,incl.fn,tol=1e-1,plot.vars=NA,...)
}
\arguments{
  \item{box.init}{Initial "box" - list of bounds for each dimension}
  \item{incl.fn}{Function to test set membership. Must return 1 for definite
    membership, -1 for non-membership, 0 for indeterminate (boundary)
    case.
    \code{\{-1,0,1\}=incl.fn(box,...)}
  }
  \item{tol}{Termination condition - maximum width of boxes}
  \item{plot.vars}{Numeric vector of length 2 indicating which
    dimensions to plot progress for. If \code{NA} (the default), don't
    plot.
  }
  \item{...}{Extra arguments to \code{incl.fn}}
}
\details{
  Several functions are provided for use with \code{incl.fn}:
  \itemize{
    \item{\code{test.vertices(box,boolean.fn)}}{Test each corner of a
      box by calling \code{boolean.fn} on each combination of dimension bounds. Will not work
      if all corners of \code{box.init} are in the same set (i.e. it
      must be a boundary case)}
    \item{\code{test.bounds(intervals,bounds)}}{Check whether
      \code{intervals} are in \code{bounds}. For use within custom
      \code{incl.fn}s for interval analysis (see examples)}
    \item{Interval analysis functions}{
      R does not currently have interval analysis capabilities, so three
      (sub-optimal) interchangeable solutions are provided:
      
      \code{eval.interval(fun,box,...)} uses \code{\link{dsevalmc}} from
      the package \code{ipptoolbox}. Doesn't seem to work as expected?

      eval.intervalD finds the maximum and minimum by discretising each
      dimension into ten parts. The number of function evaluations
      required will be \code{10^(number of dimensions)}.
      
      eval.intervalO finds the maximum and minimum by using the
      "L-BFGS-B" method in \code{\link{optim}}
    }
  }
  
  There is a method for \code{plot} for two dimensions. Specify \code{plot.vars=c(which.two.dim)}

Future plans: make use of true interval analysis functions; generalise to description in Jaulin & Walter, 1993
}
\value{
  object of class \code{bounds_paving} -  list of boxes (list of dimension-wise bounds)
}
\references{
This function follows the algorithm described in:
  Moore, R. (1992). "Parameter sets for bounded-error data."
  Math. Comput. Simul. 34(2):
  113-119. \url{http://dx.doi.org/10.1016/0378-4754(92)90048-l}

Also see:
  Jaulin, L. and E. Walter (1993). "Guaranteed nonlinear parameter
  estimation from bounded-error data via interval analysis." Mathematics
  and Computers in Simulation 35(2): 123-137. \url{http://dx.doi.org/10.1016/0378-4754(93)90008-I}
}
\examples{
## N.B. doesn't work if all vertices of box.init are within area
egbool <- function(x) x[1]>-6 && x[2]<3
paving.bool <- pavingBounds(box.init=list(c(-10,10),c(-10,10)),
                            incl.fn=test.vertices,
                            boolean.fn=egbool,
                            plot.vars=c(1,2)
                            )

plot(paving.bool,plot.vars=c(1,2))

## moore-1992. p5 Example 3.2
## TODO: requires proper interval arithmetic
ym <- function(m,k) (0.25*cos(m[1])+1.25)*cos(0.5*m[2]*k)
y <- 0.1
fn.e <- function(m,k) y-ym(m,k)
paving <- pavingBounds(box.init=list(c(0,pi),c(0,2*pi)),
                       incl.fn=function(box,...){
                         e <- sapply(c(1,2),function(k) eval.intervalO(fn.e,box,k=k))
                         test.bounds(e,bounds=c(-0.75,0.75))
                       },
                       tol=5e-2
                       ,plot.vars=c(1,2))

plot(paving,plot.vars=c(1,2))
                       
## moore-1992 p5 Example 3.3
ts <- seq(0,510,by=30)
ym <- function(t,m=c(1,0.01,1,0.005,1,0.001)) m[1]*exp(-m[2]*t)+m[3]*exp(-m[4]*t)+m[5]*exp(-m[6]*t)
y <- sapply(ts,ym)
fn.e <- function(t,m) y[ts==t]-ym(t,m)
paving <- pavingBounds(
                       box.init=list(
                         c(-5,5),
                         c(0.0072,0.014),
                         c(-5,5),
                         c(0.0025,0.0086),
                         c(0.53,1.52),
                         c(0.0004,0.0016)
                         ),
                       incl.fn=function(box,...) {
                         e <- sapply(ts,function(t) eval.intervalO(fn.e,box,t=t))
                         test.bounds(intervals=e,bounds=c(-0.03,0.03))
                       }
                       ,plot.vars=c(1,3),
                       tol=0.6
                       )

plot(paving,plot.vars=c(1,3))
## TODO: slightly different to paper
}