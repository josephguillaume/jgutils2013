\name{iterModel}
\alias{iterModel}
\title{
Run an integrated model defined by its graph
}
\description{
Run a model defined by the flow of data through its component functions for \code{niter} iterations.
}
\usage{
iterModel(edges, niter, init.state = list(), pars, keep.timesteps = T, 
    return.runorder = F)
}
\arguments{
  \item{edges}{
string with connections between functions described in graphviz record style. See details.
}
  \item{niter}{
Number of iterations to run model for
}
\item{init.state}{
  List with named elements describing initial values of state
  variables.
  State variables are represented by the name of the function that
  produces them.
  Initial values must be specified when there are loops in
  the model. e.g. output of a is used as argument b of f, then
  list(a=init.val.of.a).
}
\item{pars}{
  List with parameter values
}
\item{keep.timesteps}{
  Either T,F, a character vector or a function.
  If \code{T}, keep state at every time step.
  If \code{F}, the whole state variable is kept at every iteration.
  If a character vector, uses a default function to extract the named
  state variables.
  The function list=f(list with named elements representing variables)
  should extract the required information from the list of state variables at each iteration.
}
\item{return.runorder}{
  Whether to return the order in which the functions have been run, as determined by \code{\link{getRunorder}}
}
}
\details{

  Guarantees that pars passed to functions are only those required, and
  are not NA or NULL.
}
\value{
list of class \code{imodel} with state (or result of \code{report.fun}) at every iteration
}
\references{
}
\author{
Joseph Guillaume
}
\seealso{
Summary of \code{\link{workflow}} tools
}
\examples{
## Not run.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
