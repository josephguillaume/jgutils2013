\name{make-workflow}
\alias{workflow}
\alias{integrated model}
\title{
Workflow/Integrated model graphing and execution
}
\description{
Combine functions to make an integrated model, represent it as a graph and execute a model from a function graph description
}
\details{

  Edges represent passing of value as function input
  
  \code{\link{makeGraph}}
  Given a list of functions and the use of parameters between them, create a graphviz graph file
  
  \code{\link{iterModel}}
  Run an integrated model (combination of functions defined by the interaction of their inputs and outputs), for a number of iterations
  
  Uses:
  \code{parseCalls}
  Given graphviz formatted-edges, construct function calls
  
  \code{\link{getRunorder}}
  Obtain runorder of functions 
  Initial value
  
  runFun
  Run a function, looking for state variables, and storing output in \code{outputs}
  

Philosophy:
\itemize{
\item An integrated model is defined as a collection of functions that
are related by the flow of data between component inputs and outputs
\item An integrated model can be represented as a directed graph with
edges representing passing of values from variables and function outputs
to function inputs.
\item An integrated model is itself a function and can be run without
side-effects on the global environment
\item Inputs, parameters and system state are kept separated:
\item Inputs are a property of a function
\item Parameters are a list input to a function
\item System state is a collection of variables kept in an environment. It may
include global variables set outside the model. They can only be
accessed by a function if they are provided as an input. The output of
any function is considered a state variable.
\item All inputs and parameters of a function can be modified using its arguments
\item Current system state is defined within an environment from which
values are extracted to be used as inputs
}

Suggested guidelines for writing functions:
\itemize{
\item Inputs of a function are defined as named arguments
\item Any parameters used within the function should be defined within
a list passed as named argument \code{pars}.
\code{pars} is therefore a reserved argument name.
The easiest way to use parameters is to wrap the contents of the
function in a \code{\link{with}} block.
\item The parameters used in the function should be listed in the
default \code{pars} argument. If a default value is not appropriate for
a parameter, it should be marked \code{NA} or \code{NULL} and the function should
raise an error if it is used.
\item The output of the function should be atomic, i.e. can be
considered as a single result to be used as input. If this is not the
case, either create a wrapper function that extracts the required value,
or create a filter function in the integrated model that outputs only
the required value.
\item Specify the expected variable,dimensions and units in the units
attribute of a function.
\item If possible, allow scalar inputs to be vector valued to allow parallelisation
}
These are only best-practice suggestions that make the most of the
features of this package. There are no strict limitations on functions
that can be used.


Helper functions:
\code{feedTimestepData} to feed in data for each iteration.

To keep historical state data, pass the output of the necessary function
into a post-processing function that appends the current state to the history.

}
\author{
Joseph Guillaume
}
\seealso{
  \code{\link{makeGraph}}
  \code{\link{iterModel}}
}
\examples{
## Typical usage:
## Load functions
raise.to.power <- function(x,pars) with(pars,x^y)
## Constant inputs
runif.n <- 1
## Define graph
edges <- '
"runif.n" -> runif:n
runif:runif -> "raise.to.power":x
'
## Processing
temp <- tempfile()
makeGraph(temp,edges)
##unlink(sprintf("\%s.png",temp))
unlink(temp)
res <- iterModel(edges,3,init.state=list(),pars=list(y=3))
## Output
print(as.data.frame(res))
}