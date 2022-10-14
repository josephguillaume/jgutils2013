\name{makeGraph}
\alias{makeGraph}
\title{
Create graph of an integrated model
}
\description{
Visualise connections between components of an integrated model using a graphviz directed graph.
}
\usage{
makeGraph(name,edges,fun.names=NULL,exec=T)
}
\arguments{
  \item{name}{
name of the file to write graphviz dot representation to. Also used to create png file if \code{exec=T}
}
  \item{edges}{
string with connections between functions described in graphviz record style. See details.
}
  \item{fun.names}{
The functions to include in the graph, by default (if NULL) uses all the functions referenced in \code{edges}
}
  \item{exec}{
Whether to use execute Graphviz to make the png file and show it. Requires Graphviz to be installed and the program \code{dot} to be in the path. Displaying the image uses syntax for Windows XP+.
}
}
\details{
Use a variable in global environment, \code{outputs} or \code{pars} as the named argument to function
variable_name -> function_name:argument_name

Use output of a function as input to another function
function_name:function_name -> another_function_name:argument_name
}
\value{
Does not return a value
Creates a text file at path \code{name} and if \code{exec==T} a png file at path \code{name}.png
}
\references{
http://www.graphviz.org
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
