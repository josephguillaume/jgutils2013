\name{scale.bar}
\alias{scale.bar}
\title{
A simple scale bar
}
\description{
Creates a linear scale bar of given length and tic marks in a CRS and converts to an output CRS
}
\usage{
scale.bar(tics = c(0, 10), units = "km", multiplier.units.CRS = 1000, in.CRS = default.projection, out.CRS = NULL, height = NULL)
}
\arguments{
  \item{tics}{
numeric vector of location of tic marks. Values are also used as labels.
}
  \item{units}{
units to be shown on the scale bar
}
  \item{multiplier.units.CRS}{
The value which the \code{in.CRS} units have to be multiplied by to get to \code{units}
}
  \item{in.CRS}{
The CRS in which the scale bar is constructed
}
  \item{out.CRS}{
The CRS in which the scale bar will be displayed
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{north.arrow}}
\code{\link{place.item}}
example in \code{\link{mapping}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
