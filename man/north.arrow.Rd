\name{north.arrow}
\alias{north.arrow}
\title{
A simple north arrow
}
\description{
Creates a north arrow in a given projection
}
\usage{
north.arrow(in.CRS = default.projection)
}
\arguments{
  \item{in.CRS}{ The CRS of the output north arrow}
}
\value{
A SpatialLinesDataFrame suitable for use with \code{\link{place.item}}
}
\author{
Joseph Guillaume
}
\seealso{
\code{\link{scale.bar}}, \code{\link{place.item}}, example in \link{mapping}
}
\examples{
library(maptools)
data(wrld_simpl)
na <- north.arrow(wrld_simpl)
plot(na)
}