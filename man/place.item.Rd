\name{place.item}
\alias{place.item}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Place a spatial item in an existing plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
place.item(loc = NULL, item = scale.bar())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loc}{
Coordinates at which to place the item. If NA or NULL, the user will have to click on the plot.
}
  \item{item}{
The spatial object to place on the map, e.g. \code{\link{scale.bar}}, \code{\link{north.arrow}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The coordinates at which the item was placed. Can be used to set \code{loc} so that user-interaction is not subsequently required.
}
\seealso{
\code{\link{scale.bar}}, \code{\link{north.arrow}}, example in \link{mapping}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
