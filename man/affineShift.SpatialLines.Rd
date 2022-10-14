\name{affineShift.SpatialLines}
\alias{affineShift.SpatialLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Affine shift of a spatial object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
affineShift.SpatialLines(obj, offset, scale = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(obj,offset,scale=1,...){
  stopifnot(class(obj) \%in\% c("SpatialLines","SpatialLinesDataFrame"))
  
  if (length(offset) != 2) 
    stop("offset should have length 2")
  if (is.list(offset)) 
    offset = c(offset[[1]], offset[[2]])
  if (length(scale) == 1) 
    scale = rep(scale, 2)
      
  for (l1 in 1:length(obj@lines)){
    for (l2 in 1:length(slot(obj@lines[[l1]],"Lines"))){
      cc <- slot(obj@lines[[l1]],"Lines")[[l2]]@coords
      slot(obj@lines[[l1]],"Lines")[[l2]]@coords[,1] <- scale[1]*cc[,1]+offset[1]
      slot(obj@lines[[l1]],"Lines")[[l2]]@coords[,2] <- scale[2]*cc[,2]+offset[2]
    } ##l2
  }   ##l1

  return(obj)
  }##affineShift
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
