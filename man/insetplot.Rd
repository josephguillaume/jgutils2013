\name{insetplot}
\alias{insetplot}
\alias{panel.insetplot}
\alias{panel.insetplot1}

\title{Inset-plots}

\description{Draw inset plots, e.g. sparklines within a map}
\usage{
insetplot(x,data,...,locations)

panel.insetplot1(x,y,
  group.number=1,
  locations,
  panel.inset=panel.xyplot,
  w=unit(3,"char"),
  h=unit(1,"char"),
  just=c("right","centre"),
  vp=list(),
  ... ##args to panel.inset
  )

panel.insetplot(...,groups=NULL)
}
\arguments{
  \item{x}{For \code{insetplot}, a formula, as for
  \code{\link{xyplot}}. For \code{panel.insetplot} and
  \code{panel.insetplot1}, a vector of values to be used within an
  inset plot.}
  \item{data}{Dataframe}
  \item{locations}{Coordinates at which to draw inset plots. Dataframe
  or matrix with two columns; numeric vector of length 2. Use \code{groups} if
  there are several locations.}
\item{y}{A vector of values to be used within an inset plot}
\item{panel.inset}{A panel function to be used with x and y in an inset
  plot}
\item{w,h,just}{Width, height and justification of the inset plot. See
  \code{\link{viewport}} for details}
\item{vp}{A list of additional arguments for
  \code{\link{viewport}}. They take precedence of \code{w},\code{h} and
  \code{just}.}
\item{...}{For \code{insetplot} and \code{panel.insetplot1}, additional
  arguments to \code{panel.inset}. For \code{panel.insetplot}, arguments
  to \code{panel.insetplot1}, \code{panel.inset} and if \code{groups} is
  used, \code{\link{panel.superpose}}}

}


\details{

\code{insetplot} draws data in the formula
\code{x} from the dataframe \code{data} in \code{\link{viewport}}s
at given \code{locations} using the arbitrary panel function \code{panel.inset}.

Locations of data points are identified using \code{groups}. This is
linked to coordinates specified in \code{locations}. If a single
location is used, \code{groups} can be omitted.

N.B.  the levels of groups _must_ be in the same order as the locations
 
\code{panel.insetplot1} is the workhorse function. If there are
several locations (specified using \code{groups}),
\code{\link{panel.superpose}} is be used. \code{panel.insetplot} is
provided as a wrapper function to avoid this. \code{insetplot} is a
convenience wrapper around \code{\link{xyplot}}. Examples with and
without the convenience functions are included below.
  

Future changes:
\itemize{
  \item Automatically choose appropriate xlim,ylim using location
  \item Allow a \code{\link{SpatialPointsDataFrame}} for locations
  \item Allow more robust linking of data and locations using locations attributes
}
}

\examples{
library(lattice)
library(grid)

## Timeseries plot with inset map showing location of measurements

data(nhtemp)
library(maps)
data(us.cities)
data(usaMapEnv)

poly.usa <- map('usa',plot=F)

newhaven.x <- us.cities$long[grep("New Haven",us.cities$name)]
newhaven.y <- us.cities$lat[grep("New Haven",us.cities$name)]

summary(nhtemp)

## Just the time series
xyplot(nhtemp)

## Just the location map
panel.locationmap <- function(x,y,...){
  panel.lines(x,y,...,col="grey")
  panel.points(newhaven.x,newhaven.y,
               col="black",fill="black")
}

xyplot(y~x,poly.usa,panel=panel.locationmap)

## Insetplot of location map on timeseries

xyplot(nhtemp,
       xlab="Year",
       ylab="Mean annual temperature (fahrenheit)",
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.insetplot1(poly.usa$x,poly.usa$y,
                          locations=c(max(x),min(y)),
                          panel.inset=panel.locationmap,
                          just=c("right","bottom"),
                          w=unit(4,"cm"),
                          h=unit(3,"cm")
                          )
       })

################
## Random data examples

dd <- data.frame(
                 x=1:20,
                 y=runif(20),
                 id=1:4,
                 cat=1:2
                 )
dd <- dd[order(dd$id,dd$cat),]
loc <- data.frame(
                  x=runif(4),
                  y=runif(4)
                  )

## raw data
xyplot(y~x,dd)

## data locations
xyplot(y~x,loc)

################
## with several locations as groups
insetplot(y~x,data=dd,groups=id,
          locations=loc,
          xlim=c(0,1),
          ylim=c(0,1)
          )

## using xyplot directly with the general panel function
xyplot(y~x,data=dd,groups=id,
       panel=panel.insetplot,
       xlim=c(0,1),
       ylim=c(0,1),
       locations=loc)

## using xyplot directly with the superpose and
##   the single insetplot function
xyplot(y~x,data=dd,groups=id,
       panel=panel.superpose,
       panel.groups=panel.insetplot1,
       xlim=c(0,1),
       ylim=c(0,1),
       locations=loc)


################
## with a conditioning variable as well as groups
insetplot(y~x|cat,data=dd,groups=id,
          locations=loc,
          xlim=c(0,1),
          ylim=c(0,1)
          )

## using xyplot directly with the general panel function
xyplot(y~x|cat,data=dd,groups=id,
       panel=panel.insetplot,
       xlim=c(0,1),
       ylim=c(0,1),
       locations=loc)


################
## with only a single location
insetplot(y~x,data=dd,
          subset=id==1,
          locations=as.numeric(loc[1,]),
          xlim=c(0,1),
          ylim=c(0,1)
          )

## using xyplot directly with the general panel function
xyplot(y~x,data=dd,
       subset=id==1,
       panel=panel.insetplot1,
       xlim=c(0,1),
       ylim=c(0,1),
       locations=as.numeric(loc[1,])
       )

}