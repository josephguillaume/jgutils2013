
###################################
# BOM data
# Script originally created JG 2009-12-11 for EW
###################################


getProductPath<-function(product,station=NA){
  if (!exists("product.paths")) stop("named vector product.paths must be set to paths of csv files for rainfall,max,min (optionally with replacement string {STATION})")
  path<- product.paths[[product]]
  if(is.na(station) && regexpr("{STATION}",path)>0) stop("product.paths requires station, and station is NA")
  path <- gsub("{STATION}",station,path,fixed=T)	
  path
}

getBOMDaily <- function(product,station,all=F){
  rainfall.daily <- read.csv(getProductPath(product,station))
  rainfall.daily$date <- as.Date(with(rainfall.daily,paste(Year,Month,Day,sep="-")))
  if (!all){
    ## rainfall.daily <- with(rainfall.daily,Rainfall.amount..millimetres./Period.over.which.rainfall.was.measured..days.)
    rainfall.daily <- subset(rainfall.daily,select=c(Bureau.of.Meteorology.station.number,date,Rainfall.amount..millimetres.))
    names(rainfall.daily) <- c("station","date","rain")
  }
  rainfall.daily  
}##getBOMDaily


getBOMYears<-function(product,station=NA){
	bom<-read.csv(getProductPath(product,station),na.strings="null")
	return(subset(bom,select=c(Year,Annual)))
}#GetBOMYears

agg_months<-c(
	rainfall=function(data) return(rowSums(data)), #sum of rainfalls
	max=function(data) apply(data,1,mean), #mean of mean max. no weighting necessary
	min=function(data) apply(data,1,mean)	#as above
)
getBOMQuarters<-function(product,station=NA){
	agg_months<-agg_months[[product]]

	bom<-read.csv(getProductPath(product,station),na.strings="null")

	bom$useQ1<-agg_months(subset(bom,select=c(Jan,Feb,Mar)))
	bom$useQ2<-agg_months(subset(bom,select=c(Apr,May,Jun)))
	bom$useQ3<-agg_months(subset(bom,select=c(Jul,Aug,Sep)))
	bom$useQ4<-agg_months(subset(bom,select=c(Oct,Nov,Dec)))
	bom<-bom[,-c(4:16)]
	bom<-reshape(bom,dir="long",idvar="Year",timevar="Qtr",varying=c("useQ1","useQ2","useQ3","useQ4"),sep="Q")
	bom$Yr_Qtr<-with(bom,paste(Year,Qtr,sep="_"))
	return(bom)
}
#	str(get_bom_data("rainfall"))

mergeBOMQuarters<-function(data,products,station=NA){
	#Time-merge using Yr_Qtr
	# e.g. merge_bom(water_determinants[1:5,],c("rainfall","max"))
	for (product in products){
		bom<-subset(get_bom_data(product,station),select=c(Yr_Qtr,use))
		names(bom)[names(bom)=="use"]<-product
		data<-merge(data,bom,by="Yr_Qtr",all.x=TRUE,all.y=FALSE)
	}
	return(data)
}

getBOMMonths<-function(product,station=NA){
	bom<-read.csv(getProductPath(product,station),na.strings="null")
	names(bom)<-sapply(names(bom),function(x) ifelse(x %in% month.abb,paste("use",which(x==month.abb),sep="."),x))
	bom<-subset(bom,select=-c(Annual))
	bom<-reshape(bom,dir="long",idvar="Year",timevar="Month",varying=paste("use",1:12,sep="."))
	bom$Yr_Month<-with(bom,paste(Year,Month,sep="_"))
	return(bom)
}

#Time-merge using Yr_Month
# e.g. merge_bom(water_determinants[1:5,],c("rainfall","max"))
mergeBOMMonths<-function(data,products,station=NA){
	for (product in products){
		bom<-subset(get_bom_data_monthly(product,station),select=c(Yr_Month,use))
		names(bom)[names(bom)=="use"]<-product
		data<-merge(data,bom,by="Yr_Month",all.x=TRUE,all.y=FALSE)
	}
	return(data)
}

########################################
# DAYLIGHT
########################################

#http://mathforum.org/library/drmath/view/56478.html
#_Ecological Modeling_, volume 80 (1995) pp. 87-95, called "A Model Comparison for Daylength as a Function of Latitude and Day of the Year."
#   D = daylength
#   L = latitude -35.3 ~= Canberra
#   J = day of the year
daylight<-function(J=NA,d=NA,L=-35.3){
	if (is.na(d) & is.na(J)) stop("Neither day of the year, nor date provided")
	else if (is.na(J)) J<-as.numeric(format(d,"%j"))
	P = asin(.39795*cos(.2163108 + 2*atan(.9671396*tan(.00860*(J-186)))))
	D= 24 - (24/pi)*acos((sin(0.8333*pi/180) + sin(L*pi/180)*sin(P))/(cos(L*pi/180)*cos(P)))
	return(D)
}

total_daylight<-function(start,end){
	if (class(start)!="Date") start<-as.Date(start)
	if (class(end)!="Date") end<-as.Date(end)
	return(
		sum(
			sapply(as.numeric(format(start,"%j")):as.numeric(format(end,"%j")),
				function(x) daylight(J=x))))
}

quarters_daylight<-c(
	Q1=total_daylight("2009-01-01","2009-03-31"),
	Q2=total_daylight("2009-04-01","2009-06-30"),
	Q3=total_daylight("2009-07-01","2009-09-30"),
	Q4=total_daylight("2009-10-01","2009-12-31")
)
