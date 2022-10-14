
months.ts <- function(obj,text=T){
  aaa <- time(obj)
  aaa <- as.numeric(round((aaa-trunc(aaa))*12)+1)
  if(text) aaa <- factor(format(as.Date(paste("2000",aaa,"01",sep="-")),"%b"),
                         levels=format(as.Date(paste("2000",1:12,"01",sep="-")),"%b"),
                         ordered=T
                         )
  return(aaa)
}##months.ts


seasons<-c(rep("Summer",2),rep("Autumn",3),rep("Winter",3),rep("Spring",3),"Summer")

#Return character year from date
#Original scalar function from building_approvals.R
#Made into a vector function here
GetAlternateYear<-function(date,first.month=9){
	if (class(date)!="Date") date<-as.Date(date)
	altyear<-rep(NA,length(date))
	year<-as.numeric(format(date,"%Y"))

	wanted<-which(as.numeric(format(date,"%m"))<first.month)
	if (length(wanted)>0) altyear[wanted]<-sprintf("%d-%d",year[wanted]-1,year[wanted])

	wanted<-which(as.numeric(format(date,"%m"))>=first.month)
	if (length(wanted)>0) altyear[wanted]<-sprintf("%d-%d",year[wanted],year[wanted]+1)

	return(altyear)
}
GetFinancialYear<-function(date) GetAlternateYear(date,7)
GetAlternateYearDays<-function(year.start,first.month=9){

	if (class(year.start)=="character") year.start<-as.numeric(strsplit(year.start,"-")[[1]][1])

	year.end<-year.start+1
	days<-seq(
		as.Date(sprintf("%d-%d-01",year.start,first.month)),
		as.Date(sprintf("%d-%d-01",year.end,first.month)),
		by=1
	)
	days<-days[-length(days)]
	return(days)
}


GetQuarterLengths<-function(year.start){
	days<-GetAlternateYearDays(year.start,first.month=1)
	return(table(quarters(days)))
}#GetSeasonLength

GetMonthLengths<-function(year.start,first.month=9){

	days<-GetAlternateYearDays(year.start,first.month=first.month)

	return(table(
		factor(as.numeric(format(days,"%m")))
	))

}#GetMonthLengths
