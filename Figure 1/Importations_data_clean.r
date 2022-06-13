library(ggplot2)
library(patchwork)
library(readxl)

### These are the commands that were used to construct the dataset, but they are now commented out to
### Just call a smaller cleaned dataset

# This is to pull the data from the open data working group
# This is the province-level data 
COVID.data <- read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/timeseries_prov/active_timeseries_prov.csv', fill = TRUE)

# Data set assembled by Bilal:
NB.Bilal <- read_excel("~/MUN/Amy_Paper/New_travel_cases_by_reported_date.csv")
NB.Bilal <- as.data.frame(NB.Bilal)
week1 = cut(as.Date(NB.Bilal$Date), "week")
NB.Bilal <- cbind(NB.Bilal,week1)
Bilal.week = aggregate(NB.Bilal[,2], by = list(NB.Bilal$week1),FUN = sum)
Bilal.week = data.frame(week = Bilal.week[,1],cases=Bilal.week[,2])
Bilal.week$week = as.Date(Bilal.week$week)

## These datasets that give travel-related cases are time consuming to pull - 
### I will make a cleaned .csv later in the code
#Pull and write the data to Reopening folder
#These individual-level data files are too large to share
COVID.data.2020=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2020.csv', fill=TRUE)
COVID.data.2021=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2021_1.csv', fill = TRUE)
COVID.data.2021b=read.csv('https://raw.githubusercontent.com/ishaberry//Covid19Canada/master/retired_datasets/individual_level/cases_2021_2.csv', fill = TRUE)

#write.csv(COVID.data.2020,"~/Desktop/Work/Research/Research_Projects/2021/Reopening/COVIDdata2020.csv")
#write.csv(COVID.data.2021,"~/Desktop/Work/Research/Research_Projects/2021/Reopening/COVIDdata2021.csv")
#write.csv(COVID.data.2021b,"~/Desktop/Work/Research/Research_Projects/2021/Reopening/COVIDdata2021b.csv")

# Individual-level datasets to recover the number of travel-related cases.
 
# Correct inconsistency on "Close contact" and "close contact" => Close Contact"
COVID.data.2020$locally_acquired[COVID.data.2020$locally_acquired =="Close contact"] = "Close Contact"
COVID.data.2020$locally_acquired[COVID.data.2020$locally_acquired =="close contact"] = "Close Contact"
#
COVID.data.2021$locally_acquired[COVID.data.2021$locally_acquired =="Close contact"] = "Close Contact"
COVID.data.2021$locally_acquired[COVID.data.2021$locally_acquired =="close contact"] = "Close Contact"
COVID.data.2021$locally_acquired[COVID.data.2021$locally_acquired =="Close Contact "] = "Close Contact"
#
COVID.data.2021b$locally_acquired[COVID.data.2021b$locally_acquired =="Close contact"] = "Close Contact"
COVID.data.2021b$locally_acquired[COVID.data.2021b$locally_acquired =="close contact"] = "Close Contact"
#
# Same for country of origin
COVID.data.2020$travel_history_country[COVID.data.2020$travel_history_country =="Not Reported "] = "Not Reported"
COVID.data.2021$travel_history_country[COVID.data.2021$travel_history_country =="Not Repoted"] = "Not Reported"
COVID.data.2021$travel_history_country[COVID.data.2021$travel_history_country =="Not reported"] = "Not Reported"
i = which(COVID.data.2021$travel_history_country =="Close contact")
COVID.data.2021 = COVID.data.2021[-i,]

# An extra row gets added when doing the csv write and subsequent read
#COVID.data.2020[,1]<-NULL
#COVID.data.2021[,1]<-NULL
#COVID.data.2021b[,1]<-NULL

# Remove data after June 28, 2021
COVID.data = COVID.data[as.Date(COVID.data$date_active, format = "%d-%m-%Y") <= as.Date("27-06-2021",format="%d-%m-%Y"), ]

# Weekly new cases in all provinces - originally this was done on new cases then switched to active cases
AB.new <- diff(c(0,COVID.data[COVID.data$province=="Alberta",3]))
BC.new <- diff(c(0,COVID.data[COVID.data$province=="BC",3]))
SK.new <- diff(c(0,COVID.data[COVID.data$province=="Saskatchewan",3]))
MB.new <- diff(c(0,COVID.data[COVID.data$province=="Manitoba",3]))
ON.new <- diff(c(0,COVID.data[COVID.data$province=="Ontario",3]))
QC.new <- diff(c(0,COVID.data[COVID.data$province=="Quebec",3]))
NS.new <- diff(c(0,COVID.data[COVID.data$province=="Nova Scotia",3]))
PEI.new <- diff(c(0,COVID.data[COVID.data$province=="PEI",3]))
NB.new <- diff(c(0,COVID.data[COVID.data$province=="New Brunswick",3]))
NL.new <- diff(c(0,COVID.data[COVID.data$province=="NL",3]))
NV.new <- diff(c(0,COVID.data[COVID.data$province=="Nunavut",3]))
NWT.new <- diff(c(0,COVID.data[COVID.data$province=="NWT",3]))
YT.new <- diff(c(0,COVID.data[COVID.data$province=="Yukon",3]))

# Weekly active cases in all provinces per 10K people
# province sizes based on 
AB.active <- 1e4*COVID.data[COVID.data$province=="Alberta",6]/4067175
BC.active <- 1e4*COVID.data[COVID.data$province=="BC",6]/4648055
SK.active <- 1e4*COVID.data[COVID.data$province=="Saskatchewan",6]/1098352
MB.active <- 1e4*COVID.data[COVID.data$province=="Manitoba",6]/1278365
ON.active <- 1e4*COVID.data[COVID.data$province=="Ontario",6]/13448494
# In QC there is a change in how active cases are reported on July 17, 2020.
QC.active <- COVID.data[COVID.data$province=="Quebec",]
change.index = which(QC.active$active_cases_change==min(QC.active$active_cases_change))
QC.scale = QC.active[change.index,]$active_cases/QC.active[change.index-1,]$active_cases
# Rescale active cases prior to July 17, 2020:
QC.active$active_cases[1:change.index-1] <- QC.scale*QC.active$active_cases[1:change.index-1]
QC.active = 1e4*QC.active$active_cases/8164361
NS.active <- 1e4*COVID.data[COVID.data$province=="Nova Scotia",6]/923598
PEI.active <- 1e4*COVID.data[COVID.data$province=="PEI",6]/142907
NB.active <- 1e4*COVID.data[COVID.data$province=="New Brunswick",6]/747101
NL.active <- 1e4*COVID.data[COVID.data$province=="NL",6]/519716
NV.active <- 1e4*COVID.data[COVID.data$province=="Nunavut",6]/35944
NWT.active <- 1e4*COVID.data[COVID.data$province=="NWT",6]/41786
YT.active <- 1e4*COVID.data[COVID.data$province=="Yukon",6]/35874


Canada.data = data.frame(date=as.Date(COVID.data[COVID.data$province=="Alberta",2],format="%d-%m-%Y"),AB = AB.active,BC=BC.active,SK=SK.active,MB=MB.active,ON=ON.active,QC=QC.active,NS=NS.active,NB=NB.active,PEI=PEI.active,NL=NL.active,YT=YT.active,NV=NV.active,NWT=NWT.active, NL.new = NL.new, NS.new = NS.new, NB.new = NB.new, PEI.new = PEI.new, YT.new = YT.new, NWT.new = NWT.new)
week = cut(as.Date(Canada.data$date), "week")
Canada.data <- cbind(Canada.data,week)
Canada.week = aggregate(Canada.data[,2:20], by = list(Canada.data$week),FUN = sum)
Canada.week = data.frame(week = Canada.week[,1],Canada.week[,2:20])
Canada.week$week = as.Date(Canada.week$week)
# Begin the week of March 2, 2021 (remove first 6 weeks)
Canada.week <- tail(Canada.week,-6)

########
# Travel-related cases
importations=function(province){
travel.data.2020 <- COVID.data.2020[COVID.data.2020$province==province,]
travel.data.2021 <- COVID.data.2021[COVID.data.2021$province==province,]
travel.data.2021b <- COVID.data.2021b[COVID.data.2021b$province==province,]
travel.data = rbind(travel.data.2020,travel.data.2021, travel.data.2021b)
travel.data$date_report=as.Date(travel.data$date_report,format="%d-%m-%Y")
# Only travel-related
travel = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired!="Close Contact",]
# Close contacts only
contacts = travel.data[travel.data$travel_yn!=1 & travel.data$locally_acquired=="Close Contact",]$date_report
# Close contacts and travel-related are mutually exclusive for all 
travel.contacts = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired=="Close Contact",]$date_report
# Travel and close contact

### Need to fix up here
# Canada
travel.Canada = travel$date_report[travel$travel_history_country=="Canada"]
travel.Not.Reported = travel$date_report[travel$travel_history_country=="Not Reported"]
travel.Int = travel$date_report[travel$travel_history_country!="Not Reported" & travel$travel_history_country != "Canada"]
week = cut(as.Date(travel.Canada), "week")
travel.Canada = data.frame(date = travel.Canada, number = rep(1,length(travel.Canada)),week=as.Date(week))
domestic = aggregate(travel.Canada$number, by = list(travel.Canada$week),FUN = sum)

# International
# No international reported for NWT
if(province!="NWT"){
week = cut(as.Date(travel.Int), "week")
travel.Int = data.frame(date = travel.Int, number = rep(1,length(travel.Int)),week=as.Date(week))
international = aggregate(travel.Int$number, by = list(travel.Int$week),FUN = sum)
}

# Not reported
week = cut(as.Date(travel.Not.Reported), "week")
travel.Not.Reported = data.frame(date = travel.Not.Reported, number = rep(1,length(travel.Not.Reported)),week=as.Date(week))
Not.Reported = aggregate(travel.Not.Reported$number, by = list(travel.Not.Reported$week),FUN = sum)

travel = data.frame(week = Canada.week$week, domestic = rep(0,length(Canada.week$week)), international = rep(0,length(Canada.week$week)), close.contact = rep(0,length(Canada.week$week)), not.reported = rep(0,length(Canada.week$week)))
# Close contacts
week = cut(as.Date(contacts), "week")
contacts = data.frame(date = contacts, number = rep(1,length(contacts)),week=as.Date(week))
contacts.data = aggregate(contacts$number, by = list(contacts$week),FUN = sum)

# Match with the weeks for Canada cases
if(province!="NWT"){
for(i in seq(length(international[,1]))){
  loc = which(Canada.week$week==international$Group.1[i])
  travel$international[loc] = international$x[i]
}
}


for(i in seq(length(domestic[,1]))){
  loc = which(Canada.week$week==domestic$Group.1[i])
  travel$domestic[loc] = domestic$x[i]
}


  for(i in seq(length(contacts.data[,1]))){
    loc = which(Canada.week$week==contacts.data$Group.1[i])
    travel$close.contact[loc] = contacts.data$x[i]
  }


  for(i in seq(length(Not.Reported[,1]))){
    loc = which(Canada.week$week==Not.Reported$Group.1[i])
    travel$not.reported[loc] = Not.Reported$x[i]
  }


return(travel)
}

NL.travel = importations("NL")
# No values after May 31
NL.travel[67:69,2:5]=rep(NA,12)
NS.travel = importations("Nova Scotia")
NS.travel[67:69,2:5]=rep(NA,12)
YT.travel = importations("Yukon")
YT.travel[67:69,2:5]=rep(NA,12)
NB.travel = importations("New Brunswick")
NB.travel[67:69,2:5]=rep(NA,12)
PEI.travel = importations("PEI")
PEI.travel[67:69,2:5]=rep(NA,12)
#Nunavut has not travel-related cases reported
NWT.travel = importations("NWT")
NWT.travel[67:69,2:5]=rep(NA,12)
NB2 = rep(NA, length(Canada.week[,1]))
NB2[44:69] = Bilal.week[,2]
Canada.week = data.frame(Canada.week, NL.dom = NL.travel$domestic, NL.int = NL.travel$international, NL.nr = NL.travel$not.reported, NS.dom = NS.travel$domestic, NS.int = NS.travel$international, NS.nr = NS.travel$not.reported,PEI.dom = PEI.travel$domestic, PEI.int = PEI.travel$international,PEI.nr = PEI.travel$not.reported, NB.dom = NB.travel$domestic, NB.int=NB.travel$international, NB.nr = NB.travel$not.reported,YT.dom = YT.travel$domestic, YT.int = YT.travel$international,YT.nr = YT.travel$not.reported, NWT.dom = NWT.travel$domestic, NWT.int = NWT.travel$international,NWT.nr = NWT.travel$not.reported,NB2=NB2)
# write.csv(Canada.week,"~/Desktop/Work/Research/Research_Projects/2021/Reopening/reopening/Hurford/Canada_week.csv")

contacts.week = data.frame(week = NL.travel$week, NL = NL.travel$close.contact, NS = NS.travel$close.contact, PEI = PEI.travel$close.contact, NB = NB.travel$close.contact, NWT = NWT.travel$close.contact, YT = YT.travel$close.contact)
# write.csv(contacts.week,"~/Desktop/Work/Research/Research_Projects/2021/Reopening/reopening/Hurford/contacts_week.csv")
