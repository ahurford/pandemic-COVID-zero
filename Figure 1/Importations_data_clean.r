library(ggplot2)
library(patchwork)
library(readxl)
library(dplyr)

### These are the commands that were used to construct the dataset, but they are now commented out to
### Just call a smaller cleaned dataset

# This is to pull the data a copy of the PHAC data for active cases
# This is the province-level data
#PHAC.data <- read.csv('https://raw.githubusercontent.com/francisanokye/Pandemic-Modelling---COVID19/master/data/covid19-download.csv', fill = TRUE)
PHAC.data <- read.csv('https://raw.githubusercontent.com/ahurford/covid-nl/master/covid19-download.csv')

## These datasets are inidividual-level from the COVID-19 Canada Open data working group. They give travel-related
# cases. The are large files. I will clean and archive just the travel-related cases.
#These individual-level data files are too large to share
CCODWG.2020=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2020.csv', fill=TRUE)
CCODWG.2021=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2021_1.csv', fill = TRUE)
CCODWG.2021b=read.csv('https://raw.githubusercontent.com/ishaberry//Covid19Canada/master/retired_datasets/individual_level/cases_2021_2.csv', fill = TRUE)

# Correcting some syntax inconsistencies
CCODWG.2020$locally_acquired[CCODWG.2020$locally_acquired =="Close contact"] = "Close Contact"
CCODWG.2020$locally_acquired[CCODWG.2020$locally_acquired =="close contact"] = "Close Contact"
CCODWG.2021$locally_acquired[CCODWG.2021$locally_acquired =="Close contact"] = "Close Contact"
CCODWG.2021$locally_acquired[CCODWG.2021$locally_acquired =="close contact"] = "Close Contact"
CCODWG.2021$locally_acquired[CCODWG.2021$locally_acquired =="Close Contact "] = "Close Contact"
CCODWG.2021b$locally_acquired[CCODWG.2021b$locally_acquired =="Close contact"] = "Close Contact"
CCODWG.2021b$locally_acquired[CCODWG.2021b$locally_acquired =="close contact"] = "Close Contact"
CCODWG.2020$travel_history_country[CCODWG.2020$travel_history_country =="Not Reported "] = "Not Reported"
CCODWG.2021$travel_history_country[CCODWG.2021$travel_history_country =="Not Repoted"] = "Not Reported"
CCODWG.2021$travel_history_country[CCODWG.2021$travel_history_country =="Not reported"] = "Not Reported"
i = which(CCODWG.2021$travel_history_country =="Close contact")
CCODWG.2021 = CCODWG.2021[-i,]

# Atlantic Canada and territories specific data extraction
importations=function(province){
  travel.data.2020 <- CCODWG.2020[CCODWG.2020$province==province,]
  travel.data.2021 <- CCODWG.2021[CCODWG.2021$province==province,]
  travel.data.2021b <- CCODWG.2021b[CCODWG.2021b$province==province,]
  travel.data = rbind(travel.data.2020,travel.data.2021, travel.data.2021b)
  travel.data$date_report=as.Date(travel.data$date_report,format="%d-%m-%Y")
  # Only travel-related
  travel = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired!="Close Contact",]
  travel = select(travel,date_report,report_week,travel_yn,locally_acquired)%>%
    group_by(report_week)%>%
    add_tally()%>%
    rename("travel"="n")%>%
    select(report_week,travel)%>%
    distinct()
  # Close contacts only
  contacts = travel.data[travel.data$travel_yn!=1 & travel.data$locally_acquired=="Close Contact",]
  contacts = select(contacts,date_report,report_week,travel_yn,locally_acquired)%>%
    group_by(report_week)%>%
    add_tally()%>%
    rename("contacts"="n")%>%
    select(report_week,contacts)%>%
    distinct()
  # Travel and close contact:There is no data of this type - all close contacts are coded as not travel-related.
  travel.contacts = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired=="Close Contact",]$date_report
  # Total new cases
  
  
  travel2=full_join(travel,contacts)

  
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
#Nunavut has no travel-related cases reported
NWT.travel = importations("NWT")
NWT.travel[67:69,2:5]=rep(NA,12)



# Weekly active cases in all provinces per 10K people
# province sizes based on
AB.active <- 1e4*CCODWG[CCODWG$province=="Alberta",6]/4067175
BC.active <- 1e4*CCODWG[CCODWG$province=="BC",6]/4648055
SK.active <- 1e4*CCODWG[CCODWG$province=="Saskatchewan",6]/1098352
MB.active <- 1e4*CCODWG[CCODWG$province=="Manitoba",6]/1278365
ON.active <- 1e4*CCODWG[CCODWG$province=="Ontario",6]/13448494
# In QC there is a change in how active cases are reported on July 17, 2020.
QC.active <- CCODWG[CCODWG$province=="Quebec",]
change.index = which(QC.active$active_cases_change==min(QC.active$active_cases_change))
QC.scale = QC.active[change.index,]$active_cases/QC.active[change.index-1,]$active_cases
# Rescale active cases prior to July 17, 2020:
QC.active$active_cases[1:change.index-1] <- QC.scale*QC.active$active_cases[1:change.index-1]
QC.active = 1e4*QC.active$active_cases/8164361
NS.active <- 1e4*CCODWG[CCODWG$province=="Nova Scotia",6]/923598
PEI.active <- 1e4*CCODWG[CCODWG$province=="PEI",6]/142907
NB.active <- 1e4*CCODWG[CCODWG$province=="New Brunswick",6]/747101
NL.active <- 1e4*CCODWG[CCODWG$province=="NL",6]/519716
NV.active <- 1e4*CCODWG[CCODWG$province=="Nunavut",6]/35944
NWT.active <- 1e4*CCODWG[CCODWG$province=="NWT",6]/41786
YT.active <- 1e4*CCODWG[CCODWG$province=="Yukon",6]/35874


Canada.data = data.frame(date=as.Date(CCODWG[CCODWG$province=="Alberta",2],format="%d-%m-%Y"),AB = AB.active,BC=BC.active,SK=SK.active,MB=MB.active,ON=ON.active,QC=QC.active,NS=NS.active,NB=NB.active,PEI=PEI.active,NL=NL.active,YT=YT.active,NV=NV.active,NWT=NWT.active, NL.new = NL.new, NS.new = NS.new, NB.new = NB.new, PEI.new = PEI.new, YT.new = YT.new, NWT.new = NWT.new)
week = cut(as.Date(Canada.data$date), "week")
Canada.data <- cbind(Canada.data,week)
Canada.week = aggregate(Canada.data[,2:20], by = list(Canada.data$week),FUN = sum)
Canada.week = data.frame(week = Canada.week[,1],Canada.week[,2:20])
Canada.week$week = as.Date(Canada.week$week)
# Begin the week of March 2, 2021 (remove first 6 weeks)
Canada.week <- tail(Canada.week,-6)



# Data set assembled by Bilal:
NB.Bilal <- read_excel("~/MUN/Amy_Paper/New_travel_cases_by_reported_date.csv")
NB.Bilal <- as.data.frame(NB.Bilal)
week1 = cut(as.Date(NB.Bilal$Date), "week")
NB.Bilal <- cbind(NB.Bilal,week1)
Bilal.week = aggregate(NB.Bilal[,2], by = list(NB.Bilal$week1),FUN = sum)
Bilal.week = data.frame(week = Bilal.week[,1],cases=Bilal.week[,2])
Bilal.week$week = as.Date(Bilal.week$week)


NB2 = rep(NA, length(Canada.week[,1]))
NB2[44:69] = Bilal.week[,2]
Canada.week = data.frame(Canada.week, NL.dom = NL.travel$domestic, NL.int = NL.travel$international, NL.nr = NL.travel$not.reported, NS.dom = NS.travel$domestic, NS.int = NS.travel$international, NS.nr = NS.travel$not.reported,PEI.dom = PEI.travel$domestic, PEI.int = PEI.travel$international,PEI.nr = PEI.travel$not.reported, NB.dom = NB.travel$domestic, NB.int=NB.travel$international, NB.nr = NB.travel$not.reported,YT.dom = YT.travel$domestic, YT.int = YT.travel$international,YT.nr = YT.travel$not.reported, NWT.dom = NWT.travel$domestic, NWT.int = NWT.travel$international,NWT.nr = NWT.travel$not.reported,NB2=NB2)
# write.csv(Canada.week,"~/Desktop/Work/Research/Research_Projects/2021/Reopening/reopening/Hurford/Canada_week.csv")

contacts.week = data.frame(week = NL.travel$week, NL = NL.travel$close.contact, NS = NS.travel$close.contact, PEI = PEI.travel$close.contact, NB = NB.travel$close.contact, NWT = NWT.travel$close.contact, YT = YT.travel$close.contact)
# write.csv(contacts.week,"~/Desktop/Work/Research/Research_Projects/2021/Reopening/reopening/Hurford/contacts_week.csv")
