# Title: Data cleaning
# Date: June 14, 2022
# Description: Produces data files travel-related cases per week to Atlantic
#    Canada & territories from the COVID-19 Canada Open Data Working Group (CCODWG)
#    Appends validation data for NB and NL
#    Uses PHAC data source to record active cases per 10K people
#    CCODWG files are large
#======================

library(dplyr)

## PULLING THE DATA FILES
# This is to pull the data a copy of the PHAC data for active cases
PHAC.data <- read.csv('https://raw.githubusercontent.com/ahurford/covid-nl/master/covid19-download.csv')
# This adds the report week to the PHAC data so that it is consistent with CCODWG
PHAC.data <- select(PHAC.data,date,numtoday,numactive,prname)%>%
  mutate(report_week = as.Date(cut(as.Date(date),"week", start.on.monday = F)))%>%
  distinct()
## These datasets are inidividual-level from the COVID-19 Canada Open data working group. They give travel-related
# cases. The are large files.
CCODWG.2020=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2020.csv', fill=TRUE)
CCODWG.2021=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2021_1.csv', fill = TRUE)
CCODWG.2021b=read.csv('https://raw.githubusercontent.com/ishaberry//Covid19Canada/master/retired_datasets/individual_level/cases_2021_2.csv', fill = TRUE)
# These are data files obtained by the researchers
# Append report week
NL.travel.2 = read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL-travel.csv', fill=TRUE)%>%
  mutate(report_week = as.Date(cut(as.Date(REPORTED_DATE),"week", start.on.monday = F)))%>%
  distinct()
NL.travel.2.daily = select(NL.travel.2, REPORTED_DATE, TRAVEL)
NB.travel.2 = read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NB-travel.csv', fill=TRUE)[,1:2]
NB.travel.2$Date = format(as.Date(NB.travel.2$Date, format = "%m/%d/%y"),"%Y-%m-%d")
NB.travel.2 = NB.travel.2%>%
  mutate(report_week = as.Date(cut(as.Date(Date),"week", start.on.monday = F)))%>%
  distinct()
NB.travel.2.daily = select(NB.travel.2, Date, Travel.related.Cases)


## TRAVEL-RELATED CASES FROM CCODWG
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
  travel.data$date_report=format(as.Date(travel.data$date_report, format = "%d-%m-%Y"),"%Y-%m-%d")
  travel.data$report_week = format(as.Date(travel.data$report_week, format = "%d-%m-%Y"),"%Y-%m-%d")
  # Only travel-related
  travel = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired!="Close Contact",]
  travel = select(travel,date_report,report_week,travel_yn,locally_acquired)%>%
    group_by(report_week)%>%
    add_tally()%>%
    rename("travel"="n")%>%
    select(report_week,travel)%>%
    distinct()
  travel$report_week=as.Date(travel$report_week)
  # Close contacts only
  contacts = travel.data[travel.data$travel_yn!=1 & travel.data$locally_acquired=="Close Contact",]
  contacts = select(contacts,date_report,report_week,travel_yn,locally_acquired)%>%
    group_by(report_week)%>%
    add_tally()%>%
    rename("contacts"="n")%>%
    select(report_week,contacts)%>%
    distinct()
  contacts$report_week=as.Date(contacts$report_week)
  # Travel and close contact:There is no data of this type - all close contacts are coded as not travel-related.
  travel.contacts = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired=="Close Contact",]$date_report

  if(province=="NL"){
    province="Newfoundland and Labrador"
  }
  if(province=="PEI"){
    province="Prince Edward Island"
  }
  if(province=="NWT"){
  province="Northwest Territories"
  }
  data = filter(PHAC.data, prname == province)%>%
    group_by(report_week)%>%
    add_tally(numtoday)%>%
    rename("new_cases"="n")%>%
    select(report_week,new_cases)%>%
    distinct()
  data=full_join(travel,contacts)%>%full_join(data)
}

NL.travel = importations("NL")%>%
  rename("NL_travel"=travel)%>%
  rename("NL_contacts"=contacts)%>%
  rename("NL_new_cases"=new_cases)
NS.travel = importations("Nova Scotia")%>%
  rename("NS_travel"=travel)%>%
  rename("NS_contacts"=contacts)%>%
  rename("NS_new_cases"=new_cases)
YT.travel = importations("Yukon")%>%
  rename("YT_travel"=travel)%>%
  rename("YT_contacts"=contacts)%>%
  rename("YT_new_cases"=new_cases)
NB.travel = importations("New Brunswick")%>%
  rename("NB_travel"=travel)%>%
  rename("NB_contacts"=contacts)%>%
  rename("NB_new_cases"=new_cases)
PEI.travel = importations("PEI")%>%
  rename("PEI_travel"=travel)%>%
  rename("PEI_contacts"=contacts)%>%
  rename("PEI_new_cases"=new_cases)
NWT.travel = importations("NWT")%>%
  rename("NWT_travel"=travel)%>%
  rename("NWT_contacts"=contacts)%>%
  rename("NWT_new_cases"=new_cases)
travel = full_join(NL.travel,NS.travel)%>%
  full_join(NB.travel)%>%
  full_join(PEI.travel)%>%
  full_join(YT.travel)%>%
  full_join(NWT.travel)%>%
  arrange(report_week)


## AV. WEEKLY ACTIVE CASES PER 10K PEOPLE FROM PHAC
# Weekly active cases in all provinces per 10K people - to be used as explanatory variables
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901
#Q1 2021
NL.pop<-519664
PEI.pop<-161514
NS.pop<-982012
NB.pop<-784156
QC.pop<-8579370
ON.pop<-14759431
MB.pop<-1381459
SK.pop<-1178971
AB.pop<-4431454
BC.pop<-5163919

active.fun = function(prov,pop){ 
  active = filter(PHAC.data, prname==prov)%>%
  group_by(report_week)%>%
  add_tally(numactive)%>%
  mutate("av.active"=n/7)%>%
  mutate("active.per.10K" = 1e4*av.active/pop)%>%
  select(report_week,active.per.10K)%>%
  distinct()
}

BC.active = active.fun("British Columbia", BC.pop)%>%
  rename("BC.active"=active.per.10K)
AB.active = active.fun("Alberta", AB.pop)%>%
  rename("AB.active"=active.per.10K)
SK.active = active.fun("Saskatchewan", SK.pop)%>%
  rename("SK.active"=active.per.10K)
MB.active = active.fun("Manitoba", MB.pop)%>%
  rename("MB.active"=active.per.10K)
ON.active = active.fun("Ontario", ON.pop)%>%
  rename("ON.active"=active.per.10K)
QC.active = active.fun("Quebec", QC.pop)%>%
  rename("QC.active"=active.per.10K)
NB.active = active.fun("New Brunswick", NB.pop)%>%
  rename("NB.active"=active.per.10K)
NS.active = active.fun("Nova Scotia", NS.pop)%>%
  rename("NS.active"=active.per.10K)

active = full_join(BC.active, AB.active)%>%
  full_join(SK.active)%>%
  full_join(MB.active)%>%
  full_join(ON.active)%>%
  full_join(QC.active)%>%
  full_join(NB.active)%>%
  full_join(NS.active)%>%
  distinct()


## TRAVEL-RELATED CASE DATA FROM EXTERNAL SOURCES
NL.travel.2 = group_by(NL.travel.2,report_week)%>%
  add_tally(TRAVEL)%>%
  rename("NL.travel.2"="n")%>%
  add_tally(CLOSE_CONTACT)%>%
  rename("NL.contact.2"="n")%>%
  select(report_week,NL.travel.2,NL.contact.2)%>%
  distinct()

NB.travel.2 = group_by(NB.travel.2,report_week)%>%
  add_tally(Travel.related.Cases)%>%
  rename("NB.travel.2"="n")%>%
  select(report_week,NB.travel.2)%>%
  distinct()

travel = full_join(travel, NL.travel.2)%>%full_join(active)%>%
  as.data.frame()
# The NAs recreated are 0
travel[is.na(travel)]=0

# FOR NB.travel.2 only some NAs are 0 because NB.travel.2 does not cover the full time range
travel = full_join(travel, NB.travel.2)%>%
  arrange(report_week)

# Constrain dates:
# Few travel-related cases before July 5, 2020
# No CCODWG data available after May 31, 2021
travel = travel%>%filter(report_week>="2020-07-05" & report_week<="2021-05-30")

## MAKE THE VALIDATION DATASETS - Do not aggregate by week
province = "NL"
travel.data.2020 <- CCODWG.2020[CCODWG.2020$province==province,]
travel.data.2021 <- CCODWG.2021[CCODWG.2021$province==province,]
travel.data.2021b <- CCODWG.2021b[CCODWG.2021b$province==province,]
travel.data = rbind(travel.data.2020,travel.data.2021, travel.data.2021b)
travel.data$date_report=format(as.Date(travel.data$date_report, format = "%d-%m-%Y"),"%Y-%m-%d")
travel.data$report_week = format(as.Date(travel.data$report_week, format = "%d-%m-%Y"),"%Y-%m-%d")
CCODWG.NL = data.frame(date_report=travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired!="Close Contact",]$date_report, travel = 1)
CCODWG.NL = group_by(CCODWG.NL,date_report)%>%
  add_tally()%>%
  distinct()%>%
  select(date_report,n)
  

province = "New Brunswick"
travel.data.2020 <- CCODWG.2020[CCODWG.2020$province==province,]
travel.data.2021 <- CCODWG.2021[CCODWG.2021$province==province,]
travel.data.2021b <- CCODWG.2021b[CCODWG.2021b$province==province,]
travel.data = rbind(travel.data.2020,travel.data.2021, travel.data.2021b)
travel.data$date_report=format(as.Date(travel.data$date_report, format = "%d-%m-%Y"),"%Y-%m-%d")
travel.data$report_week = format(as.Date(travel.data$report_week, format = "%d-%m-%Y"),"%Y-%m-%d")
CCODWG.NB = data.frame(date_report=travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired!="Close Contact",]$date_report, travel = 1)
CCODWG.NB = group_by(CCODWG.NB,date_report)%>%
  add_tally()%>%
  distinct()%>%
  select(date_report,n)%>%
  rename("CCODWG"=n)
NB.travel.2.daily=rename(NB.travel.2.daily, "date_report" = Date, "NB.govt" = Travel.related.Cases)
NB.validation = left_join(NB.travel.2.daily, CCODWG.NB)
NL.travel.2.daily=rename(NL.travel.2.daily, "date_report" = REPORTED_DATE, "NLCHI" = TRAVEL)

# There is a problem with the NLHCI data that needs to be fixed
write.csv(travel, "~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel.csv")
write.csv(active, "~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/active.csv")