# Title: Data cleaning
# Date: June 14, 2022
# Description: Produces data files travel-related cases per week to Atlantic
#    Canada & territories from the COVID-19 Canada Open Data Working Group (CCODWG)
#    Appends validation data for NB and NL
#    Uses PHAC data source to record active cases per 10K people
#    CCODWG files are large
#======================

library(dplyr)
library(zoo)

## PULLING THE DATA FILES
# This is to pull the data a copy of the PHAC data for active cases
PHAC.data <- read.csv('https://raw.githubusercontent.com/ahurford/covid-nl/master/covid19-download.csv')
PHAC.data <- dplyr::select(PHAC.data,date,numtoday,numactive,prname)%>%
  mutate(report_week = as.Date(cut(as.Date(date),"week", start.on.monday = F)))%>%
  distinct()
## These datasets are inidividual-level from the COVID-19 Canada Open data working group. They give travel-related
# cases. The are large files.
CCODWG.2020=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2020.csv', fill=TRUE)
CCODWG.2021=read.csv('https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/retired_datasets/individual_level/cases_2021_1.csv', fill = TRUE)
CCODWG.2021b=read.csv('https://raw.githubusercontent.com/ishaberry//Covid19Canada/master/retired_datasets/individual_level/cases_2021_2.csv', fill = TRUE)
date_report <- data.frame(date_report=as.Date(seq(from=as.Date("2020-06-15"), to =as.Date("2021-05-31"), by = "day")))

# These are data files that will be used for validation
NL.travel.2 = read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL-travel.csv', fill=TRUE)
NB.travel.2 = read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NB-travel.csv', fill=TRUE)[,1:2]
NB.travel.2$Date = format(as.Date(NB.travel.2$Date, format = "%m/%d/%y"),"%Y-%m-%d")


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

# FOR ATLANTIC CANADA AND TERRITORIES TIME SERIES AGGREGATED BY WEEKS
importations=function(province){
  travel.data.2020 <- CCODWG.2020[CCODWG.2020$province==province,]
  travel.data.2021 <- CCODWG.2021[CCODWG.2021$province==province,]
  travel.data.2021b <- CCODWG.2021b[CCODWG.2021b$province==province,]
  travel.data = rbind(travel.data.2020,travel.data.2021, travel.data.2021b)
  travel.data$date_report=format(as.Date(travel.data$date_report, format = "%d-%m-%Y"),"%Y-%m-%d")
  travel.data$report_week = format(as.Date(travel.data$report_week, format = "%d-%m-%Y"),"%Y-%m-%d")
  # Only travel-related
  travel = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired!="Close Contact",]
  travel = dplyr::select(travel,date_report,report_week,travel_yn,locally_acquired)%>%
    group_by(report_week)%>%
    add_tally()%>%
    rename("travel"="n")%>%
    dplyr::select(report_week,travel)%>%
    distinct()
  travel$report_week=as.Date(travel$report_week)
  # Close contacts only
  contacts = travel.data[travel.data$travel_yn!=1 & travel.data$locally_acquired=="Close Contact",]
  contacts = dplyr::select(contacts,date_report,report_week,travel_yn,locally_acquired)%>%
    group_by(report_week)%>%
    add_tally()%>%
    rename("contacts"="n")%>%
    dplyr::select(report_week,contacts)%>%
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
    dplyr::select(report_week,new_cases)%>%
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
  arrange(report_week)%>%
  filter(report_week>"2020-07-01"&report_week<"2021-05-30")%>%
  as.data.frame()

travel[is.na(travel)]=0
travel.wk = travel

# FOR MODELLING TRAVEL-RELATED CASES ARRIVING IN ATLANTIC CANADA
importations2=function(province){
  travel.data.2020 <- CCODWG.2020[CCODWG.2020$province==province,]
  travel.data.2021 <- CCODWG.2021[CCODWG.2021$province==province,]
  travel.data.2021b <- CCODWG.2021b[CCODWG.2021b$province==province,]
  travel.data = rbind(travel.data.2020,travel.data.2021, travel.data.2021b)

  # Only travel-related
  travel = travel.data[travel.data$travel_yn==1 & travel.data$locally_acquired!="Close Contact",]
  travel$date_report=format(as.Date(travel$date_report, format = "%d-%m-%Y"),"%Y-%m-%d")
  
  travel = dplyr::select(travel,date_report)%>%
    group_by(date_report)%>%
    add_tally()%>%
    rename("travel"="n")%>%
    dplyr::select(date_report,travel)%>%
    distinct()%>%
    as.data.frame()
    travel$date_report=as.Date(travel$date_report)
    # Add zeros were no travel-related cases are reported
    travel = left_join(date_report,travel)
    travel[is.na(travel)]=0
    # 7-day rolling average of travel-related cases
    av_7 = c(rep(NA,6),rollmean(travel$travel,7, align="right"))
    travel = mutate(travel, av_7=av_7)
}

NL.travel = importations2("NL")%>%
  rename("NL_travel"=travel)%>%
  rename("NL_av_7"=av_7)

NS.travel = importations2("Nova Scotia")%>%
  rename("NS_travel"=travel)%>%
  rename("NS_av_7"=av_7)

NB.travel = importations2("New Brunswick")%>%
  rename("NB_travel"=travel)%>%
  rename("NB_av_7"=av_7)

PEI.travel = importations2("PEI")%>%
  rename("PEI_travel"=travel)%>%
  rename("PEI_av_7"=av_7)

travel = full_join(NL.travel,NS.travel)%>%
  full_join(NB.travel)%>%
  full_join(PEI.travel)%>%
  arrange(date_report)%>%
  filter(date_report>="2020-07-01")


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
  mutate("active.per.10K" = 1e4*numactive/pop)%>%
  dplyr::select(date,active.per.10K)%>%
  distinct()%>%
  arrange(date)%>%
    as.data.frame()
  return(active)
}

BC_active = active.fun("British Columbia", BC.pop)%>%
  rename("BC_active"=active.per.10K)
AB_active = active.fun("Alberta", AB.pop)%>%
  rename("AB_active"=active.per.10K)
SK_active = active.fun("Saskatchewan", SK.pop)%>%
  rename("SK_active"=active.per.10K)
MB_active = active.fun("Manitoba", MB.pop)%>%
  rename("MB_active"=active.per.10K)
ON_active = active.fun("Ontario", ON.pop)%>%
  rename("ON_active"=active.per.10K)
QC_active = active.fun("Quebec", QC.pop)%>%
  rename("QC_active"=active.per.10K)
NB_active = active.fun("New Brunswick", NB.pop)%>%
  rename("NB_active"=active.per.10K)
NS_active = active.fun("Nova Scotia", NS.pop)%>%
  rename("NS_active"=active.per.10K)

active = full_join(BC_active, AB_active)%>%
  full_join(SK_active)%>%
  full_join(MB_active)%>%
  full_join(ON_active)%>%
  full_join(QC_active)%>%
  full_join(NB_active)%>%
  full_join(NS_active)%>%
  distinct()

active$date=as.Date(active$date)
travel = rename(travel,date=date_report)
travel = left_join(travel,active)


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
  dplyr::select(date_report,n)%>%
  rename("CCODWG"=n)
  

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
  dplyr::select(date_report,n)%>%
  rename("CCODWG"=n)

date_report$date_report=as.Date(date_report$date_report)
CCODWG.NB$date_report=as.Date(CCODWG.NB$date_report)
# Validation period starts Jan 1 2021
NB.travel.2=rename(NB.travel.2, "date_report" = Date, "NB.govt" = Travel.related.Cases)
NB.travel.2$date_report=as.Date(NB.travel.2$date_report)
NB.validation = full_join(date_report, CCODWG.NB)%>%
  full_join(NB.travel.2,by="date_report")%>%
  filter(date_report<"2021-06-01"&date_report>"2020-12-31")%>% # Since CCODWG stops reporting after this.
  as.data.frame()%>%
  arrange(date_report)
NB.validation[is.na(NB.validation)]=0

# Validation period is the time covered for the CCOWGD  
NL.travel.2=rename(NL.travel.2, "date_report" = REPORTED_DATE, "NLCHI" = TRAVEL)%>%
  dplyr::select(date_report, NLCHI)%>%
  as.data.frame()
NL.travel.2$date_report=as.Date(NL.travel.2$date_report)
CCODWG.NL$date_report=as.Date(CCODWG.NL$date_report)
NL.validation = full_join(date_report,CCODWG.NL)%>%
  full_join(NL.travel.2)%>%
  filter(date_report>="2020-07-01"&date_report<"2021-06-01")%>%
  as.data.frame()%>%
  arrange(date_report)
NL.validation[is.na(NL.validation)]=0

# (1) Data to make the graph of time series of travel-related cases, close contacts, and new cases, aggregated by week
# for Atlantic Canada and the territories (except Nunavuat - no data)
write.csv(travel.wk, "~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel_wk.csv")

# (2) Data to model travel-related cases to Atlantic Canada (7-day moving average) predicted from active cases per 10K
# in other Canadian provinces. No not include the validation datasets: NL.travel.2 and NB.travel.2 in this analysis
write.csv(travel, "~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel.csv")

# (3) Full data set of active cases per 10K which can be used for prediction outside the time range of the model described in (2) 
write.csv(active, "~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/active.csv")

# (4) Datasets to validate the number of travel-related cases reported in the CCODWG data
write.csv(NL.validation, "~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL_validation.csv")
write.csv(NB.validation, "~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NB_validation.csv")