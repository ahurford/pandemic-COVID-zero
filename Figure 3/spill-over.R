# Figure 3: Probability of at least one community infection each week for Newfoundland and Labrador until Dec 15, 2022.

library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)
library(imputeTS)

## Color map
cb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

##----------
## Load the data & clean to minimum level with date and the time variable 
# Travel-related cases arriving in NL, n (source: NLCHI data)
n <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL-travel.csv')[,-1]%>%
  rename(date=REPORTED_DATE)
n <- data.frame(date=NL.travel$date, n = NL.travel$TRAVEL)%>%filter(date<"2021-12-25")
n$date <- as.Date(n$date)

# Vaccination data (source: PHAC)
vaccination <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/vaccination-coverage-map.csv')%>%
  rename(date = week_end)
vaccination$date = as.Date(vaccination$date)
vaccination$proptotal_fully = as.numeric(vaccination$proptotal_fully)

vaccination.Canada <- filter(vaccination,prename=="Canada")%>%
  select(date, proptotal_partially, proptotal_fully, proptotal_additional)
vaccination.NL <- filter(vaccination,prename=="Newfoundland and Labrador")%>%
  select(date, proptotal_partially, proptotal_fully, proptotal_additional)



# Variant data (source: PHAC)
variant <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/covid19-epiSummary-variants.csv')[,-1]%>%
  rename(date = "Collection..week.", fraction = "X.CT.Count.of.Sample..")
variant$date <- as.Date(variant$date, format = "%Y-%m-%d")

variant.clean  = function(var){
  variant%>%filter(X_Identifier==var)%>%select(date,fraction)%>%
  group_by(date)%>%
  add_tally(fraction)%>%
  select(date,n)%>%
  distinct()%>%
  arrange(date)%>%
    rename(freq=n)%>%
  as.data.frame()
}

alpha = variant.clean("Alpha")
delta = variant.clean("Delta")
omicron = variant.clean("BA.1")

#-------------
# Align all the data sources, fill NAs with 0s or interpolate missing values
start.date = as.Date(min(n$date))
data = data.frame(date = seq(from=start.date, to = as.Date("2021-12-24"), by = "days"))
data = left_join(data, n)

# replace any NAs created by missing dates with 0 travel-related cases
data[is.na(data)]=0

# Join the Canadian vaccination data
data <- left_join(data, vaccination.Canada)

vacc.interp = function(){
# Linear interpolation for the vaccination data where NAs were created during the left_join
data$proptotal_partially[1] = 0
data$proptotal_partially = na_interpolation(data$proptotal_partially)
data$proptotal_fully[1] = 0
data$proptotal_fully = na_interpolation(data$proptotal_fully)
data$proptotal_additional[1] = 0
if(is.na(tail(data$proptotal_additional,1))){
  data$proptotal_additional[length(data$proptotal_additional)]=0
}
data$proptotal_additional = na_interpolation(data$proptotal_additional)
# Because additional doses are few and recently administered bundle with fully vaccinated:
data$proptotal_fully = data$proptotal_fully+data$proptotal_additional
# Remove additional doses column
data = data%>%select(-proptotal_additional)
}

# Interpolate and clean-up the Canadian vaccination data
data = vacc.interp()
# Rename the Canadian vaccination data columns
data = data %>% rename(CAN.partial = proptotal_partially, CAN.full = proptotal_fully)%>%
  mutate(CAN.unvax = 100 - CAN.full - CAN.partial)

# Join the NL vaccination data:
data <- left_join(data, vaccination.NL)
# Perform the linear interpolation for the NL vaccination data:
data = vacc.interp()%>% rename(NL.partial = proptotal_partially, NL.full = proptotal_fully)%>%
  mutate(NL.unvax = 100 - NL.full - NL.partial)

# Join the alpha variant
data <- left_join(data,alpha)

var.interp = function(){
  data$freq[1]=0
  data$freq = na_interpolation(data$freq)
  return(data)
}

data = var.interp()%>%rename(alpha = freq)
# Join the delta variant
data <- left_join(data,delta)
data <- var.interp()%>%rename(delta = freq)
# Join the omicron variant
data <- left_join(data,omicron)
data <- var.interp()%>%rename(omicron = freq)
