# Figure 3: Probability of at least one community infection each week for Newfoundland and Labrador until Dec 15, 2022.

library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)
cb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

# Travel-related cases arriving in NL, n
NL.travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL-travel.csv')[,-1]%>%
  rename(date=REPORTED_DATE)
n = data.frame(date=NL.travel$date, n = NL.travel$TRAVEL)%>%filter(date<"2021-12-25")

# Vaccination data
vaccination <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/vaccination-coverage-map.csv')[,-1]

# Variant data
variant <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/covid19-epiSummary-variants.csv')[,-1]%>%
  rename(week = "Collection..week.", fraction = "X.CT.Count.of.Sample..")
variant$week <- as.Date(variant$week, format = "%Y-%m-%d")

variant.clean  = function(var){
  variant%>%filter(X_Identifier==var)%>%select(week,fraction)%>%
  group_by(week)%>%
  add_tally(fraction)%>%
  select(week,n)%>%
  distinct()%>%
  arrange(week)%>%
  as.data.frame()
}

alpha = variant.clean("Alpha")
delta = variant.clean("Delta")
omicron = variant.clean("BA.1")
