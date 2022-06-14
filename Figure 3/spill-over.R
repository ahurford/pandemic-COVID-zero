# Figure 3: Probability of at least one community infection each week for Newfoundland and Labrador until Dec 15, 2022.

library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)

# Travel-related cases arriving in NL



# NL case data
data=read.csv('~/Desktop/Work/Research/Research_Projects/2021/forecast/Randy/New_cases_by_episode_date_and_type.csv')
data=filter(data,)
i=which(data[,1]=="2020-07-01")
NLCHI.contacts.per.travel = sum(data$CLOSE_CONTACT_TRAVEL[-(1:(i-1))])/sum(data$HX_TRAVEL[-(1:(i-1))])

NL.episode.date = data.frame(ep.date = as.Date(data$EPISODE_DT), cases = rowSums(data[,2:6]), comm.cases=rowSums(data[,4:6]))


NL.episode.date = cbind(NL.episode.date, roll7 = c(rep(NA,6), rollmean(NL.episode.date$cases,7)))
NL.episode.date = NL.episode.date[-(1:(i-1)),]

# Check that the data resembles the reported number of daily cases
#plot(as.Date(data$EPISODE_DT), rowSums(data[,2:6]))

# Load NL travel-related cases.
data=read.csv('~/Desktop/Work/Research/Research_Projects/2021/forecast/Randy/New_travel_cases_by_reported_date.csv')



COVID.data<- read.csv('https://raw.githubusercontent.com/ahurford/covid-nl/master/covid19-download.csv')

