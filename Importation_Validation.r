
#Hi Amy, there is possibly a simple explanation for this: some time ago the GNB people
#told us that when interpreting the press releases to associate cases with the previous day.
#So, the travel related cases for March 24 are actually listed on the March 25 release as opposed
#to the March 24 release.  Similarly, the May 6 release mentions 10 travel related cases, which
#Bilal recorded on May 5.  We definitely should have mentioned this reporting quirk earlier...
#we need to do it to match up with some datasets that the province gave us directly.

#Wondering if the other data set actually does report 19 and 10 travel related cases on March 25 and
#May 6, respectively?  The May data is interesting because it includes people from NB who were
#isolating out of province, which I guess are rotational workers that got sick elsewhere.
#Not sure what to do with such data.


library(readxl)
library(ggplot2)
library(patchwork)

COVID.data.2020=read.csv("/Users/ahurford/Desktop/Work/Research/Research_Projects/2021/Reopening/COVIDdata2020.csv")
COVID.data.2021=read.csv("/Users/ahurford/Desktop/Work/Research/Research_Projects/2021/Reopening/COVIDdata2021.csv")
COVID.data.2021b=read.csv("/Users/ahurford/Desktop/Work/Research/Research_Projects/2021/Reopening/COVIDdata2021b.csv")


# New Brunswick - validating dataset against Bilal's work
province = "New Brunswick"
travel.data.2020 <- COVID.data.2020[COVID.data.2020$province==province,]
travel.data.2021 <- COVID.data.2021[COVID.data.2021$province==province,]
travel.data.2021b <- COVID.data.2021b[COVID.data.2021b$province==province,]
travel.data = rbind(travel.data.2020,travel.data.2021, travel.data.2021b)
travel.data$date_report=as.Date(travel.data$date_report,format="%d-%m-%Y")
# Only travel-related
travel.data = travel.data[travel.data$travel_yn==1,]
travel.data2 = data.frame(number = rep(1,length(travel.data[,1])),date=as.Date(travel.data$date_report))
NB.travel = aggregate(travel.data2$number, by = list(travel.data2$date),FUN = sum)

dates = seq(as.Date("2020-03-01"), as.Date("2021-05-31"),"days")
NB.travel.days = data.frame(dates = dates, cases = rep(0,length(dates)))

for(i in seq(length(NB.travel[,1]))){
  loc = which(dates==NB.travel$Group.1[i])
  NB.travel.days$cases[loc] = NB.travel$x[i]
}

## DATASET VALIDATION
# Load Bilal's data:
NB.Bilal <- read_excel("/Users/ahurford/Desktop/Work/Research/Research_Projects/2021/Reopening/reopening/Hurford/NB travel related cases.xlsx")
NB.Bilal = as.data.frame(NB.Bilal)


# Dates with overlap on both datasets
dates2 = seq(as.Date("2021-01-02"), as.Date("2021-05-31"),"days")
NB.travel = data.frame(dates = dates2, Bilal = NA, OpenCovid=NA) 

NB.travel$Bilal = NB.Bilal[1:length(dates2),2]
loc = which(dates==as.Date("2021-01-02"))
NB.travel$OpenCovid = NB.travel.days$cases[loc:length(NB.travel.days[,1])]
NB.travel$diff = NB.travel$Bilal-NB.travel$OpenCovid

g1 =ggplot(NB.travel,aes(x=dates)) +
  geom_line(aes(y=Bilal), col="red") +
  geom_line(aes(y=OpenCovid), col="black") +
  ylab("Bilal - Open COVID")+
  xlab("")

