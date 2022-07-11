# Figure 3: Probability of at least one community infection each week for Newfoundland and Labrador until Dec 15, 2022.

library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)
library(imputeTS)
library(patchwork)

## Color map
cb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

##----------
## Load the data & clean to minimum level with date and the time variable 
# Travel-related cases arriving in NL, n (source: NLCHI data)
n <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL-travel.csv')[,-1]%>%
  rename(date=REPORTED_DATE)%>%filter(date<"2021-12-25")
# close contacts per traveller
c = sum(n$CLOSE_CONTACT)/sum(n$TRAVEL)
n <- data.frame(date=n$date, n = n$TRAVEL, c=n$CLOSE_CONTACT)
n$date <- as.Date(n$date)

# Load community cases
community <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NLCHI_cases.csv')[,-1]%>%
  rename(date=REPORTED_DATE)%>%filter(date<"2021-12-25")%>%
  dplyr::select(date,COMMUNITY)
community$date = as.Date(community$date)

# Vaccination data (source: PHAC)
vaccination <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/vaccination-coverage-map.csv')%>%
  rename(date = week_end)
vaccination$date = as.Date(vaccination$date)
vaccination$proptotal_additional[is.na(vaccination$proptotal_additional)]=0
vaccination$proptotal_fully = as.numeric(vaccination$proptotal_fully)
vaccination <- mutate(vaccination, proptotal_fully = proptotal_fully/100)%>%
  mutate(proptotal_partially = proptotal_partially/100)%>%
  mutate(proptotal_additional = proptotal_additional/100)

vaccination.Canada <- filter(vaccination,prename=="Canada")%>%
  dplyr::select(date, proptotal_partially, proptotal_fully, proptotal_additional)
vaccination.NL <- filter(vaccination,prename=="Newfoundland and Labrador")%>%
  dplyr::select(date, proptotal_partially, proptotal_fully, proptotal_additional)



# Variant data (source: PHAC)
variant <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/covid19-epiSummary-variants.csv')[,-1]%>%
  rename(date = "Collection..week.", fraction = "X.CT.Count.of.Sample..")
variant$date <- as.Date(variant$date, format = "%Y-%m-%d")

variant.clean  = function(var){
  variant%>%filter(X_Identifier==var)%>%dplyr::select(date,fraction)%>%
  group_by(date)%>%
  add_tally(fraction)%>%
  dplyr::select(date,n)%>%
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

# Add the community cases
data <- left_join(data, community)
data[is.na(data)]=0

# Join the Canadian vaccination data
data <- left_join(data, vaccination.Canada)

vacc.interp = function(){
# Linear interpolation for the vaccination data where NAs were created during the left_join
data$proptotal_partially = na_interpolation(data$proptotal_partially)
data$proptotal_fully = na_interpolation(data$proptotal_fully)
data$proptotal_additional = na_interpolation(data$proptotal_additional)
# Exclude additional from full
data$proptotal_fully = data$proptotal_fully- data$proptotal_additional
return(data)
}

# Interpolate and clean-up the Canadian vaccination data
data = vacc.interp()
# Rename the Canadian vaccination data columns
data = data %>% rename(CAN.partial = proptotal_partially, CAN.full = proptotal_fully, CAN.additional = proptotal_additional)%>%
  mutate(CAN.unvax = 1 - CAN.full - CAN.partial - CAN.additional)

# Join the NL vaccination data:
data <- left_join(data, vaccination.NL)
# Perform the linear interpolation for the NL vaccination data:
data = vacc.interp()%>% rename(NL.partial = proptotal_partially, NL.full = proptotal_fully, NL.additional = proptotal_additional)%>%
  mutate(NL.unvax = 1 - NL.full - NL.partial - NL.additional)

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
data <- var.interp()%>%rename(omicron = freq)%>%
  mutate(original = round(1- omicron - delta-alpha,2))
# rounding is to remove -ve numbers

g.var =ggplot(data,aes(as.Date(date),group=1)) +
  geom_ribbon(aes(ymax=1, ymin=alpha+delta+omicron), fill="grey", alpha=0.3)+
  geom_ribbon(aes(ymax = alpha+delta+omicron, ymin=alpha+delta), fill=palette.colors(7)[7], alpha=.8)+
  geom_ribbon(aes(ymax = alpha+delta, ymin=alpha), fill=palette.colors(3)[3], alpha=.8)+
  geom_ribbon(aes(ymax = alpha, ymin=0), fill=palette.colors(4)[4], alpha=.8)+
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("proportion")+
  ggtitle("Variants")+
  #coord_cartesian(ylim=c(0, 25))+
  annotate("text", x = as.Date("2021-12-10"), y = .9, label = "BA.1", fontface=2)+
  annotate("text", x = as.Date("2021-09-01"), y = .6, label = "Delta", col = "black", fontface=2)+
  annotate("text", x = as.Date("2021-04-07"), y = .25, label = "Alpha", col = "black", fontface=2)+
  annotate("text", x = as.Date("2020-09-01"), y = .75, label = "Original", col = "black", fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))

g.vax =ggplot(data,aes(as.Date(date),group=1)) +
  geom_ribbon(aes(ymax=1, ymin=1-CAN.unvax), fill="grey", alpha=0.3)+
  geom_ribbon(aes(ymax = CAN.full+CAN.partial+CAN.additional, ymin=CAN.partial+CAN.full), fill="dodgerblue", alpha=.5)+
  geom_ribbon(aes(ymax = CAN.partial+CAN.full, ymin=CAN.partial), fill=palette.colors(2)[2], alpha=.5)+
  geom_ribbon(aes(ymax = CAN.partial, ymin=0), fill="darkorchid", alpha=.5)+
  geom_line(aes(y=NL.full+NL.partial+NL.additional), col="green")+
  geom_line(aes(y=NL.partial+NL.full), col=palette.colors(2)[2])+
  geom_line(aes(y=NL.partial), col="darkorchid")+
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("proportion")+
  ggtitle("Vaccination")+
  #coord_cartesian(ylim=c(0, 25))+
  annotate("text", x = as.Date("2021-11-01"), y = .6, label = "2 doses", fontface=2)+
  annotate("text", x = as.Date("2021-05-25"), y = .1, label = "1 dose", fontface=2)+
  annotate("text", x = as.Date("2020-10-01"), y = .75, label = "0 doses", col = "black", fontface=2)+
  annotate("text", x = as.Date("2021-12-24"), y = .75, label = "3", col = "black", fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))

###-------
# Vaccine efficacies and calculating, T_j,k
# There are teh Pfizer values
# https://www.nejm.org/doi/full/10.1056/NEJMoa2119451 (Omicron values, 2-does 25+ weeks)
VE = data.frame(original = c(0, .49, .93, .93), alpha = c(0, .49, .93, .93), delta = c(0, .33, 0.88, 0.88), omicron = c(0, 0, 0.09, 0.67))
T.original = data.frame(unvax = data$CAN.unvax*VE$original[1], partial = data$CAN.partial*VE$original[2], full = data$CAN.full*VE$original[3], additional = data$CAN.additional*VE$original[4])*data$original
T.alpha = data.frame(unvax = data$CAN.unvax*VE$alpha[1], partial = data$CAN.partial*VE$alpha[2], full = data$CAN.full*VE$alpha[3], additional = data$CAN.additional*VE$alpha[4])*data$alpha
T.delta = data.frame(unvax = data$CAN.unvax*VE$delta[1], partial = data$CAN.partial*VE$delta[2], full = data$CAN.full*VE$delta[3], additional = data$CAN.additional*VE$delta[4])*data$delta
T.omicron = data.frame(unvax = data$CAN.unvax*VE$omicron[1], partial = data$CAN.partial*VE$omicron[2], full = data$CAN.full*VE$omicron[3], additional = data$CAN.additional*VE$omicron[4])*data$omicron

# Normalize by dividing by the sum of the row sums:
T.sum = rowSums(T.original)+rowSums(T.alpha) + rowSums(T.delta) + rowSums(T.omicron)
T.original = T.original/T.sum
T.alpha = T.alpha/T.sum
T.delta = T.delta/T.sum
T.omicron = T.omicron/T.sum

## Maskwearing by anyone in the NL community (including travellers)
# It is assumed that mandatory masking reduces transmission by 50%
# Masking recommended reduces transmission by 25%

# No mask prior to Aug 24, 2020
L=length(data$date)
masks <- rep(1,L)
# Mandatory masks August 24, 2020 to August 10, 2021
i = which(data$date=="2020-08-24")
masks[i:L] <- 0.5
# Masks recommended become recommended on August 10, 2021
i = which(data$date =="2021-08-10")
masks[i:L] <- 0.75
# Maks are mandatory Sept 18, 2021 to beyond the end date
i = which(data$date=="2021-09-18")
masks[i:L] <- 0.5

## Estiamte number of travellers when the alpha variant occurred (divided by .5 because masks were mandatory)
alpha.travellers = sum(data$n*data$alpha)/0.5

## Consideration of vaccination in the NL community (no additional doses during study period)
NL.original = data$NL.unvax*VE$original[1] + data$NL.partial*VE$original[2] + data$NL.full*VE$original[3]
NL.alpha = data$NL.unvax*VE$alpha[1] + data$NL.partial*VE$alpha[2] + data$NL.full*VE$alpha[3]
NL.delta = data$NL.unvax*VE$delta[1] + data$NL.partial*VE$delta[2] + data$NL.full*VE$delta[3]
NL.omicron = data$NL.unvax*VE$omicron[1] + data$NL.partial*VE$omicron[2] + data$NL.full*VE$omicron[3]

## Self-isolation until negative test
# Another restriction that has been applied to travellers into NL is self-isolation until a negative test result.
# We assume this corresponds to 1-day of self-isolation, a test on day 0 of entering the province, and that days
# since exposure of arriving travellers are uniformly distributed from 0 to 10 days.  We assume that infectivity
# as a function of days since exposure follows a Weibull distribution with a shape parameter of 2.83 and a scale
# parameter of 5.67 (Ferretti et al. 2020). After 1 day of self-isolation, the distribution of days since
# exposure is uniform from 1 to 11 days. Summing the Weibull distribution evaluated for these days since exposure
# gives the exposure to the community.

# A restriction is a test on day 7, 8, or 9 and isolation until a negative test result. We assume the test
# occurs on day 8 and the traveller exits self-isolation on day 9, if negative. By day 8, many travellers
# are no longer infective as some were several days post-exposure on arrival. More precisely, and as implied
# by the Weibull distribution assumption, after 8 days of self-isolation 11% of infectivity remains.
# When the test occurs on day 8, the probability of a false negative test is estimated as 0.45.
# Therefore, the probability that infectious travellers are released into the community given day
# 7-9 testing and self-isolation until a negative test result is 0.45x0.89x(variant and mask effects)
# The requirement of testing more than offsets the reduced duration of self-isolation, such that
# the probability an infected traveller is released into the community with testing on day 8 is
# less than that of 14-day self-isolation.

# Infectivity on each day:
#dweibull(seq(0,10), 2.83, scale = 5.67, log = FALSE)

# Infectivity in community if isolating for 1-day - distribution of days since infection shifts by 1-day.
iso.0 = sum(dweibull(seq(1,11), 2.83, scale = 5.67, log = FALSE))
iso.7 = sum(dweibull(seq(8,18), 2.83, scale = 5.67, log = FALSE))
iso.5 = sum(dweibull(seq(6,16), 2.83, scale = 5.67, log = FALSE))

# The probability of a true positive when days since exposure is
# uniformly distribution from 0 to 10 is the mean of $t$, where $t_i$ is the probability of testing positive
# given infection $i$ days ago:
  
t.sens = c(0,.19,.39,.58,.77,.73,.68,.64,.59,.55,.5)
    
# The probability that an infected traveller is released into the community given
# self-isolation until a negative test result on arrival is estimated as $0.49$.

1-mean(t.sens)

# Transmissibility advantange of variants
omicron.trans = 1
delta.trans = omicron.trans/0.91
alpha.trans =delta.trans/1.95
original.trans = alpha.trans/1.29

## Restrictions for unvaccinated travellers
m.unvax.original <- rep(1/alpha.travellers/1.5, L)
m.unvax.alpha <- rep(1/alpha.travellers, L)
m.unvax.delta <- rep(1.5/1/alpha.travellers,L)
m.unvax.omicron <- rep((3.3*1.5)/1/alpha.travellers,L)

# on August 1, test on day 8 + isolation to negative for unvaccinated
i = which(data$date=="2021-08-01")
m.unvax.original[i:L] <- (0.45*m.unvax.original[i:L]/0.89)*original.trans
m.unvax.alpha[i:L] <- (0.45*m.unvax.alpha[i:L]/0.89)*alpha.trans
m.unvax.delta[i:L] <- (0.45*m.unvax.delta[i:L]/0.89)*delta.trans
m.unvax.omicron[i:L] <- (0.45*m.unvax.omicron[i:L]/0.89)*omicron.trans

## Restrictions for travellers with 1 dose
# Same restrictions prior re-opening
m.1.original <- m.unvax.original
m.1.alpha <- m.unvax.alpha
m.1.delta <- m.unvax.delta
m.1.omicron <- m.unvax.omicron

# July 1 - negative test step 1
i = which(data$date=="2021-07-01")
m.1.original[i:L] <- 0.49*original.trans
m.1.alpha[i:L] <- 0.49*alpha.trans
m.1.delta[i:L] <- 0.49*delta.trans
m.1.omicron[i:L] <- 0.49*omicron.trans

# August 1 - no measures
i = which(data$date=="2021-08-01")
m.1.original[i:L] <- original.trans
m.1.alpha[i:L] <- alpha.trans
m.1.delta[i:L] <- delta.trans
m.1.omicron[i:L] <- omicron.trans
# Sept 30 - same as unvaccinated
i = which(data$date=="2021-09-30")
m.1.original[i:L] <- m.unvax.original[i:L]
m.1.alpha[i:L] <- m.unvax.alpha[i:L]
m.1.delta[i:L] <- m.unvax.delta[i:L]
m.1.omicron[i:L] <- m.unvax.omicron[i:L]

## Two dose travellers (same as 3-dose also)
# Same restrictions as other travellers prior to reopening
m.2.original <- m.unvax.original
m.2.alpha <- m.unvax.alpha
m.2.delta <- m.unvax.delta
m.2.omicron <- m.unvax.omicron

# No requirements after July 1.
i = which(data$date=="2021-07-01")
m.2.original[i:L] <- original.trans
m.2.alpha[i:L] <- alpha.trans
m.2.delta[i:L] <- delta.trans
m.2.omicron[i:L] <- omicron.trans

# Dec 21: 5 RAT and isolate for 5 days.
# Since the study ends on Dec 24, the only impact
# is due to self-isolation
i = which(data$date=="2021-12-21")
iso.1 = sum(dweibull(seq(1,11), 2.83, scale = 5.67, log = FALSE))
iso.2 = sum(dweibull(seq(2,12), 2.83, scale = 5.67, log = FALSE))
iso.3 = sum(dweibull(seq(3,13), 2.83, scale = 5.67, log = FALSE))
iso.4 = sum(dweibull(seq(4,14), 2.83, scale = 5.67, log = FALSE))
m.2.original[i:L] <- original.trans*iso.1
m.2.alpha[i:L] <- alpha.trans*iso.1
m.2.delta[i:L] <- delta.trans*iso.1
m.2.omicron[i:L] <- omicron.trans*iso.1

m.2.original[(i+1):L] <- original.trans*iso.2
m.2.alpha[(i+1):L] <- alpha.trans*iso.2
m.2.delta[(i+1):L] <- delta.trans*iso.2
m.2.omicron[(i+1):L] <- omicron.trans*iso.2

m.2.original[(i+2):L] <- original.trans*iso.3
m.2.alpha[(i+2):L] <- alpha.trans*iso.3
m.2.delta[(i+2):L] <- delta.trans*iso.3
m.2.omicron[(i+2):L] <- omicron.trans*iso.3

m.2.original[(i+3):L] <- original.trans*iso.4
m.2.alpha[(i+3):L] <- alpha.trans*iso.4
m.2.delta[(i+3):L] <- delta.trans*iso.4
m.2.omicron[(i+3):L] <- omicron.trans*iso.4

# Vulnerability of the NL community to different variants
NL.original = data$NL.unvax*masks*(1-VE$original[1]) + data$NL.partial*(1-VE$original[2]) + data$NL.full*(1-VE$original[3]) + data$NL.additional*(1-VE$original[4])
NL.alpha = data$NL.unvax*masks*(1-VE$alpha[1]) + data$NL.partial*(1-VE$alpha[2]) + data$NL.full*(1-VE$alpha[3]) + data$NL.additional*(1-VE$alpha[4])
NL.delta = data$NL.unvax*masks*(1-VE$delta[1]) + data$NL.partial*(1-VE$delta[2]) + data$NL.full*(1-VE$delta[3]) + data$NL.additional*(1-VE$delta[4])
NL.omicron = data$NL.unvax*masks*(1-VE$omicron[1]) + data$NL.partial*(1-VE$omicron[2]) + data$NL.full*(1-VE$omicron[3]) + data$NL.additional*(1-VE$omicron[4])

# Spillover prob from a traveller that is infected with a given variant (and vax status)
p.original = data.frame(date = data$date,unvax=m.unvax.original*NL.original, partial=m.1.original*NL.original, full =m.2.original*NL.original, additional = m.2.original*NL.original)%>%
  mutate(total = unvax+partial+full+additional)
p.alpha = data.frame(date = data$date,unvax = m.unvax.alpha*NL.alpha, partial = m.1.alpha*NL.alpha, full = m.2.alpha*NL.alpha, additional = m.2.alpha*NL.alpha)%>%
  mutate(total = unvax+partial+full+additional)
p.delta = data.frame(date = data$date, unvax = m.unvax.delta*NL.delta, partial = m.1.delta*NL.delta, full = m.2.delta*NL.delta, additional = m.2.delta*NL.delta)%>%
  mutate(total = unvax+partial+full+additional)
p.omicron = data.frame(date = data$date,unvax = m.unvax.omicron*NL.omicron, partial = m.1.omicron*NL.omicron, full = m.2.omicron*NL.omicron, additional = m.2.omicron*NL.omicron)%>%
  mutate(total = unvax+partial+full+additional)

community.outbreaks = data.frame(date= data$date, community = data$COMMUNITY)
ymax=-.3
ymin=-.4
community.outbreaks$community[which(community.outbreaks$community<=5)]=ymin
community.outbreaks$community[which(community.outbreaks$community>5)]=ymax
community.alpha = community.outbreaks%>%
  filter(date<"2021-05-01")%>%
  rename(alpha=community)
community.delta= community.outbreaks%>%
  filter(date>="2021-05-01"&date<="2021-12-01")%>%
  rename(delta=community)
community.omicron= community.outbreaks%>%
  filter(date>="2021-12-01")%>%
  rename(omicron=community)
community.outbreaks=left_join(community.outbreaks, community.alpha)%>%
  left_join(community.delta)%>%
  left_join(community.omicron)
community.outbreaks[is.na(community.outbreaks)]=ymin

g.original =ggplot(p.original,aes(as.Date(date),group=1)) +
  geom_line(aes(y = full), col=palette.colors(2)[2])+
  geom_line(aes(y = partial), col="darkorchid")+
  geom_line(aes(y = unvax), col="grey")+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"), limits = c(as.Date("2021-04-01"), as.Date("2021-12-24")))+
  xlab("") +
  ylab("probability")+
  ggtitle("Original variant")+
  #coord_cartesian(ylim=c(0, 25))+
  annotate("text", x = as.Date("2021-12-07"), y = 0.75, label = "1st BA.1", col = palette.colors(7)[7], angle=90)+
  annotate("text", x = as.Date("2021-06-23"), y = 0.75, label = "Reopening", angle=90, col  ="darkgrey")+
  geom_line(data = data.frame(x = c(as.Date("2021-12-15"), as.Date("2021-12-15")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = palette.colors(7)[7])+
  geom_line(data = data.frame(x = c(as.Date("2021-07-01"), as.Date("2021-07-01")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = "darkgrey")+
  #annotate("text", x = as.Date("2021-12-24"), y = .75, label = "+1", col = "black", fontface=2)+
  theme_classic() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))

g.omicron =ggplot(p.omicron,aes(as.Date(date),group=1)) +
  geom_line(aes(y = additional), col="dodgerblue")+
  geom_line(aes(y = full), col=palette.colors(2)[2])+
  geom_line(aes(y = partial), col="darkorchid")+
  geom_line(aes(y = unvax), col="grey")+
  annotate("text", x = as.Date("2021-11-01"), y = .95, label = "2+ doses", fontface=2, col = palette.colors(2)[2])+
  annotate("text", x = as.Date("2021-09-01"), y = .75, label = "1 dose", fontface=2, col ="darkorchid")+
  annotate("text", x = as.Date("2021-06-01"), y = .1, label = "0 doses", col = "grey", fontface=2)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"), limits = c(as.Date("2021-04-01"), as.Date("2021-12-24")))+
  annotate("text", x = as.Date("2021-12-07"), y = 0.6, label = "1st BA.1", col = palette.colors(7)[7], angle=90)+
  annotate("text", x = as.Date("2021-06-23"), y = 0.75, label = "Reopening", angle=90, col  ="darkgrey")+
  geom_line(data = data.frame(x = c(as.Date("2021-12-15"), as.Date("2021-12-15")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = palette.colors(7)[7])+
  geom_line(data = data.frame(x = c(as.Date("2021-07-01"), as.Date("2021-07-01")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = "darkgrey")+
  xlab("") +
  ylab("probability")+
  ggtitle("Omicron variant")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))

g.alpha =ggplot(p.alpha,aes(as.Date(date),group=1)) +
  geom_line(aes(y = additional), col="dodgerblue")+
  geom_line(aes(y = full), col=palette.colors(2)[2])+
  geom_line(aes(y = partial), col="darkorchid")+
  geom_line(aes(y = unvax), col="grey")+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"), limits = c(as.Date("2021-04-01"), as.Date("2021-12-24")))+
  annotate("text", x = as.Date("2021-12-07"), y = 0.75, label = "1st BA.1", col = palette.colors(7)[7], angle=90)+
  annotate("text", x = as.Date("2021-06-23"), y = 0.75, label = "Reopening", angle=90, col  ="darkgrey")+
  geom_line(data = data.frame(x = c(as.Date("2021-12-15"), as.Date("2021-12-15")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = palette.colors(7)[7])+
  geom_line(data = data.frame(x = c(as.Date("2021-07-01"), as.Date("2021-07-01")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = "darkgrey")+
  xlab("") +
  ylab("probability")+
  ggtitle("Alpha variant")+
  theme_classic() + theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank(), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))

g.delta =ggplot(p.delta,aes(as.Date(date),group=1)) +
  geom_line(aes(y = additional), col="dodgerblue")+
  geom_line(aes(y = full), col=palette.colors(2)[2])+
  geom_line(aes(y = partial), col="darkorchid")+
  geom_line(aes(y = unvax), col="grey")+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"), limits = c(as.Date("2021-04-01"), as.Date("2021-12-24")))+
  annotate("text", x = as.Date("2021-12-07"), y = 0.75, label = "1st BA.1", col = palette.colors(7)[7], angle=90)+
  annotate("text", x = as.Date("2021-06-23"), y = 0.75, label = "Reopening", angle=90, col  ="darkgrey")+
  geom_line(data = data.frame(x = c(as.Date("2021-12-15"), as.Date("2021-12-15")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = palette.colors(7)[7])+
  geom_line(data = data.frame(x = c(as.Date("2021-07-01"), as.Date("2021-07-01")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = "darkgrey")+
  xlab("") +
  ylab("probability")+
  ggtitle("Delta variant")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))


# Expected number of travellers with a given variant (and vax status)
n.original = data.frame(date = data$date, unvax = T.original$unvax*data$n, partial = T.original$partial*data$n, full = T.original$full*data$n, additional = T.original$additional*data$n)%>%
  mutate(total = unvax+partial+full+additional)
n.alpha = data.frame(date = data$date, unvax = T.alpha$unvax*data$n, partial = T.alpha$partial*data$n, full = T.alpha$full*data$n, additional = T.alpha$additional*data$n)%>%
  mutate(total = unvax+partial+full+additional)
n.delta = data.frame(date = data$date, unvax = T.delta$unvax*data$n, partial = T.delta$partial*data$n, full = T.delta$full*data$n, additional = T.delta$additional*data$n)%>%
  mutate(total = unvax+partial+full+additional)
n.omicron = data.frame(date = data$date, unvax = T.omicron$unvax*data$n, partial = T.omicron$partial*data$n, full = T.omicron$full*data$n, additional = T.omicron$additional*data$n)%>%
  mutate(total = unvax+partial+full+additional)

# Expected probability of at least one infection from each variant
p1.original = 1 - (1-p.original$unvax)^(n.original$unvax*(1+c))*(1-p.original$partial)^(n.original$partial*(1+c))*(1-p.original$full)^(n.original$full*(1+c))*(1-p.original$additional)^(n.original$additional*(1+c))
p1.alpha = 1 - (1-p.alpha$unvax)^(n.alpha$unvax*(1+c))*(1-p.alpha$partial)^(n.alpha$partial*(1+c))*(1-p.alpha$full)^(n.alpha$full*(1+c))*(1-p.alpha$additional)^(n.alpha$additional*(1+c))
p1.delta = 1 - (1-p.delta$unvax)^(n.delta$unvax*(1+c))*(1-p.delta$partial)^(n.delta$partial*(1+c))*(1-p.delta$full)^(n.delta$full*(1+c))*(1-p.delta$additional)^(n.delta$additional*(1+c))
p1.omicron = 1 - (1-p.omicron$unvax)^(n.omicron$unvax*(1+c))*(1-p.omicron$partial)^(n.omicron$partial*(1+c))*(1-p.omicron$full)^(n.omicron$full*(1+c))*(1-p.omicron$additional)^(n.omicron$additional*(1+c))
p1.total = 1-(1-p1.original)*(1-p1.alpha)*(1-p1.delta)*(1-p1.omicron)

p1 = data.frame(date = data$date, original = p1.original, alpha = p1.alpha, delta = p1.delta, omicron = p1.omicron, total=p1.total)
p1$original = c(p1$original[1:6], rollmean(p1$original,7))
p1$alpha = c(p1$alpha[1:6], rollmean(p1$alpha,7))
p1$delta = c(p1$delta[1:6], rollmean(p1$delta,7))
p1$omicron = c(p1$omicron[1:6], rollmean(p1$omicron,7))
p1$total = c(p1$total[1:6], rollmean(p1$total,7))

#Expected spillovers
E.original = p.original$unvax*n.original$unvax + p.original$partial*n.original$partial + p.original$full*n.original$full + p.original$additional*n.original$additional
E.alpha = p.alpha$unvax*n.alpha$unvax + p.alpha$partial*n.alpha$partial + p.alpha$full*n.alpha$full + p.alpha$additional*n.alpha$additional
E.delta = p.delta$unvax*n.delta$unvax + p.delta$partial*n.delta$partial + p.delta$full*n.delta$full + p.delta$additional*n.delta$additional
E.omicron = p.omicron$unvax*n.omicron$unvax + p.omicron$partial*n.omicron$partial + p.omicron$full*n.omicron$full + p.omicron$additional*n.omicron$additional

# Take rolling mean
E.original = c(E.original[1:6], rollmean(E.original, 7))
E.alpha = c(E.alpha[1:6], rollmean(E.alpha, 7))
E.delta = c(E.delta[1:6], rollmean(E.delta, 7))
E.omicron = c(E.omicron[1:6], rollmean(E.omicron, 7))

E.spillovers = data.frame(date = data$date, original = E.original, alpha = E.alpha, delta = E.delta, omicron = E.omicron)

g.spillovers =ggplot(E.spillovers,aes(as.Date(date),group=1)) +
  geom_ribbon(aes(ymax = alpha+delta+omicron+original, ymin=alpha+delta+original), fill=palette.colors(7)[7], alpha=.8)+
  geom_ribbon(aes(ymax = alpha+delta+original, ymin=alpha+original), fill=palette.colors(3)[3], alpha=.8)+
  geom_ribbon(aes(ymax = alpha+original, ymin=original), fill=palette.colors(4)[4], alpha=.8)+
  geom_ribbon(aes(ymax = original, ymin=0), fill="grey", alpha=.8)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("Expected number per day (7-day rolling mean)")+
  ggtitle("Spillovers")+
  #coord_cartesian(ylim=c(0, .5))+
  annotate("text", x = as.Date("2021-12-10"), y = .9, label = "BA.1", fontface=2)+
  annotate("text", x = as.Date("2021-09-01"), y = .6, label = "Delta", col = "black", fontface=2)+
  annotate("text", x = as.Date("2021-04-07"), y = .25, label = "Alpha", col = "black", fontface=2)+
  annotate("text", x = as.Date("2020-09-01"), y = .75, label = "Original", col = "black", fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))

g1 = ggplot(p1,aes(as.Date(date),group=1))+
  geom_line(aes(y=total), col = "black")+
  geom_line(aes(y=omicron), col=palette.colors(7)[7])+
  geom_line(aes(y=delta), col=palette.colors(3)[3])+
  geom_line(aes(y = alpha), col=palette.colors(4)[4])+
  geom_line(aes(y = original), col="grey")+
  geom_ribbon(aes(ymax=community.outbreaks$alpha, ymin = ymin), fill =palette.colors(4)[4])+
  geom_ribbon(aes(ymax=community.outbreaks$delta, ymin = ymin), fill =palette.colors(3)[3])+
  geom_ribbon(aes(ymax=community.outbreaks$omicron, ymin = ymin), fill =palette.colors(7)[7])+
  scale_x_date(breaks = date_breaks("2 week"),
               labels = date_format("%d %b %Y"), limits = c(as.Date("2021-01-01"), as.Date("2021-12-24")))+
  xlab("") +
  scale_y_continuous(breaks=c(0,.25, .5, .75,1))+
  ylab("7-day rolling mean, daily")+
  ggtitle("Newfoundland and Labrador: Community outbreak probability")+
  annotate("text", x = as.Date("2021-12-24"), y = .5, label = "BA.1",col = palette.colors(7)[7], fontface=2)+
  annotate("text", x = as.Date("2021-09-01"), y = .45, label = "Delta", col = palette.colors(3)[3], fontface=2)+
  annotate("text", x = as.Date("2021-05-05"), y = .25, label = "Alpha", col = palette.colors(4)[4], fontface=2)+
  annotate("text", x = as.Date("2021-07-07"), y = .45, label = "All", col = "black", fontface=2)+
  annotate("text", x = as.Date("2021-01-10"), y = -.25, label = "Community\noutbreaks", col = "black", size=3)+
  annotate("text", x = as.Date("2021-02-20"), y = -.2, label = "Mt. Pearl", col = palette.colors(4)[4], size=3)+
  annotate("text", x = as.Date("2021-06-01"), y = -.2, label = "Lewisporte", col = palette.colors(3)[3], size=3)+
  annotate("text", x = as.Date("2021-09-29"), y = -.2, label = "Baie Verte", col = palette.colors(3)[3], size=3)+
  annotate("text", x = as.Date("2021-09-05"), y = -.2, label = "Labrador", col = palette.colors(3)[3], size=3)+
  annotate("text", x = as.Date("2021-10-26"), y = -.2, label = "Burin Pen.", col = palette.colors(3)[3], size=3)+
  annotate("text", x = as.Date("2021-12-18"), y = -.2, label = "St. John's", col = palette.colors(7)[7], size=3)+
  annotate("text", x = as.Date("2021-12-10"), y = 0.75, label = "1st BA.1", col = palette.colors(7)[7], angle=90)+
  annotate("text", x = as.Date("2021-06-26"), y = 0.75, label = "Reopening", angle=90, col  ="darkgrey")+
  geom_line(data = data.frame(x = c(as.Date("2021-12-15"), as.Date("2021-12-15")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = palette.colors(7)[7])+
  geom_line(data = data.frame(x = c(as.Date("2021-07-01"), as.Date("2021-07-01")), y = c(0, 1)), aes(x = x, y = y),lty=2, col = "darkgrey")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))

n.variant = data.frame(date = data$date, original = n.original$total, alpha = n.alpha$total, delta = n.delta$total, omicron = n.omicron$total)
n.variant$original = c(n.variant$original[1:6], rollmean(n.variant$original, 7))
n.variant$alpha = c(n.variant$alpha[1:6], rollmean(n.variant$alpha, 7))
n.variant$delta = c(n.variant$delta[1:6], rollmean(n.variant$delta, 7))
n.variant$omicron = c(n.variant$omicron[1:6], rollmean(n.variant$omicron, 7))

p.variant = data.frame(date = data$date, original = p.original$total, alpha = p.alpha$total, delta = p.delta$total, omicron = p.omicron$total)
p.variant$original = c(p.variant$original[1:6], rollmean(p.variant$original, 7))
p.variant$alpha = c(p.variant$alpha[1:6], rollmean(p.variant$alpha, 7))
p.variant$delta = c(p.variant$delta[1:6], rollmean(p.variant$delta, 7))
p.variant$omicron = c(p.variant$omicron[1:6], rollmean(p.variant$omicron, 7))


g.n =ggplot(n.variant,aes(as.Date(date),group=1)) +
  geom_ribbon(aes(ymax = alpha+delta+omicron+original, ymin=alpha+delta+original), fill=palette.colors(7)[7], alpha=.8)+
  geom_ribbon(aes(ymax = alpha+delta+original, ymin=alpha+original), fill=palette.colors(3)[3], alpha=.8)+
  geom_ribbon(aes(ymax = alpha+original, ymin=original), fill=palette.colors(4)[4], alpha=.8)+
  geom_ribbon(aes(ymax = original, ymin=0), fill="grey", alpha=.8)+
  scale_x_date(breaks = date_breaks("2 month"),
               labels = date_format("%b %Y"))+
  #scale_y_continuous(trans='log2')+
  xlab("") +
  #annotate("text", x = as.Date("2021-12-07"), y = 5, label = "1st BA.1", col = palette.colors(7)[7], angle=90)+
  annotate("text", x = as.Date("2021-06-23"), y = 5, label = "Reopening", angle=90, col  ="darkgrey")+
  #geom_line(data = data.frame(x = c(as.Date("2021-12-15"), as.Date("2021-12-15")), y = c(0, 10)), aes(x = x, y = y),lty=2, col = palette.colors(7)[7])+
  geom_line(data = data.frame(x = c(as.Date("2021-07-01"), as.Date("2021-07-01")), y = c(0, 10)), aes(x = x, y = y),lty=2, col = "darkgrey")+
  ylab("7-day rolling mean, daily")+
  ggtitle("Travel-related cases")+
  #coord_cartesian(ylim=c(0, .5))+
  annotate("text", x = as.Date("2021-11-20"), y = 9, label = "BA.1", fontface=2, col = palette.colors(7)[7])+
  annotate("text", x = as.Date("2021-10-01"), y = 2.5, label = "Delta", col =palette.colors(3)[3], fontface=2)+
  annotate("text", x = as.Date("2021-04-21"), y = 7, label = "Alpha", fontface=2, col=palette.colors(4)[4])+
  annotate("text", x = as.Date("2020-08-01"), y = .75, label = "Original", col = "grey", fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))

g.omicron2 = g.omicron+
  ggtitle("Per traveller - Omicron")+
  theme(axis.title = element_text(size=rel(1)))+
  ylab("Probability of a \ncommunity infection")
  #scale_x_date(breaks = date_breaks("1 month"),
               #labels = date_format("%b %Y"))

#g.spillovers
gout2 = (g.original+g.alpha)/(g.delta+g.omicron)
ggsave("p_ijk.png", width = 8, height = 6)
gout1 = (g.var+g.vax)/(g.n+g.omicron2)/g1+ plot_annotation(tag_levels = 'A') + plot_layout(heights = c(1,1, 2))
ggsave("community-outbreak.png", width = 10, height = 12)
