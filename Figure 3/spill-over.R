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
  rename(date=REPORTED_DATE)
n <- data.frame(date=NL.travel$date, n = NL.travel$TRAVEL)%>%filter(date<"2021-12-25")
n$date <- as.Date(n$date)

# Vaccination data (source: PHAC)
vaccination <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/vaccination-coverage-map.csv')%>%
  rename(date = week_end)
vaccination$date = as.Date(vaccination$date)
vaccination$proptotal_fully = as.numeric(vaccination$proptotal_fully)
vaccination <- mutate(vaccination, proptotal_fully = proptotal_fully/100)%>%
  mutate(proptotal_partially = proptotal_partially/100)%>%
  mutate(proptotal_additional = proptotal_additional/100)

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
  mutate(CAN.unvax = 1 - CAN.full - CAN.partial)

# Join the NL vaccination data:
data <- left_join(data, vaccination.NL)
# Perform the linear interpolation for the NL vaccination data:
data = vacc.interp()%>% rename(NL.partial = proptotal_partially, NL.full = proptotal_fully)%>%
  mutate(NL.unvax = 1 - NL.full - NL.partial)

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

g.var =ggplot(data,aes(as.Date(date),group=1)) +
  geom_ribbon(aes(ymax=1, ymin=alpha+delta+omicron), fill="grey", alpha=0.3)+
  geom_ribbon(aes(ymax = alpha+delta+omicron, ymin=alpha+delta), fill=palette.colors(7)[7], alpha=.8)+
  geom_ribbon(aes(ymax = alpha+delta, ymin=alpha), fill=palette.colors(3)[3], alpha=.8)+
  geom_ribbon(aes(ymax = alpha, ymin=0), fill=palette.colors(4)[4], alpha=.8)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("proportion")+
  ggtitle("Variants")+
  #coord_cartesian(ylim=c(0, 25))+
  annotate("text", x = as.Date("2021-12-10"), y = .9, label = "BA.1", fontface=2)+
  annotate("text", x = as.Date("2021-09-01"), y = .6, label = "Delta", col = "black", fontface=2)+
  annotate("text", x = as.Date("2021-04-07"), y = .25, label = "Alpha", col = "black", fontface=2)+
  annotate("text", x = as.Date("2020-09-01"), y = .75, label = "Original", col = "black", fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))

g.vax =ggplot(data,aes(as.Date(date),group=1)) +
  geom_ribbon(aes(ymax=1, ymin=1-CAN.unvax), fill="grey", alpha=0.3)+
  geom_ribbon(aes(ymax = CAN.full+CAN.partial, ymin=CAN.partial), fill=palette.colors(2)[2], alpha=.5)+
  geom_ribbon(aes(ymax = CAN.partial, ymin=0), fill="darkorchid", alpha=.5)+
  geom_line(aes(y=NL.full+NL.partial), col=palette.colors(2)[2])+
  geom_line(aes(y=NL.partial), col="darkorchid")+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("proportion")+
  ggtitle("Vaccination")+
  #coord_cartesian(ylim=c(0, 25))+
  annotate("text", x = as.Date("2021-11-01"), y = .6, label = "Full", fontface=2)+
  annotate("text", x = as.Date("2021-05-25"), y = .1, label = "Partial", fontface=2)+
  annotate("text", x = as.Date("2020-10-01"), y = .75, label = "Unvaccinated", col = "black", fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))

g.var/g.vax

# Timeline of measures
# Variant transmissibility factors
# No mask prior to Aug 24, 2020
masks <- rep(1,T)
# Mandatory masks August 24, 2020 to August 10, 2021
masks[55:T] <- 0.5
# Masks recommended
masks[406:T] <- 0.75
# Mandatory Sept 18, 2021-
masks[445:T] <- 0.5

In February 2021, NL experienced a community outbreak of [alpha variant](link). During the entire period that the alpha variant was common, travellers were required to self-isolate for 14 days in NL, and few NLers were vaccinated. From our modelling above, we estimate 316 travel-related cases and close contacts of travellers infected with the alpha variant, one of which resulted in a community outbreak in Mt. Pearl. Therefore, we estimate the efficacy of 14-day isolation, for a traveller infected with the alpha variant, when no one in the NL community is vaccinated as $s_\alpha^0 = 1/316$.

Generally, it is difficult to estimate the efficacy of self-isolation but our calculation above is based on NL data. Next, we use this estimated value of $s_\alpha^0 = 1/316$ make assumes to estimate similar probabilities given different variants, vaccination rates, and restrictions applied to travellers. 

Assuming the delta variant is 50% more transmissible than alpha, and alpha is 50% more transmissible than the original variant, we estimate $s_\delta^0 = 1/211$ and $s_0^0 = 1/474$ (where the superscript 0 indicates that no NLers are vaccinated).  If $x = [x_0, x_1, x_2]$ is the frequency of NLers that are unvaccinated, vaccinated with 1 dose, or vaccinated with 2 doses, then the probability of a community case given that a traveller is infected with a particular variant and is required to self-isolate for 14 days is:
  
  $s^0 = s_0^0(x_0 + 0.5x_1 + 0.06x_2)$
  
  $s^\alpha = s_\alpha^0(x_0 + 0.5x_1 + 0.06x_2)$
  
  $s^\delta = s_\delta^0(x_0 + 0.94x_1 + 0.12x_2)$
  
  where the breakthrough rates for the original and alpha variants are assumed to be the same: 50% for partially vaccinated individuals and 6% for fully vaccinated individuals. For the delta variant, we assume a 64% breakthrough rate for partially vaccinated individuals and a 12% breakthrough rate for fully vaccinated individuals.

### Self-isolation until negative test
Another restriction that has been applied to travellers into NL is self-isolation until a negative test result. We assume this corresponds to 1-day of self-isolation, a test on day 0 of entering the province, and that days since exposure of arriving travellers are uniformly distributed from 0 to 10 days.  We assume that infectivity as a function of days since exposure follows a Weibull distribution with a shape parameter of 2.83 and a scale parameter of 5.67 (Ferretti/Hurford et al. 2021). After 1 day of self-isolation, the distribution of days since exposure is uniform from 1 to 11 days, however, since infectivity zero days from exposure is small, the Weibull distribution suggests that 1 day of self-isolation does not appreciably reduce the risk of infection spread to the community, relative to no self-isolation. The probability of a true positive when days since exposure is uniformly distribution from 0 to 10 is the mean of $t$, where $t_i$ is the probability of testing positive given infection $i$ days ago:
  
  \[
    t = [0,.19,.39,.58,.77,.73,.68,.64,.59,.55,.5]
    \]
(reference). As such, the probability that an infected traveller is released into the community given self-isolation until a negative test result is estimated as $0.49$.

```{r,echo=FALSE,results=FALSE}
dweibull(seq(0,10), 2.83, scale = 5.67, log = FALSE)
sum(dweibull(seq(1,11), 2.83, scale = 5.67, log = FALSE))
sum(dweibull(seq(8,18), 2.83, scale = 5.67, log = FALSE))
sum(dweibull(seq(6,16), 2.83, scale = 5.67, log = FALSE))
1-mean(c(.59,.55,.5))
```

### Testing on day 7, 8, or 9 and self-isolation until negative test
Another restriction is a test on day 7, 8, or 9 and isolation until a negative test result. We assume the test occurs on day 8 and the traveller exits self-isolation on day 9, if negative. By day 8, many travellers are no longer infective as some were several days post-exposure on arrival. More precisely, and as implied by the Weibull distribution assumption, after 8 days of self-isolation 11% of infectivity remains. When the test occurs on day 8, the probability of a false negative test is estimated as 0.45. Therefore, the probability that infectious travellers are released into the community given day 7-9 testing and self-isolation until a negative test result is $0.45s^v/0.89$. The requirement of testing more than offsets the reduced duration of self-isolation, such that the probability an infected traveller is released into the community with testing on day 8 is less than that of 14-day self-isolation, $s^v$.

Restrictions apply to travellers with given vaccination statuses. We calculate the vaccination status, and infecting variant, of a traveller given that they are infected as:
  
  $t_j^i = \frac{x_jv_iz_{i,j}}{T} \ \ \mbox{where} \ \ T = x_0 + x_1(0.5(v_0 + v_\alpha) + 0.64v_\delta) + x_2(0.06(v_0 + v_\alpha) + 0.88v_\delta)$
  
  such that $v_i$ is the frequency of variant $i$, $x_j$ is the fraction of travellers with vaccination status $j$, and $z_{i,j}$ is the breakthrough rate of the variant $i$ for individuals with vaccination status $j =1$ or $2$, or it is the increased transmissibility of the alpha and delta variants in unvaccinated individuals, $j=0$.

```{r, echo=FALSE, message = FALSE, warning=FALSE}
alpha.trans <- alpha.trans*masks
OV.trans <- OV.trans*masks
delta.trans <- delta.trans*masks
omicron.trans <-omicron.trans*masks

## Restrictions for unvaccinated travellers
m.unvax.0 <- rep(1/316/1.5, T)
m.unvax.a <- rep(1/316, T)
m.unvax.d <- rep(1.5/316,T)
m.unvax.o <- rep((1.5*1.2)/316,T)

# on August 1, test on day 8 + isolation to negative for unvaccinated
m.unvax.0[397:T] <- (0.45*m.unvax.0[397:T]/0.89)*OV.trans
m.unvax.a[397:T] <- (0.45*m.unvax.a[397:T]/0.89)*alpha.trans
m.unvax.d[397:T] <- (0.45*m.unvax.d[397:T]/0.89)*delta.trans
m.unvax.o[397:T] <- (0.45*m.unvax.o[397:T]/0.89)*omicron.trans

## Restrictions for travellers with 1 dose
# Same restrictions prior re-opening
m.1.0 <- m.unvax.0
m.1.a <- m.unvax.a
m.1.d <- m.unvax.d
m.1.o <- m.unvax.o

# July 1 - negative test step 1
m.1.0[366:T] <- 0.49*OV.trans
m.1.a[366:T] <- 0.49*alpha.trans
m.1.d[366:T] <- 0.49*delta.trans
m.1.o[366:T] <- 0.49*omicron.trans

# August 1 - no measures
m.1.0[397:T] <- OV.trans
m.1.a[397:T] <- alpha.trans
m.1.d[397:T] <- delta.trans
m.1.o[397:T] <- omicron.trans
# Sept 30 - same as unvaccinated
m.1.0[457:T] <- m.unvax.0[457:T]
m.1.a[457:T] <- m.unvax.a[457:T]
m.1.d[457:T] <- m.unvax.d[457:T]
m.1.o[457:T] <- m.unvax.o[457:T]

## Two dose travellers
# Same restrictions as other travellers prior to reopening
m.2.0 <- m.unvax.0
m.2.a <- m.unvax.a
m.2.d <- m.unvax.d
m.2.o <- m.unvax.o

# No requirements after July 1.
m.2.0[366:T] <- OV.trans
m.2.a[366:T] <- alpha.trans
m.2.d[366:T] <- delta.trans
m.2.o[366:T] <- omicron.trans

# Dec 18 5 RAT for arriving travllers (536) efficacy 0.07
# Dec 22 self-isolation of travellers (540) efficacy 0.4