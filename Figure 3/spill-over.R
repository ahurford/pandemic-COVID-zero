# Figure 3: Probability of at least one community infection each week for Newfoundland and Labrador until Dec 15, 2022.

library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)
cb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
# Travel-related cases arriving in NL
NL.travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL-travel.csv')[,-1]%>%
  rename(date=REPORTED_DATE)
active <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/active.csv')[,-1]%>%
  filter(date>="2020-07-01" & date<"2021-12-25")
CCODWG = read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel.csv')[,-1]

NL_travel = CCODWG$NL_travel
ON2 = CCODWG$ON_active
AB2 = CCODWG$AB_active
NS2 = CCODWG$NS_active

mod = glm(NL_travel ~ 0+ON2+AB2+NS2, family = "poisson") 
c.ON = coef(mod)[1]
c.AB = coef(mod)[2]
c.NS = coef(mod)[3]

ON = active$ON_active
NS = active$NS_active
AB = active$AB_active
n = data.frame(date = active$date, n = exp(c.ON*ON + c.AB*AB + c.NS*NS))

obs.data = select(NL.travel, date,TRAVEL)
# To get the date where there were 0 travel-related cases in the data
obs.data = left_join(n,obs.data)%>%
  as.data.frame()%>%
  arrange(date)

obs.data[is.na(obs.data)] = 0
av_7 = data.frame(date = obs.data$date,av_7 = c(rep(NA,6),rollmean(obs.data$TRAVEL,7, align = "right")))

validation.data = obs.data%>%
  filter(date>"2021-05-31")%>%
  rename(validation.data = TRAVEL)%>%
  as.data.frame()

fitted.data  = obs.data%>%
  filter(date<="2021-05-31")%>%
  rename(fitted.data = TRAVEL)%>%
  as.data.frame()

data = left_join(n,fitted.data)%>%
  left_join(validation.data)%>%
  left_join(av_7)


gNL.tot =ggplot(data,aes(as.Date(date),group=1)) +
  geom_vline(xintercept = as.Date("2021-05-30"), col = cb[2],lty=2)+
  geom_point(aes(y=validation.data),color=cb[2], cex = 1) +
  geom_point(aes(y=fitted.data), color = cb[2], pch=1, cex=1)+
  geom_line(aes(y=n),col = "black", lwd=1) +
  geom_line(aes(y=av_7),color="grey", lwd=1)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("total travel-related (daily)")+
  ggtitle("Newfoundland and Labrador")+
  coord_cartesian(ylim=c(0, 20))+
  annotate("text", x = as.Date("2021-01-25"), y = 20, label = "Data used for model fitting", col = cb[2])+
  annotate("text", x = as.Date("2021-09-15"), y = 20, label = "Model predictions with\ndata shown for validation", col = cb[2], fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(0.9)), legend.title = element_blank(),legend.text=element_text(size=rel(1)),plot.title=element_text(size=rel(1)),axis.title = element_text(size=rel(1)))



