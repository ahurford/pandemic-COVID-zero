# Figure 3: Probability of at least one community infection each week for Newfoundland and Labrador until Dec 15, 2022.

library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)

# Travel-related cases arriving in NL
travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL-travel.csv')[,-1]%>%
  mutate(report_week = as.Date(cut(as.Date(REPORTED_DATE),"week", start.on.monday = F)))%>%
  group_by(report_week)%>%
  add_tally(TRAVEL)%>%
  select(report_week,n)%>%
  distinct()%>%
  as.data.frame()

travel$report_week = as.Date(travel$report_week)

active <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/active.csv')[,-1]
active$report_week=as.Date(active$report_week)
data = left_join(active, travel)%>%
  filter(report_week>"2020-07-01"& report_week <="2021-12-26" )
data[is.na(data)]=0

CCODWG = read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel.csv')[,-1]

NL_travel = CCODWG$NL_travel

ON2 = CCODWG$ON.active
AB2 = CCODWG$AB.active
NS2 = CCODWG$NS.active

mod = glm(NL_travel ~ 0+ON2+AB2+NS2, family = "poisson") 
c.ON = coef(mod)[1]
c.AB = coef(mod)[2]
c.NS = coef(mod)[3]

data = data.frame(week = as.Date(data$report_week), actual = data$n, predicted2 = exp(c.ON*ON+c.AB*AB+c.NS*NS))
validation = select(data, week,actual)%>%
  filter(week>"2021-05-30")%>%
  rename(validation = actual)
fitted  = select(data, week,actual)%>%
  filter(week<="2021-05-30")%>%
  rename(fitted = actual)

data = left_join(data,validation)%>%left_join(fitted)%>%
  select(week, predicted2, validation, fitted)

gNL.tot =ggplot(data,aes(week,group=1)) +
  geom_vline(xintercept = as.Date("2021-05-30"), col = cb[2],lty=2)+
  geom_line(aes(y=predicted2),color=cb[2]) +
  geom_point(aes(y=validation),color=cb[2]) +
  geom_point(aes(y=fitted), color = cb[2], pch=1)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("total travel-related (weekly)")+
  ggtitle("Newfoundland and Labrador")+
  coord_cartesian(ylim=c(0, 100))+
  annotate("text", x = as.Date("2021-01-25"), y = 60, label = "Data used for model fitting", col = cb[2])+
  annotate("text", x = as.Date("2021-09-15"), y = 60, label = "Model predictions with\ndata shown for validation", col = cb[2], fontface=2)+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(0.9)), legend.title = element_blank(),legend.text=element_text(size=rel(1)),plot.title=element_text(size=rel(1)),axis.title = element_text(size=rel(1)))

theme(axis.text.x = element_text(angle = 90, size=rel(0.9)), legend.title = element_blank(),legend.text=element_text(size=rel(1)),plot.title=element_text(size=rel(.9)),axis.title = element_text(size=rel(.8)))

