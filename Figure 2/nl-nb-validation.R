library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)
cb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
# Travel-related cases arriving in NL
NL.travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL_validation.csv')[-1]
NB.travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NB_validation.csv')[,-1]
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

obs.data = dplyr::select(NL.travel, date,TRAVEL)
# To get the date where there were 0 travel-related cases in the data
obs.data = left_join(n,obs.data)%>%
  as.data.frame()%>%
  arrange(date)

obs.data[is.na(obs.data)] = 0
av_7 = data.frame(date = obs.data$date,av_7 = c(rep(NA,6),rollmean(obs.data$TRAVEL,7, align = "right")))

data = left_join(n,obs.data)%>%
  left_join(av_7)

i=which(data$date == "2021-05-31")
shading = rep(0,length(data$date))
shading[1:i] = 25
gNL.tot =ggplot(data,aes(as.Date(date),group=1)) +
  geom_ribbon(aes(ymax = shading, ymin=0), fill="grey", alpha = 0.3)+
  geom_ribbon(aes(ymax=TRAVEL, ymin = 0),fill=cb[2], alpha=0.3) +
  geom_line(aes(y=av_7),color=cb[2])+
  geom_line(aes(y=n),col = "black") +
  geom_point(aes(y=TRAVEL), col=cb[2],alpha=0.5, cex=1)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("travel-related cases (daily)")+
  ggtitle("Newfoundland and Labrador Centre for Health Information data")+
  coord_cartesian(ylim=c(0, 25))+
  annotate("text", x = as.Date("2020-12-01"), y = 20, label = "Date range of CCODWG used\nfor model fitting", col = "grey32")+
  annotate("text", x = as.Date("2021-09-25"), y = 20, label = "Model predictions with\ndata shown for validation", col = "black")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))



