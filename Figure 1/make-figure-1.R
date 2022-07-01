library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)
library(imputeTS)
library(patchwork)

## Color map
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
travel_wk <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel_wk.csv')[,-1]
travel_day <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel_day.csv')[,-1]


make.plot = function(m1,new_cases,travel,contacts, col1){
g1=ggplot(travel_wk,aes(as.Date(report_week),group=1)) +
  geom_ribbon(aes(ymax = new_cases, ymin=travel+contacts), fill = col1, alpha = 0.4)+
  geom_ribbon(aes(ymax =travel+contacts,ymin = travel), fill = col1, alpha = 0.6)+
  geom_ribbon(aes(ymax =travel,ymin = 0), fill = col1, alpha = 1)+
  geom_line(aes(y=travel+contacts), col = "black",lty=3)+
  geom_line(aes(y=travel), col = "black")+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  coord_cartesian(ylim = c(0,m1))+
  xlab("") +
  ylab("cases (weekly)")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))
return(g1)
}


g.NL = make.plot(m1=1.5*(max(travel_wk$NL_contacts+travel_wk$NL_travel)),new_cases=travel_wk$NL_new_cases,travel=travel_wk$NL_travel,contacts=travel_wk$NL_contacts, col1 = cb[2])+ggtitle("Newfoundland and Labrador")+
  annotate("segment", x = as.Date("2020-12-01"), xend = as.Date("2021-02-15"), y = 80, yend = 80,
           colour = cb[2],size=0.5)+
  annotate("text", x = as.Date("2020-10-15"),y = 80,
           label = "community",size=3, col = cb[2])+
  annotate("text", x = as.Date("2020-10-15"),y = 60,
           label = "importations",size=3, col = "black")+
  annotate("text", x = as.Date("2020-10-15"),y = 40,
           label = "contacts",size=3, col = "black")+
  annotate("segment", x = as.Date("2020-11-01"), xend = as.Date("2020-11-20"), y = 32, yend = 20,
                                                             colour = "black",size=0.5, lty=3)+
  annotate("segment", x = as.Date("2020-12-01"), xend = as.Date("2020-12-10"), y = 52, yend = 27,
           colour = "black",size=0.5)

g.NS = make.plot(m1=1.5*(max(travel_wk$NS_contacts+travel_wk$NS_travel)),new_cases=travel_wk$NS_new_cases,travel=travel_wk$NS_travel,contacts=travel_wk$NS_contacts, col1 = cb[3])+ggtitle("Nova Scotia")

g.PEI = make.plot(m1=1.5*(max(travel_wk$PEI_contacts+travel_wk$PEI_travel)),new_cases=travel_wk$PEI_new_cases,travel=travel_wk$PEI_travel,contacts=travel_wk$PEI_contacts, col1 = cb[5])+ggtitle("Prince Edward Island")

g.NWT = make.plot(m1=1.5*(max(travel_wk$NWT_contacts+travel_wk$NWT_travel)),new_cases=travel_wk$NWT_new_cases,travel=travel_wk$NWT_travel,contacts=travel_wk$NWT_contacts,col1 = cb[4])+ggtitle("Northwest territories")

g.NB = make.plot(m1=1.5*(max(travel_wk$NB_contacts+travel_wk$NB_travel)),new_cases=travel_wk$NB_new_cases,travel=travel_wk$NB_travel,contacts=travel_wk$NB_contacts, col1 = cb[1])+ggtitle("New Brunswick")

g.YT = make.plot(m1=1.5*(max(travel_wk$YT_contacts+travel_wk$YT_travel)),new_cases=travel_wk$YT_new_cases,travel=travel_wk$YT_travel,contacts=travel_wk$YT_contacts, col1 = cb[6])+ggtitle("Yukon")

NL_contacts = sum(travel_day$NL_contacts)
NL_travel = sum(travel_day$NL_travel)
NS_contacts = sum(travel_day$NS_contacts)
NS_travel = sum(travel_day$NS_travel)
NB_contacts = sum(travel_day$NB_contacts)
NB_travel = sum(travel_day$NB_travel)
PEI_contacts = sum(travel_day$PEI_contacts)
PEI_travel = sum(travel_day$PEI_travel)
YT_contacts = sum(travel_day$YT_contacts)
YT_travel = sum(travel_day$YT_travel)
NWT_contacts = sum(travel_day$NWT_contacts)
NWT_travel = sum(travel_day$NWT_travel)

type = c(rep("importation",6), rep("contact",6))
value = c(NL_travel, NS_travel, NB_travel, PEI_travel, YT_travel, NWT_travel, NL_contacts, NS_contacts, NB_contacts, PEI_contacts, YT_contacts, NWT_contacts)
province = rep(c("NL", "NS", "NB", "PE", "YT", "NT"),2)

travel_related = data.frame(province=province, value = value, type = type)

cb2 = c(cb[2], cb[3], cb[1], cb[5], cb[4], cb[6])
g.sum = ggplot(travel_related, aes(x=reorder(province, rep(c(1,2,4,3,5,6),2))))+
  geom_bar(aes(y=value), stat="identity", fill = c(cb2,cb2), alpha = c(rep(1,6), rep(0.5, 6)),col="black", lty = c(rep(1,6), rep(3,6)))+theme_classic()+
  xlab("")+ylab("total")+
  annotate("text", x = "NL", y = 350, label = "contacts", size=2.4)+
  annotate("text", x = "NL", y = 150, label = "importations", size=2.3)

NL.perc = 100*(travel_day$NL_travel+travel_day$NL_contacts)/travel_day$NL_new_cases
NL.perc = NL.perc[!is.na(NL.perc)&is.finite(NL.perc)]
NL = rep("NL", length(NL.perc))
NL.order = rep(1,length(NL))
NL.col = rep(cb[2],length(NL))

NB.perc = 100*(travel_day$NB_travel+travel_day$NB_contacts)/travel_day$NB_new_cases
NB.perc = NB.perc[!is.na(NB.perc)&is.finite(NB.perc)]
NB = rep("NB", length(NB.perc))
NB.order = rep(4,length(NB))
NB.col = rep(cb[1],length(NB))

NS.perc = 100*(travel_day$NS_travel+travel_day$NS_contacts)/travel_day$NS_new_cases
NS.perc = NS.perc[!is.na(NS.perc)&is.finite(NS.perc)]
NS = rep("NS", length(NS.perc))
NS.order = rep(2,length(NS))
NS.col = rep(cb[3],length(NS))

PE.perc = 100*(travel_day$PEI_travel+travel_day$PEI_contacts)/travel_day$PEI_new_cases
PE.perc = PE.perc[!is.na(PE.perc)&is.finite(PE.perc)]
PE = rep("PE", length(PE.perc))
PE.order = rep(3,length(PE))
PE.col = rep(cb[5],length(PE))

YT.perc = 100*(travel_day$YT_travel+travel_day$YT_contacts)/travel_day$YT_new_cases
YT.perc = YT.perc[!is.na(YT.perc)&is.finite(YT.perc)]
YT = rep("YT", length(YT.perc))
YT.order = rep(6,length(YT))
YT.col = rep(cb[6],length(YT))

NT.perc = 100*(travel_day$NWT_travel+travel_day$NWT_contacts)/travel_day$NWT_new_cases
NT.perc = NT.perc[!is.na(NT.perc)&is.finite(NT.perc)]
NT = rep("NT", length(NT.perc))
NT.order = rep(5,length(NT))
NT.col = rep(cb[4],length(NT))

daily.per.travel = data.frame(province = c(NL, NS, PE, NB, NT, YT),
                              percent.travel = c(NL.perc, NS.perc, PE.perc, NB.perc, NT.perc, YT.perc))
daily.per.travel$percent.travel[daily.per.travel$percent.travel>100]=100

g.per = ggplot(daily.per.travel, aes(x=reorder(province, c(NL.order, NS.order, PE.order, NB.order, NT.order, YT.order)), y=percent.travel)) + 
  geom_jitter(position=position_jitter(0.2), col = c(NL.col, NS.col, PE.col, NB.col, NT.col, YT.col), cex=1)+
  ylab("% travel-related (daily)")+
  xlab("")+
  stat_summary(fun = "median", bg = c(cb[2], cb[3], cb[5], cb[1], cb[4], cb[6]), col = c(cb[2], cb[3], cb[5], cb[1], cb[4], cb[6]),cex = 8,pch=21, geom = "point",alpha=0.5)+theme_classic()
  
### All plots together
(g.NL+g.NS+g.PEI)/(g.NB+g.NWT+g.YT)/(g.per+g.sum)+ plot_annotation(tag_levels = 'A')
ggsave("travel-related.png")
