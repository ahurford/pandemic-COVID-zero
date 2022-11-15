library(ggplot2)
library(scales)
library(zoo)
library(bbmle)
library(dplyr)
library(zetadiv)
cb = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
# Travel-related cases arriving in NL
NL.travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NL_validation.csv')[-1]
NB.travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NB_validation.csv')[,-1]
CCODWG = read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/travel.csv')[,-1]
new <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/new.csv')[,-1]%>%
  filter(date<"2021-12-25")
# Validation figures
CCODWG_shift = tail(NB.travel$CCODWG,-1)
NB.travel = head(NB.travel,-1)
NB.travel$CCODWG = CCODWG_shift
NB.travel = data.frame(NB.travel, diff = NB.travel$CCODWG-NB.travel$NB.govt)
NL.travel = data.frame(NL.travel, diff = NL.travel$CCODWG-NL.travel$NLCHI)

gNB.1 =ggplot(NB.travel,aes(x=as.Date(date_report)))+
  geom_ribbon(aes(ymax=NB.govt, ymin=0), fill="black", alpha = 0.5)+
  geom_line(aes(y=CCODWG), col="black",lwd=1)+
scale_x_date(breaks = date_breaks("1 month"),
             labels = date_format("%b %Y"))+
  xlab("") +
  ylab("imported cases (daily)")+
  ggtitle("Validation of CCODWG NB importation data")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1)))


gNB.2 = ggplot(NB.travel, aes(diff)) +
  geom_histogram(binwidth = 1, aes(y = after_stat(count / sum(count))))+
  ylab("Frequency")+
  xlab("CCODWG - NB government")+
  theme_classic()+ theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1)))

gNL.1 =ggplot(NL.travel,aes(x=as.Date(date_report)))+
  geom_ribbon(aes(ymax=NLCHI, ymin=0), fill=cb[2], alpha = 0.5)+
  geom_line(aes(y=CCODWG), col=cb[2], lwd=1)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %Y"))+
  xlab("") +
  ylab("imported cases (daily)")+
  ggtitle("Validation of CCODWG NL importation data")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1)))


gNL.2 = ggplot(NL.travel, aes(diff)) +
  geom_histogram(binwidth = 1, aes(y = after_stat(count / sum(count))), fill = cb[2])+
  ylab("Frequency")+
  xlab("CCODWG - NLCHI")+
  theme_classic()+theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(.8)))

######
NL_travel = tail(CCODWG$NL_travel,-13)
ON2 = rollmean(CCODWG$ON_new,14)
AB2 = rollmean(CCODWG$AB_new,14)
NS2 = rollmean(CCODWG$NS_new,14)
BC2 = rollmean(CCODWG$BC_new,14)
QC2 = rollmean(CCODWG$QC_new,14)
MB2 = rollmean(CCODWG$MB_new,14)
SK2 = rollmean(CCODWG$SK_new,14)
NB2 = rollmean(CCODWG$NB_new,14)


mod = glm.cons(NL_travel ~ ON2+AB2+NS2+QC2+BC2+SK2+NB2+MB2,cons=1,family = "poisson")
mod = glm.cons(NL_travel ~ NS2,cons=1,family = "poisson")


NS = tail(new$NS_new,-13)
intcp = coef(mod)[1]
cNS = coef(mod)[2]

n = data.frame(date = tail(new$date,-13), n = exp(intcp+cNS*NS))

NL.travel <- read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/NLCHI_cases.csv')[-1]%>%
  rename(date = REPORTED_DATE)%>%
  filter(date < "2021-12-25")

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
  ylab("imported cases (daily)")+
  ggtitle("Statistical model of importations to NL")+
  coord_cartesian(ylim=c(0, 25))+
  annotate("text", x = as.Date("2020-12-01"), y = 20, label = "Date range of CCODWG used\nfor model fitting", col = "grey32")+
  annotate("text", x = as.Date("2021-09-25"), y = 20, label = "Model predictions with\n NLCHI data shown for\nvalidation", col = "black")+
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1)), legend.title = element_blank(),legend.text=element_text(size=rel(1.2)),plot.title=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.2)))

gNL.tot
ggsave("Desktop/importation_NL.png", width = 10, height=5)

g=gNL.1+gNL.2+gNB.1 + gNB.2 
g+plot_layout(widths = c(2, 1)) + plot_annotation(tag_levels = 'A') 
ggsave("importation_validation.png", width=12)
