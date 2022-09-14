
#theta<-c(beta0,delta,alpha,changepoint,...)
betaSIR<-function(t,theta){
    if(t< theta$changepoint) return(theta$beta0)
    theta$beta0*(theta$delta+
            (1-theta$delta)*exp(-theta$alpha*(t-theta$changepoint)))
}

#theta<-c(beta0,delta,alpha,changepoint,gamma,pi,omega,mu...)
mpearl<-read.csv('~/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/data/mount_pearl.csv',header=T)
theta<-list(beta0=0.35,delta=0.4,alpha=0.2,changepoint=10,gamma=1/10,pi=0.6,
            omega=1/2,mu=0)

#theta<-c(beta0,delta,changepoint,gamma,pi,omega,mu,...)
sir.simula<-function(i0,theta,time.horizon){
    dN1<-rep(0,time.horizon)
    dN2<-rep(0,time.horizon)
    deltaNc<-rep(0,time.horizon)
    iu0<-i0*(1-theta$pi)/theta$pi
    ic0<-i0
    deltaNc[1]<-ic0
    for(k in seq(1,time.horizon)){
        dN1o<-rpois(1,betaSIR(k,theta)*(iu0+theta$omega*ic0)+theta$mu)
        dNc1<-theta$pi*dN1o
        dNu1<-(1-theta$pi)*dN1o
        dN2o<-rpois(1,theta$gamma*(ic0+iu0))
        ic0<-max(0,ic0+dNc1-theta$pi*dN2o)
        iu0<-max(0,iu0+dNu1-(1-theta$pi)*dN2o)
        dN1[k]<-dN1o
        dN2[k]<-dN2o
        deltaNc[k]<-dNc1
    }
    return(data.frame(time=seq(1,time.horizon),
                      deltaN1=dN1,deltaN2=dN2,deltaNc=deltaNc))
}

simulantro<-function(i0,theta,time.horizon,runs=500){
    risu<-data.frame()
    for(i in 1:runs){
        risu0<-sir.simula(i0,theta,time.horizon)
        risu<-rbind(risu,cbind(risu0,run=as.factor(i)))
    }
    return(risu)
}

library(ggplot2)
library(scales) 
library(patchwork)

mpearl$time=seq(1,length(mpearl$newcases))
theta<-list(beta0=0.89,delta=0.03,alpha=.325,changepoint=8,gamma=1/10,pi=0.6,
            omega=1/2,mu=0)
mpframo<-simulantro(4,theta,dim(mpearl)[1])
mAlpha<-with(mpframo,tapply(deltaNc,as.factor(time),mean))
mAlpha.min <-with(mpframo,tapply(deltaNc,as.factor(time),min))
mAlpha.max <-with(mpframo,tapply(deltaNc,as.factor(time),max))
mObs<-data.frame(date=as.Date(mpearl$date), alpha = unname(mAlpha), alpha.min = unname(mAlpha.min), alpha.max = unname(mAlpha.max))

thOrig<-theta
thOrig$beta0<-theta$beta0/1.5
mOrig<-simulantro(4,thOrig,dim(mpearl)[1])
mOrig<-with(mOrig,tapply(deltaNc,as.factor(time),mean))
mObs<-data.frame(mObs,OV = unname(mOrig))

thDelta<-theta
thDelta$beta0<-theta$beta0/1.5*1.97
mDelta<-simulantro(4,thDelta,dim(mpearl)[1])
mDelta<-with(mDelta,tapply(deltaNc,as.factor(time),mean))
mObs<-data.frame(mObs, Delta = unname(mDelta))

thOmicron<-theta
thOmicron$beta0<-theta$beta0/1.5*1.97
mOmicron<-simulantro(4,thOmicron,dim(mpearl)[1])
mOmicron<-with(mOmicron,tapply(deltaNc,as.factor(time),mean))
mObs<-data.frame(mObs, Omicron = unname(mOmicron))
mObs<-data.frame(mObs, MtPearl = mpearl$newcases)

g1=ggplot(mObs,aes(x=as.Date(date),group=1))+
    geom_ribbon(aes(ymin = alpha.min, ymax = alpha.max), fill = palette.colors(4)[4], alpha=.2)+
  geom_line(aes(y=alpha), col = palette.colors(4)[4], lwd=1)+
  geom_line(aes(y=Delta), col = palette.colors(3)[3])+
  geom_line(aes(y=Omicron), col = palette.colors(7)[7])+
  geom_line(aes(y=OV), col = "darkgrey")+
  geom_point(aes(y=MtPearl), col = palette.colors(4)[4])+
    ylab('new reported cases (daily)')+
      scale_x_date(breaks = date_breaks("1 days"),
                   labels = date_format("%d %b"))+
  coord_cartesian(ylim = c(0,200))+
  annotate("text", x = as.Date("2021-02-10"), y = 65, label = "Alpha", fontface=1, col=palette.colors(4)[4])+
  annotate("text", x = as.Date("2021-02-15"), y = 170, label = "BA.1", fontface=1, col=palette.colors(7)[7])+
  annotate("text", x = as.Date("2021-02-15"), y = 150, label = "Delta", fontface=1, col=palette.colors(3)[3])+
  annotate("text", x = as.Date("2021-02-10"), y = 2, label = "Original", fontface=1, col="darkgrey")+
  ggtitle("Mt. Pearl, NL, 2021 - Variant scenarios")+
  xlab("Date of symptom onset")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size=rel(1)))

#ggsave(file='mpearl1.png',width=9,height=6)




x0<-seq(0,1,0.05)
bAlpha<-theta$beta0*(x0+(1-x0)*.07)
bWild<-bAlpha/1.5
bDelta<-theta$beta0/1.5*1.97*(x0+(1-x0)*.12)
bOmicron<-theta$beta0/1.5*1.97*(x0+(1-x0)*.91)
fsAlpha<-rep(NA,21)
fsWild<-rep(NA,21)
fsDelta<-rep(NA,21)
fsOmicron<-rep(NA,21)
th0<-theta
for(k in 1:21){
    th0$beta0<-bAlpha[k]
    mm0<-simulantro(4,th0,dim(mpearl)[1])
    fsAlpha[k]<-sum(with(mm0,tapply(deltaNc,as.factor(time),mean)))
    th0$beta0<-bWild[k]
    mm0<-simulantro(4,th0,dim(mpearl)[1])
    fsWild[k]<-sum(with(mm0,tapply(deltaNc,as.factor(time),mean)))
    th0$beta0<-bDelta[k]
    mm0<-simulantro(4,th0,dim(mpearl)[1])
    fsDelta[k]<-sum(with(mm0,tapply(deltaNc,as.factor(time),mean)))
    th0$beta0<-bOmicron[k]
    mm0<-simulantro(4,th0,dim(mpearl)[1])
    fsOmicron[k]<-sum(with(mm0,tapply(deltaNc,as.factor(time),mean)))
}
rm(th0)
# mm0<-data.frame(unvaxo=x0,finalsize=fsAlpha,variant='Alpha')
# mm0<-rbind(mm0,data.frame(unvaxo=x0,finalsize=fsWild,variant='OV'),
#            data.frame(unvaxo=x0,finalsize=fsDelta,variant='Delta'),
#            data.frame(unvaxo=x0,finalsize=fsOmicron,variant='Omicron'))

mm0 = data.frame(full.vax = 100*(1-x0), alpha = fsAlpha, OV = fsWild, delta = fsDelta, omicron = fsOmicron)

g2=ggplot(mm0,aes(x=full.vax,group=1))+
    geom_line(aes(y=alpha), col = palette.colors(4)[4], lwd=1)+
  geom_line(aes(y=delta), col = palette.colors(3)[3], lwd=1)+
  geom_line(aes(y=omicron), col = palette.colors(7)[7],lwd=1)+
  geom_line(aes(y=OV), col ="darkgrey", lwd=1)+
  geom_hline(yintercept = sum(mpearl$newcases), lty = 2, col = palette.colors(4)[4])+
    ylab('Mean reported cases (after 27 days)')+
  coord_cartesian(ylim = c(0,2000))+
  ggtitle("Mt. Pearl, NL, 2021 - Vaccination scenarios")+
  annotate("text", x = 65, y = 530, label = "472 cases in the Mt. Pearl outbreak", fontface=1, col=palette.colors(4)[4])+
  annotate("text", x = 25, y = 260, label = "Alpha", fontface=1, col=palette.colors(4)[4])+
  annotate("text", x = 100, y = 1350, label = "BA.1", fontface=1, col=palette.colors(7)[7])+
  annotate("text", x = 21, y = 1000, label = "Delta", fontface=1, col=palette.colors(3)[3])+
  annotate("text", x = 5, y = 160, label = "Original", fontface=1, col="darkgrey")+
    xlab('% population with 2 doses of vaccine')+theme_classic()

g1+g2+plot_annotation(tag_levels = 'A')
ggsave(file='~/Desktop/mpearl2.png', width=10, height=5)
