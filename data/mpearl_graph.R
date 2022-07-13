
#theta<-c(beta0,delta,alpha,changepoint,...)
betaSIR<-function(t,theta){
    if(t< theta$changepoint) return(theta$beta0)
    theta$beta0*(theta$delta+
            (1-theta$delta)*exp(-theta$alpha*(t-theta$changepoint)))
}


#################################################

#theta<-c(beta0,delta,alpha,changepoint,gamma,pi,omega,mu...)
mpearl<-read.csv('mount_pearl.csv',header=T)
theta<-list(beta0=0.35,delta=0.4,alpha=0.2,changepoint=10,gamma=1/10,pi=0.6,
            omega=1/2,mu=2/30)

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

simulantro<-function(i0,theta,time.horizon,runs=100){
    risu<-data.frame()
    for(i in 1:runs){
        risu0<-sir.simula(i0,theta,time.horizon)
        risu<-rbind(risu,cbind(risu0,run=as.factor(i)))
    }
    return(risu)
}

library(ggplot2)
library(scales) 
mpearl<-read.csv('mount_pearl.csv',header=T)
mpearl$time=seq(1,length(mpearl$newcases))
theta<-list(beta0=0.75,delta=0.091,alpha=.34,changepoint=9,gamma=1/11,pi=0.6,
            omega=1/2,mu=3.5/30)
mpframo<-simulantro(4,theta,dim(mpearl)[1])
mAlpha<-with(mpframo,tapply(deltaNc,as.factor(time),mean))
mObs<-data.frame(time=seq(1,length(mAlpha)),mean=mAlpha,variant='alpha')

ttmm<-c(1,8,15,22)
ttll<-c('1 Feb 2021', '15 Feb 2021', '15 Feb 2021', '22 Feb 2021')

ggplot(mpframo,aes(x=time,y=deltaNc,group=run))+
    geom_line(lwd=0.25,colour='bisque2')+
    geom_line(aes(y=mean,x=time),lwd=1,colour='coral2',
              data=mObs,inherit.aes=FALSE)+
   geom_point(aes(y=newcases,x=time),
              data=mpearl,inherit.aes=FALSE)+
    ylab('new daily confirmed cases')+
    scale_x_continuous(breaks = ttmm,
               labels = ttll)+
    theme(axis.text.x = element_text(angle = 90, size=rel(1)))
ggsave(file='mpearl.svg',width=9,height=6)


thOrig<-theta
thOrig$beta0<-theta$beta0/1.29
mOrig<-simulantro(4,thOrig,dim(mpearl)[1])
mOrig<-with(mOrig,tapply(deltaNc,as.factor(time),mean))
mObs<-rbind(mObs,
  data.frame(time=seq(1,length(mOrig)),mean=mOrig,variant='wild'))

thDelta<-theta
thDelta$beta0<-theta$beta0/1.29*1.97
mDelta<-simulantro(4,thDelta,dim(mpearl)[1])
mDelta<-with(mDelta,tapply(deltaNc,as.factor(time),mean))
mObs<-rbind(mObs,
  data.frame(time=seq(1,length(mDelta)),mean=mDelta,variant='delta'))


ggplot(mpframo,aes(x=time,y=deltaNc,group=run))+
    geom_line(lwd=0.25,colour='bisque2')+
    geom_line(aes(y=mean,x=time,group=variant,colour=variant),lwd=1,
              data=mObs,inherit.aes=FALSE)+
   geom_point(aes(y=newcases,x=time),
              data=mpearl,inherit.aes=FALSE)+
    ylab('new daily confirmed cases')+
    scale_x_continuous(breaks = ttmm,
               labels = ttll)+
    theme(axis.text.x = element_text(angle = 90, size=rel(1)))
ggsave(file='mpearl1.svg',width=9,height=6)




x0<-seq(0,1,0.05)
bAlpha<-theta$beta0*(x0+(1-x0)*.07)
bWild<-bAlpha/1.29
bDelta<-theta$beta0/1.29*1.97*(x0+(1-x0)*.12)
bOmicron<-theta$beta0/1.29*1.24*(x0+(1-x0)*.91)
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
mm0<-data.frame(unvaxo=x0,finalsize=fsAlpha,variant='alpha')
mm0<-rbind(mm0,data.frame(unvaxo=x0,finalsize=fsWild,variant='wild'),
           data.frame(unvaxo=x0,finalsize=fsDelta,variant='delta'),
           data.frame(unvaxo=x0,finalsize=fsOmicron,variant='omicron'))

ggplot(mm0,aes(x=unvaxo,y=finalsize,group=variant,colour=variant))+
    geom_line(lwd=1)+
    ylab('expected final count of confirmed cases')+
    xlab('proportion of unvaxinated individuals')

ggsave(file='mpearl2.svg',width=9,height=6)
