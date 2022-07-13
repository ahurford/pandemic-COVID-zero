
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
mpearl<-read.csv('mount_pearl.csv',header=T)
mpearl$time=seq(1,length(mpearl$newcases))
theta<-list(beta0=0.765,delta=0.121,alpha=.44,changepoint=9,gamma=1/11,pi=0.6,
            omega=1/2,mu=2/30)
mpframo<-simulantro(4,theta,dim(mpearl)[1])
mObs<-with(mpframo,tapply(deltaNc,as.factor(time),mean))
mObs<-data.frame(time=seq(1,length(mObs)),mean=mObs)

ttmm<-c(1,15,27)
ttll<-mpearl$date[ttmm]
ggplot(mpframo,aes(x=time,y=deltaNc,group=run))+
    geom_line(lwd=0.25,colour='bisque2')+
    geom_line(aes(y=mean,x=time),lwd=1,colour='bisque3',
              data=mObs,inherit.aes=FALSE)+
   geom_point(aes(y=newcases,x=time),
              data=mpearl,inherit.aes=FALSE)+
    ylab('new daily confirmed cases')+
    scale_x_continuous(breaks=ttmm,labels=ttll)+
    theme(axis.text.x=element_text(angle=45,hjust=1))
    theme_bw()
ggsave(file='mpearl.svg',width=9,height=6)

