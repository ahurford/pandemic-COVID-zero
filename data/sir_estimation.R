
#theta<-c(beta0,delta,changepoint,...)
betaSIR<-function(t,theta){
    if(t< theta[3]) return(theta[1])
    return(theta[1]*theta[2])
}

#theta<-c(beta0,delta,changepoint,gamma,pi,omega,...)
sir.prediction<-function(newinfections,i0,theta){
    time.horizon<-length(newinfections)+1
    Ic<-rep(0,time.horizon)
    Iu<-rep(0,time.horizon)
    deltaN1<-rep(0,time.horizon)
    iu0<-i0*(1-theta[5])/theta[5]
    ic0<-i0
    Ic[1]<-ic0
    Iu[1]<-iu0
    deltaN1[1]<-ic0+iu0
    for(k in seq(2,time.horizon)){
        dNu1<-(1-theta[5])*betaSIR(k,theta)*(iu0+theta[6]*ic0)
        if(k> 1/theta[4]) dN2<-theta[4]*(ic0+iu0)
        else dN2<-0
        ic0<-max(0,ic0+newinfections[k-1]-theta[5]*dN2)
        iu0<-max(0,iu0+dNu1-(1-theta[5])*dN2)
        Ic[k]<-ic0
        Iu[k]<-iu0
        deltaN1[k]<-dNu1+newinfections[k-1]
    }
    return(list(Ic=Ic,Iu=Iu,deltaN1=deltaN1))
}


sir.lee<-function(prediction,theta){
    K<-length(prediction$Ic)
    sn0<-sum(prediction$deltaN1)
    il0<-0
    for(k in 1:K){
        il0<-il0+betaSIR(k,theta)*(prediction$Iu[k] + theta[6]*prediction$Ic[k])
    }
    sn1<-sum(prediction$deltaN0[theta[3]:K])
    il1<-0
    for(k in theta[3]:K){
        il1<-il1+betaSIR(k,theta)**(prediction$Iu[k] + theta[6]*prediction$Ic[k])
    }
    return(c((sn0-il0)/theta[1],(sn1-il1)/theta[2]))   
}

sir.hessian<-function(prediction,theta){
    K<-length(prediction$Ic)
    hessi<-matrix(rep(0,4),ncol=2)
    hessi[1,1]<- -sum(prediction$deltaN1)/theta[1]^2
    hessi[2,2]<- -sum(prediction$deltaN1[theta[3]:K])/theta[2]^2
    il1<-0
    for(k in theta[3]:K){
        il1<-il1-(prediction$Iu[k] + theta[6]*prediction$Ic[k])
    }
    hessi[1,2]<- il1
    hessi[2,1]<- il1
    return(hessi)
}



library(matlib)
#theta<-c(beta0,delta,changepoint,gamma,pi,omega,...)
mpearl<-read.csv('/Users/ahurford/Desktop/Work/Research/Research_Projects/2022/reopening/pandemic-COVID-zero/Data/mount_pearl.csv',header=T)
theta<-c(0.7,0.1,12,1/10,0.6,1/2)
for(i in 1:50){
qq<-sir.prediction(mpearl$newcases,2,theta)
theta[1:2]<-theta[1:2]-inv(sir.hessian(qq,theta))%*%sir.lee(qq,theta)
print(theta)
}
mpearl.theta<-theta
theta[1]/theta[4]

########### SIMULATION
i0 <- mpearl$newcases[1]
numsims <- 1000
time <- seq(1,length(qq$Ic))
sim.new.cases <- matrix(NA,nrow=length(time), ncol=numsims)

for(i in seq(1,numsims)){
    Ic <- i0/theta[5]
    Iu <- i0*(1-theta[5])/theta[5]
    for(t in time){
        lambda1 = round(betaSIR(t,theta)*(Iu + theta[5]*Ic))
        lambda2c = round(theta[4]*Ic)
        lambda2u = round(theta[4]*Iu)
        deltaN1 = rpois(1,lambda1)
        deltaN2u = rpois(1,lambda2u)
        deltaN2c = rpois(1,lambda2c)
        deltaIc = rbinom(1,deltaN1,theta[5])
        Ic = Ic + deltaIc - deltaN2c
        Iu = Iu + (deltaN1 - deltaIc) - deltaN2u
        sim.new.cases[t,i] = deltaIc
    }
}

final.size = colSums(sim.new.cases)
sort.final.size = sort(final.size)

ilow = min(which(final.size==sort.final.size[25]))
imax = min(which(final.size==sort.final.size[975]))
plot(mpearl$newcases, ylim = c(0,100))
lines(qq$deltaN1*theta[5])
lines(sim.new.cases[,ilow],lty=2)
lines(sim.new.cases[,imax],lty=2)

theta[1] = 2.5*theta[1]
qq2 = sir.prediction(mpearl$newcases,2,theta)
lines(qq2$deltaN1*theta[5], col = "red")

