
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

########### STUFF THAT AMY ADDED
library(deSolve)

# Define the system of coupled ODEs for the Miller et al. model
SIR = function(t,y,parms){
    S <- y[1]
    I <- y[2]
    cumI <- y[3]
    
    
    dS = -S*beta.fun(t)*(IP+bC*IC+bA*IA)/N
    dE = S*beta.fun(t)*(IP+bC*IC+bA*IA)/N-deltaE*E
    dIP = r*deltaE*E-deltaP*IP
    dIC = deltaP*IP-deltaC*IC
    dIA = (1-r)*deltaE*E - deltaA*IA
    cumI = S*beta.fun(t)*(IP+bC*IC+bA*IA)/N
    
    return(list(c(dS,dI,cumI)))
}
