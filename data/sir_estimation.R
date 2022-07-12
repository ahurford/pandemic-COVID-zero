
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
qq<-sir.prediction(mpearl$newcases,mpearl$newcases[1],theta)
theta[1:2]<-theta[1:2]-inv(sir.hessian(qq,theta))%*%sir.lee(qq,theta)
print(theta)
}
mpearl.theta<-theta
theta[1]/theta[4]


# plot the data
plot(mpearl$newcases, ylim =c(0,60))
lines(qq$deltaN1*theta[5])

# JC - just ignore this... it is a failed experiment.
#Prediction but without conditioning on last data point
sir.prediction2<-function(T,i0,theta){
    time.horizon<-T+1
    Ic<-rep(0,time.horizon)
    Iu<-rep(0,time.horizon)
    deltaN1c<-rep(0,time.horizon)
    deltaN1<-rep(0,time.horizon)
    iu0<-i0*(1-theta[5])/theta[5]
    ic0<-i0
    Ic[1]<-ic0
    Iu[1]<-iu0
    deltaN1c[1]<-ic0
    for(k in seq(2,time.horizon)){
        dN1<-betaSIR(k,theta)*(iu0+theta[6]*ic0)
        dN1c<-theta[5]*dN1
        dN1u<-dN1-dN1c
        if(k> 1/theta[4]) dN2<-theta[4]*(ic0+iu0)
        else dN2<-0
        ic0<-max(0,ic0+dN1c-theta[5]*dN2)
        iu0<-max(0,iu0+dN1u-(1-theta[5])*dN2)
        Ic[k]<-ic0
        Iu[k]<-iu0
        deltaN1c[k]<-deltaN1c[k]+dN1c
    }
    return(list(Ic=Ic,Iu=Iu,deltaN1c=deltaN1c))
}
T=length(mpearl$newcases)
res=sir.prediction2(T,mpearl$newcases[1],theta)
#lines(res$deltaN1c, lty=2)


##### JC start here...
## Calibrated ODE fit
require(deSolve)

# Redefining betaSIR - it is exponential decay after the changepoint at rate theta[7] and levels off at theta[2]*theta[1]
betaSIR = function(t,theta){
if(t< theta[3]) return(theta[1])
return(theta[1]*theta[2]+(theta[1]-theta[1]*theta[2])*exp(-theta[7]*(t-theta[3])))
}

# Here goes numerically solving the ODE (which is linear, but I was experimenting with non-linear)
SIR = function(t,y,parms){
I = y[1]
dI = betaSIR(t,theta)*I*(theta[5]*theta[6]+(1-theta[5])) - theta[4]*I
dcumI = betaSIR(t,theta)*I*(theta[5]*theta[6]+(1-theta[5])) 
return(list(c(dI,dcumI)))
}

# define time with increments less than 1 to have a nice smooth curve
incr = 0.1
times = seq(1,length(mpearl$newcases),incr)
yini  = c(I=mpearl$newcases[1],cumI = mpearl$newcases[1])

# I think we should just fix this at 8 days and not fit it.
theta[3]<-8

# 3 calibrated parameters
theta[1]<-0.90819869
theta[2]<-0.03333333
phi = 0.325
theta<-c(theta,phi)

make.graph = function(col){
out2 <- ode(y = yini, parms = NULL, times=times, func = SIR)
out2 <- data.frame(out2)
new.cases = tail(theta[5]*diff(c(0,out2$cumI))/incr,-1)
lines(head(times,-1),new.cases, col = col)
# This is a print out of the final size of the model and the data
print(c(sum(new.cases)*incr, sum(mpearl$newcases)))
}

# Alpha variant - note that in calibrating the fit I aimed for 472 cases in the outbreak
make.graph("red")


# Original variant - the final size is now only 115 (for the less transmissible original variant)
theta[1] <- theta[1]/1.29
make.graph("blue")

# Delta variant - this outbreak is uncontrolled even after the changepoint, but that is fine
# there is no vaccination yet...
theta[1]<-theta[1]*1.29*1.95
make.graph("green")

