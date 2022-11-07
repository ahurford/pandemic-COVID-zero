i = 0
x = 5

weibull.fun = function(i,x){
incr = 0.1
L = 20
exposure <- seq(0,L, incr)
seq1 <- rep(0,length(exposure))
seq1[(i+x)*(1/incr)+1:length(seq1)] = dweibull(exposure[(i+x)*(1/incr)+1:length(seq1)], 2.83, scale = 5.67, log = FALSE)
seq1 = head(seq1,length(exposure))
data = data.frame(exposure, seq1)
}

i=0
x=5
data = weibull.fun(i,x)
data.ref = weibull.fun(0,0)[,2]
data = data.frame(data,data.ref)
g1.val = round(sum(data$seq1)*incr,2)

g1=ggplot(data,aes(x=exposure,group=1)) +
  geom_ribbon(aes(ymax = seq1, ymin=0), fill = "blue", alpha = 0.4)+
  geom_line(aes(y=data.ref))+
  ylab("prob. of infection")+
  xlab("days since entry")+
  xlim(c(0,12))+
  annotate("text", x = 10,y = 0.15,
           label =g1.val,size=4, col = "black")+
  ggtitle("Exposure: 0 days before entry\nSelf-isolation: 5 days")+theme_classic()+theme(plot.title=element_text(size=rel(1)))

i=3
x=5
data = weibull.fun(i,x)
data.ref = weibull.fun(0,0)[,2]
data = data.frame(data,data.ref)
g2.val = round(sum(data$seq1)*incr,2)

g2=ggplot(data,aes(x=exposure-3,group=1)) +
  geom_ribbon(aes(ymax = seq1, ymin=0), fill = "blue", alpha = 0.4)+
  geom_line(aes(y=data.ref))+
  ylab("prob. of infection")+
  xlab("days since entry")+
  xlim(c(0,12))+
  annotate("text", x = 10,y = 0.15,
           label =g2.val,size=4, col = "black")+
  ggtitle("Exposure: 3 days before entry\nSelf-isolation: 5 days")+theme_classic()+theme(plot.title=element_text(size=rel(1)))

i=0
x=2
data = weibull.fun(i,x)
data.ref = weibull.fun(0,0)[,2]
data = data.frame(data,data.ref)
g3.val = round(sum(data$seq1)*incr,2)

g3=ggplot(data,aes(x=exposure,group=1)) +
  geom_ribbon(aes(ymax = seq1, ymin=0), fill = "red", alpha = 0.4)+
  geom_line(aes(y=data.ref))+
  ylab("prob. of infection")+
  xlab("days since entry")+
  xlim(c(0,12))+
  annotate("text", x = 10,y = 0.15,
           label =g3.val,size=4, col = "black")+
  ggtitle("Exposure: 0 days before entry\nSelf-isolation: 2 days")+theme_classic()+theme(plot.title=element_text(size=rel(1)))

i=3
x=2
data = weibull.fun(i,x)
data.ref = weibull.fun(0,0)[,2]
data = data.frame(data,data.ref)
g4.val = round(sum(data$seq1)*incr,2)

g4=ggplot(data,aes(x=exposure-3,group=1)) +
  geom_ribbon(aes(ymax = seq1, ymin=0), fill = "red", alpha = 0.4)+
  geom_line(aes(y=data.ref))+
  ylab("prob. of infection")+
  xlab("days since entry")+
  xlim(c(0,12))+
  annotate("text", x = 10,y = 0.15,
           label =g4.val,size=4, col = "black")+
  ggtitle("Exposure: 3 days before entry\nSelf-isolation: 2 days")+theme_classic()+theme(plot.title=element_text(size=rel(1)))



weibull.fun2 =function(x){
  for(i in seq(0,10)){
  y=weibull.fun(i,x)
  ysum[i+1]=sum(y$seq1[y$exposure>=i])*incr
  }
  ymean = mean(ysum)
}

xvec <- seq(0,14)
y<-xvec
for(i in seq(1,length(xvec))){
  x = xvec[i]
  y[i] = weibull.fun2(x)
}

data = data.frame(self.iso = xvec, inf = y)
g5 = ggplot(data,aes(x=self.iso,group=1)) +
  geom_line(aes(y=inf))+
  ylab("prop. infectivity remaining")+
  xlab("days of self-isolation")+
  xlim(c(0,12))+
  ggtitle("U[0,10] days exposure before entry")+theme_classic()+theme(plot.title=element_text(size=rel(1)))

g.full = (g1+g2)/(g3+g4)+g5+ plot_annotation(tag_levels = 'A')
ggsave("supp_mat.png", height=10, width=8) 