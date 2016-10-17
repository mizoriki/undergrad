#plotx
plotx<-function(data){
  N<-length(data[,1])
  prod<-apply(data,2,cumprod)
  ymax<-max(prod)
  plot(1:N,prod[,1],type="l",ylim=c(0,ymax+2),ylab="Sn(ratio of wealth",xlab="t(Days)")
  lines(1:N,prod[,2],col="gray")
}

#crp function
crp2<- function(data,b){
  N<-length(data[,1])
  S<-1
  for(i in 1:N) S<- S* (b*data[i,1]+(1-b)*data[i,2])
  S
}


crpn<- function(data,b,N){
  S<-1
  for(i in 1:N) S<- S* (b*data[i,1]+(1-b)*data[i,2])
  S
}

crpdiri<-function(data,b,N){
  S<-1
  for(i in 1:N)S<-S* (b*data[i,1]+(1-b)*data[i,2])
  S<-S/(pi*sqrt(b*(1-b)))
  S
}

crpbeta <- function(data,b,N,p,q){
  S<-1
  for(i in 1:N) S<-S*(b*data[i,1]+(1-b)*data[i,2])
  S<-S*dbeta(b,p,q)
  S
}

#get data
library(logopt)
data("nyse.cover.1962.1984")
x<-coredata("nyse.cover.1962.1984")
xik<-x[,c("iroqu","kinar")]
nDays <- dim(xik[,1])
Days<-1:nDays

#calculate universal@uniform
univ<-1:nDays
for(i in 1:nDays) univ[i]<-integrate(function(x){crpn(xik,x,i)},lower=0,upper=1)$value
plot(Days,univ,type="l",col="green")

#calculate universal@dirichret
univd<-1:nDays
for(i in 1:nDays) univd[i]<-integrate(function(x){crpdiri(xik,x,i)},lower=0,upper=1)$value
lines(Days,univd,col="red")

#calculate universal@beta
univb<-1:nDays
for(i in 1:nDays) univb[i]<-integrate(function(x){crpbeta(xik,x,i,2,2)},lower=0,upper=1)$value
#note: replace a,b with realnumber
lines(Days, univb,col="blue")

legend("topleft",c("beta(2,2)","unifrom","dirichret(1/2,1/2)"),col=c("blue","green","red"),lty=c(1,1,1))


plot3<- function(data){
  nDays <- length(data[,1])
  Days <- 1:nDays
  univu<-1:nDays
  univb<- 1:nDays
  univd<-1:nDays
  for(i in 1:nDays){
    univu[i] <- integrate(function(x){crpn(data,x,i)},lower=0, upper=1)$value
    univb[i] <- integrate(function(x){crpbeta(data,x,i,2,2)},lower=0,upper=1)$value
    univd[i] <- integrate(function(x){crpdiri(data,x,i)},lower=0,upper=1)$value
  }
  ylim<-max(univu,univb,univd)
  plot(Days,univu,type="l",col="green",ylim=c(0,ylim+2),ylab="Sn (ratio of wealth)",xlab="t (Days)")
  lines(Days,univb,col="blue")
  lines(Days,univd,col="red")
  legend("topleft",c("beta(2,2)","unifrom","dirichret(1/2,1/2)"),col=c("blue","green","red"),lty=c(1,1,1))
}


plot4<- function(data,N){
  nDays <- N
  Days <- 1:nDays
  univu<-1:nDays
  univb<- 1:nDays
  univb2<-rep(0,nDays)
  univb3<-rep(0,nDays)
  univd<-rep(0,nDays)
  for(i in 1:nDays){
    univu[i] <- integrate(function(x){crpn(data,x,i)},lower=0, upper=1)$value
    univb[i] <- integrate(function(x){crpbeta(data,x,i,2,2)},lower=0,upper=1)$value
    univd[i] <- integrate(function(x){crpdiri(data,x,i)},lower=0,upper=1)$value
    univb2[i] <- integrate(function(x){crpbeta(data,x,i,100,100)},lower=0,upper=1)$value 
    univb3[i] <- integrate(function(x){crpbeta(data,x,i,0.1,0.1)},lower=0,upper=1)$value
  }
  ylim<-max(univu,univb,univb2,univb3,univd)
  plot(Days,univu,type="l",col="green",ylim=c(0,ylim+2),ylab="St (ratio of wealth)",xlab="t (Days)")
  lines(Days,univb,col="blue")
  lines(Days,univb2,col="red")
  lines(Days,univd,col="purple")
  lines(Days,univb3)
  legend("topleft",c("beta(100,100)","beta(2,2)","uniform","beta(1/2,1/2)","beta(0.1,0.1)"),
         col=c("red","blue","green","purple","black"),lty=c(1,1,1,1,1))
}