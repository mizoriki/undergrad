CNMLwj<-function(past,j,n,type=2){
  km<-sum(past[,1]>past[,2])#count k of past sequence
  kn<-sum(j==0)             #count k of sequence j
  m<-length(past[,1])       #length of past sequence
  i<-length(j)              #length of sequnce j
  k<-km+kn                  #total count
  nm<-n+m                   #total length of days
  if(type==2){
    b<-function(x){(k+x)/nm} #change based on type
    bd<-function(x){(x+km)/nm}
  }else if(type==1){
    b<-function(x){(kn+x)/n}
    bd<-function(x){x/n}
  }
  enum<-sum(sapply(0:(n-i),function(x){b(x)^(k+x)*(1-b(x))^(nm-(k+x))*choose(n-i,x)}))
  denom<-sum(sapply(0:n,function(x){bd(x)^(x+km)*(1-bd(x))^(nm-(x+km))*choose(n,x)}))
  w<-enum/denom
  return(w)
}

CNMLwk<-function(past,k,i,n,type=2){
  kn<-k
  km<-sum(past[,1]>past[,2])
  m<-length(past[,2])
  k<-kn+km
  nm<-n+m
  if(type==2){
    b<-function(x){(k+x)/nm} #change based on type
    bd<-function(x){(x+km)/nm}
  }else if(type==1){
    b<-function(x){(kn+x)/n}
    bd<-function(x){x/n}
  }
  enum<-sum(sapply(0:(n-i),function(x){b(x)^(k+x)*(1-b(x))^(nm-(k+x))*choose(n-i,x)}))
  denom<-sum(sapply(0:n,function(x){bd(x)^(x+km)*(1-bd(x))^(nm-(x+km))*choose(n,x)}))
  w<-enum/denom
  return(w)
}

CNMLb<-function(past,x,i,n,type=2){
  if(i==1){
    b<-CNMLwj(past,0,n,type)
    return(b)
  }else{
    enum<-0
    denom<-0
    for(t in 0:2^(i-1)-1){
      j<-dec2bin(t,i-1)
      xprod<-1
      for(s in 1:(i-1)) xprod<- xprod*as.numeric(x[s,j[s]+1])
      enum<-enum+xprod*CNMLwj(past,append(j,0),n,type)
      denom<-denom+xprod*CNMLwj(past,j,n,type)
    }
  b<-enum/denom
  return(b)
  }
}

CNMLS<-function(past,x,type=2){
  n<-length(x[,1])
  b<-sapply(1:n,function(i){CNMLb(past,x,i,n,type)})
  gain<-sapply(1:n,function(i){as.numeric(x[i,1])*b[i]+as.numeric(x[i,2])*(1-b[i])})
  S<-sapply(gain,cumprod)
  return(S)
}
CNMLplot<-function(past,x,type=2){
  plot(1:n,CNMLS(past,x,type),type="l",col="red")
}
betaplot<-function(past,x){
  n<-length(x[,1])
  b1<-sapply(1:n,function(i){CNMLb(past,x,i,n,1)})
  b2<-sapply(1:n,function(i){CNMLb(past,x,i,n,2)})
  plot(1:n,b1,type="l",col="red",ylim=c(0,1),ylab="b(portfolio)",xlab="days")
  lines(1:n,b2,col="blue")
  legend("topleft",c('CNML1','CNML2'),col=c("red","blue"),lty=c(1,1))
}


#create S,b,plot based on "a certain distribution over sequence j"
#arbitrarily set the sequnce

Wplot<-function(w,x,n){
  
  
}
