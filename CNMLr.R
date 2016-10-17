#task 12/10 
#construct CNML
#idea 1: based on well performing stock on each day
#idea 2:
#idea 3:

cnmlw<-function(y,j,n){
  l<-sum(y[,2]>y[,1])
  n1<-length(y[,2])
  d<-minimaxw2(l,n,2*n)
  k<-sum(j==1)
  i<-length(j)
  k<-k+l
  m<-n+n1
  denom<-sum(sapply(0:m,function(x){(x/m)^x*(1-x/m)^(m-x)*choose(m,x)}))
  enum<-sum(sapply(0:(n-i),function(x){((k+x)/m)^(k+x)*(1-(k+x)/m)^(m-(k+x))*choose(n-i,x)}))
  w<-enum/denom/d
  return(w)
}

cnmlw1<-function(y,k,i,n){
  l<-sum(y[,2]>y[,1])
  n1<-length(y[,2])
  d<-minimaxw2(l,n,2*n)
  k<-k+l
  m<-n+n1
  #how do i integrate the past information in the path???
  denom<-sum(sapply(0:m,function(x){(x/m)^x*(1-x/m)^(m-x)*choose(m,x)}))
  enum<-sum(sapply(0:(n-i),function(x){((k+x)/m)^(k+x)*(1-(k+x)/m)^(m-(k+x))*choose(n-i,x)}))
  w<-enum/denom/d
  return(w)
}
#x is future, y is past
#would the total of every path sum up to 1?
#task2: construct plot of weight of each path
#done 12/11 minimaxwplot
minimaxwplot<-function(n){
  t<-1:2^n-1
  k<-sapply(t,dec2bin1) #calling function takes time
  denom<-sum(sapply(0:n,function(x){(x/n)^x*(1-x/n)^(n-x)*choose(n,x)}))
  k<-sapply(k,function(x){(x/n)^x*(1-x/n)^(n-x)/denom})
  plot(t,k,type="l")
  s<-table(k)
  return(s)
}


cnmlw1plot<-function(y,n){
  l<-sum(y[,2]>y[,1])
  n1<-length(y[,1])
  t<-1:2^n-1
  k<-sapply(t,dec2bin1) #calling function takes time
  m<-n+n1
  denom1<-minimaxw2(l,n,m)
  denom<-sum(sapply(0:m,function(x){(x/m)^x*(1-x/m)^(m-x)*choose(m,x)}))
  k<-sapply(k,function(x){((x+l)/m)^(x+l)*(1-(x+l)/m)^(m-(l+x))/denom/denom1})
  plot(t,k,type="l")
  s<-table(k)
  append(s,sum(k))
  return(s)
}


cnmlw1b<-function(y,x,i,n){
  if(i==1){
    return(0.5)
  }else{
    enum<-0
    denom<-0
    for(t in 0:2^(i-1)-1){
      j<-dec2bin(t,i-1)
      xprod<-1
      for(s in 1:(i-1)) xprod<- xprod*as.numeric(x[s,j[s]+1])
      enum<-enum+xprod*cnmlw(y,append(j,1),n)
      denom<-denom+ xprod*cnmlw(y,j,n)
    }
    b<-enum/denom
    return(b)
  }
}
#check: whether the past path is considered properly
#calculate how to gain b at i=1 properly it definitely isn't 0.5

cnmlw1S<-function(y,x,n){
  S<-1
  for(i in 1:n){
    b<-cnmlw1b(y,x,i,n)
    gain<-b*as.numeric(x[i,1])+(1-b)*as.numeric(x[i,2])
    S<-S*gain
  }
  return(S)
}

mmcnmlplot<-function(y,x,n){
  S<-rep(1,n)
  cS<-rep(1,n)
  S[1]<-0.5*as.numeric(x[1,1]+x[1,2])
  cS[1]<-S[1]
  for(i in 2:n){
    b<-minimaxb(x,i,n)
    cb<-cnmlw1b(y,x,i,n)
    gain<-b*as.numeric(x[i,1])+(1-b)*as.numeric(x[i,2])
    cgain<-cb*as.numeric(x[i,1])+(1-cb)*as.numeric(x[i,2])
    S[i]<-S[i-1]*gain
    cS[i]<-cS[i-1]*cgain
  }
  ymax<-max(S,univu[1:n],cS)
  plot(1:n,S,type="l",col="red",ylim=c(0,ymax+2))
  lines(1:n,univu[1:n],col="green")
  lines(1:n,cS,col="blue")
}
#memo adjusting univu's range is MENDOI
#memo with short length S makes no big difference
#memo consider comparing the value of b based on univu, cnmlb and nmlb

b3plot <-function(y,x,n){
  buniv<-1:n
  bnml<-1:n
  bcnml<-1:n
  alpha<-seq(0,1,by=0.05)
  uniform<-rep(1,21)/21
  for(j in 1:n){
    for(i in 1:21) crps[i]<-crp(x,c(alpha[i],1-alpha[i]))[j]
    enum<-sum(crps*uniform*alpha)
    denom<-sum(crps*uniform)
    buniv[j]<-enum/denom
    bnml[j]<-minimaxb(x,j,n)
    bcnml[j]<-cnmlw1b(y,x,j,n)
  }
  #hint integrate each day?
  #use the acutal formula with fractions?
  #integrate per day can't get the value of b
  plot(1:n,buniv,type="l",col="green",ylim=c(0,1),ylab="beta",xlab="Days past")
  lines(1:n,bnml,col="red")
  lines(1:n,bcnml,col="blue")
}

#old data
plot_2data <-function(cut,Data){
  #setting variants
  alpha <- seq(0,1,by=1/cut)
  num <- cut+1
  nDays<-length(Data[,1])
  Days <- 1:nDays
  #calculating universal
  universal <- Days*0
  betaplot <- Days
  gain <- Days
  uniform <- alpha*0+1/num
  for(i in 1:num) universal <-universal + crp(Data,c(alpha[i],1-alpha[i]))
  universal <- universal/num
  #calculating beta
  distribution <- uniform
  for(j in 2:nDays){
    crp_b <-1:num
    denom <-0
    numer <-0
    for(i in 1:num){
      crp_b[i] <- crp(Data,c(alpha[i],1-alpha[i]))[j-1]
      denom <- denom + distribution[i]*crp_b[i]
      numer <- numer + distribution[i]*crp_b[i]*alpha[i]
    }
    #for(i in 1:num) distribution[i] <- crp_b[i]*distribution[i]/denom
    betaplot[j]<-numer/denom
  }
  betaplot[1]<-0.5
  #calculating test 
  gain[1]<- betaplot[1]*Data[1,1]+(1-betaplot[1])*Data[1,2]
  for(i in 2:nDays) gain[i] <- (betaplot[i]*Data[i,1]+(1-betaplot[i])*Data[i,2])*gain[i-1]
  #calculating Data revenue
  prod <- Data
  prod <- apply(Data,2,cumprod)
  #plotting
  plot(Days,universal,type="l",col="green")
  lines(Days,gain,col="purple")
  lines(Days,prod[,1],col="red")
  lines(Days,prod[,2],col="blue")
  legend("topleft",c('"universal"','"test"',"Data1","Data2"),col=c("green","purple","red","blue"),lty=c(1,1,1,1))
}

