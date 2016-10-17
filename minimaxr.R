dec2bin <- function(num, digit=0){
  if(num <= 0 && digit <= 0){
    return(NULL)
  }else{
    return(append(Recall(num%/%2,digit-1), num%%2))
  }
}

minimaxw<-function(j,n){
 denom<-sum(sapply(0:n,function(i){(i/n)^i*(1-i/n)^(n-i)*choose(n,i)}))
 k<-sum(j==1)
 l<-length(j)
 enum<-sum(sapply(0:n,function(i){((i+k)/n)^(i+k)*(1-(i+k)/n)^(n-(i+k))*choose(n-l,i)}))
 w<-enum/denom
 return(w)
}

minimaxb<-function(x,i,n){
  if(i==1){
    return(0.5)
  }else{
  enum<-0
  denom<-0
  for(t in 0:2^(i-1)-1){
    j<-dec2bin(t,i-1)
    xprod<-1
    for(s in 1:(i-1)) xprod<- xprod*as.numeric(x[s,j[s]+1])
    enum<-enum+xprod*minimaxw(append(j,1),n)
    denom<-denom+ xprod*minimaxw(j,n)
  }
  b<-enum/denom
  return(b)
  }
}
#t becomes too large due to exponentiality
#possibly avoidable with "choose func"
#cant avoid cuz the path of x has 2^n patterns
minimaxw2<-function(k,i,n){
  denom<-sum(sapply(0:n,function(x){(x/n)^x*(1-x/n)^(n-x)*choose(n,x)}))
  enum<-sum(sapply(0:(n-i),function(x){((x+k)/n)^(x+k)*(1-(x+k)/n)^(n-(x+k))*choose(n-i,x)}))
  w<-enum/denom
  return(w)
}

minimaxS<-function(x,n){
  S<-1
  for(i in 1:n){
    b<-minimaxb(x,i,n)
    gain<-b*as.numeric(x[i,1])+(1-b)*as.numeric(x[i,2])
    S<-S*gain
  }
  return(S)
}

minimaxplot<-function(x,n){
  S<-rep(1,n)
  S[1]<-0.5*as.numeric(x[1,1]+x[1,2])
  for(i in 2:n){
    b<-minimaxb(x,i,n)
    gain<-b*as.numeric(x[i,1])+(1-b)*as.numeric(x[i,2])
    S[i]<-S[i-1]*gain
  }
  ymax<-max(S,univu[1:n])
  plot(1:n,S,type="l",col="red",ylim=c(0,ymax+2))
  lines(1:n,univu[1:n],col="green")
}
#use  system.time() to see how long it takes
#or
#t<-proc.time()
#<insert function>
#proc.time()-t

dec2bin1<-function(x){
  i<-0
  while(x>0){
    if(x%%2==1){
      x<- x-1
      i<-i+1
    }
    x<-x%/%2
  }
  return(i)
}


minimaxwplot<-function(n){
  t<-1:2^n-1
  k<-sapply(t,dec2bin1) #calling function takes time
  denom<-sum(sapply(0:n,function(x){(x/n)^x*(1-x/n)^(n-x)*choose(n,x)}))
  k<-sapply(k,function(x){(x/n)^x*(1-x/n)^(n-x)/denom})
  plot(t,k,type="b")
  s<-table(k)
  return(s)
}

n<-13
tim<-proc.time()
k<-sapply(k,function(x){(x/n)^x*(1-x/n)^(n-x)/denom})
proc.time()-tim

cnml1<-function(y,x,n){
  s<-sum(y[1:n,1]>y[1:n,2])
  
}

