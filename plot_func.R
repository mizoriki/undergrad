plot_2x <-function(cut,Data1,Data2){
  #setting variants
  Data<- x[,c(Data1,Data2)]
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
    #   distribution <- uniform  ‚±‚Ìs‚ð“ü‚ê‚é‚ÆXV‚ª“Ý‚­‚È‚é
    for(i in 1:num) distribution[i] <- crp_b[i]*distribution[i]/denom
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
  legend("topleft",c('"universal"','"test"',Data1,Data2),col=c("green","purple","red","blue"),lty=c(1,1,1,1))
}
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

#chanto sekibun dekinai kana?
plot_dirichret <-function(cut,Data){
  #setting variants
  alpha <- seq(0,1,by=1/cut)
  alpha[1]<-0.00001
  alpha[21]<-0.99999
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
  distribution <- 1/pi/sqrt(alpha*(1-alpha))
  for(j in 2:nDays){
    crp_b <-1:num
    denom <-0
    numer <-0
    for(i in 1:num){
      crp_b[i] <- crp(Data,c(alpha[i],1-alpha[i]))[j-1]
      denom <- denom + distribution[i]*crp_b[i]
      numer <- numer + distribution[i]*crp_b[i]*alpha[i]
    }
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

