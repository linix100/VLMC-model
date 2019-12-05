fsimu2<-function(){
  # 3pc
  library(VLMC)
  
  directory1<-"/Users/Linix100/Documents/R/financial/all 498.txt"
  #directory1<-"/Users/Linix100/Documents/R/financial/2pc 498.txt"
  a1<-scan(directory1,what="")
  a2<-as.character(a1)
  a3<-strsplit(a2,"")
  a4<-unlist(a3)
  a5<-tolower(a4)
  m<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  m1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  m2<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  for(k in 1:20){
    #length of slide window prediction is 130
    s5<-a5[(k):(119+k)]
    #training every 130 letters
    vc5<-vlmc(s5,3)
    #simulate the 131th letter
    s<-simulate.vlmc(vc5,n=1)
    #save the 131th letter to ss
    ss<-s[1]
    #the true value of the 131th letter
    qq<-a5[120+k]
    #use the prediction matrix to find the probability
    Bi_Prob<-predict(vc5,a5[1:150])
    
    if(ss=="e"){
      j<-1
    }
    if(ss=="k"){
      j<-2
    }
    if(ss=="n"){
      j<-3
    }
   
    
    #a correction
    if(log(Bi_Prob[120+k,j])==-Inf){
      Bi_Prob[120+k,j]<-1
    }
    simuvalue<-log(Bi_Prob[120+k,j])
    # find the true value probability
    if(qq=="e"){
      h<-1
    }
    if(qq=="k"){
      h<-2
    }
    if(qq=="n"){
      h<-3
    }
    
    
    #a correction
    if(log(Bi_Prob[120+k,h])==-Inf){
      Bi_Prob[120+k,h]<-1
    }
    truevalue<-log(Bi_Prob[120+k,h])
    #calculate the square error
    m[k]<-(simuvalue-truevalue)^2
    m2[k]<-simuvalue
    m1[k]<-truevalue
  }
  print(Bi_Prob[121:140,])
  print(m1)
  print(m2)
  print(m)
  m3<-mean(m)
  m4<-var(m2)
  print(m4)
  m5<-m3-m4
  return(m5)
}