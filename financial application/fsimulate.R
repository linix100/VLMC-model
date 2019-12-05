fsimu<-function(){
  library(VLMC)
  
  #directory1<-"/Users/Linix100/Documents/R/financial/all 498.txt"
  directory1<-"/Users/Linix100/Documents/R/financial/2pc 498.txt"
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
    s5<-a5[(k):(99+k)]
    #training every 130 letters
    vc5<-vlmc(s5,3)
    #simulate the 131th letter
    s<-simulate.vlmc(vc5,n=1)
    #save the 131th letter to ss
    ss<-s[1]
    #the true value of the 131th letter
    qq<-a5[100+k]
    #use the prediction matrix to find the probability
    Bi_Prob<-predict(vc5,a5[1:120])
    
    if(ss=="b"){
      j<-1
    }
    
    if(ss=="g"){
      j<-2
    }
    if(ss=="h"){
      j<-3
    }
    if(ss=="l"){
      j<-4
    }
    if(ss=="m"){
      j<-5
    }
    
    #a correction
    if(log(Bi_Prob[100+k,j])==-Inf){
      Bi_Prob[100+k,j]<-1
    }
    simuvalue<-log(Bi_Prob[100+k,j])
    # find the true value probability
    if(qq=="b"){
      h<-1
    }
    
    if(qq=="g"){
      h<-2
    }
    if(qq=="h"){
      h<-3
    }
    if(qq=="l"){
      h<-4
    }
    if(qq=="m"){
      h<-5
    }
    
    #a correction
    if(log(Bi_Prob[100+k,h])==-Inf){
      Bi_Prob[100+k,h]<-1
    }
    truevalue<-log(Bi_Prob[100+k,h])
    #calculate the square error
    m[k]<-(simuvalue-truevalue)^2
    m2[k]<-simuvalue
    m1[k]<-truevalue
  }
  
  print(m1)
  print(m2)
  print(m)
  m3<-mean(m)
  m4<-var(m2)
  print(m4)
  m5<-m3-m4
  count<-0
  for(rate1 in 1:20){
    if(m[rate1]==0){
      count<-count+1
    }
  }
  print(count)
  return(m5)
}