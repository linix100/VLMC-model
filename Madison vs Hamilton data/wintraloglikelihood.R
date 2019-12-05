wintraloglike<-function(){
  #training 
  library(VLMC)
  
  matrixdata<-wmatrix(1)
  slices<-nrow(matrixdata)
  #define a vector to store result
  log_like<-c(1:slices)
  
  for(k in 1:slices){
    print(k)
    #pass original value
    m_data<-matrixdata
    #find the left strings
    Leftover<-m_data[-k,]
    training<-as.character(t(Leftover))
    t2<-unlist(training)
    #training
    vc<-vlmc(t2,15)
    
    
    
    Querys<-matrixdata[k,]
    #predict using decision tree
    Bi_Prob<-predict(vc,Querys)
    
    #find Log-likelihood of the Query data
    t_end<-length(Querys)
    Log_Sum<-0
    i=2
    while(i<=t_end){
      if(Querys[i]=="a"){
        j<-2
      }
      if(Querys[i]=="b"){
        j<-3
      }
      if(Querys[i]=="c"){
        j<-4
      }
      if(Querys[i]=="d"){
        j<-5
      }
      if(Querys[i]=="e"){
        j<-6
      }
      if(Querys[i]=="f"){
        j<-7
      }
      if(Querys[i]=="g"){
        j<-8
      }
      if(Querys[i]=="h"){
        j<-9
      }
      if(Querys[i]=="i"){
        j<-10
      }
      if(Querys[i]=="j"){
        j<-11
      }
      if(Querys[i]=="k"){
        j<-12
      }
      if(Querys[i]=="l"){
        j<-13
      }
      if(Querys[i]=="m"){
        j<-14
      }
      if(Querys[i]=="n"){
        j<-15
      }
      if(Querys[i]=="o"){
        j<-16
      }
      if(Querys[i]=="p"){
        j<-17
      }
      if(Querys[i]=="q"){
        j<-18
      }
      if(Querys[i]=="r"){
        j<-19
      }
      if(Querys[i]=="s"){
        j<-20
      }
      if(Querys[i]=="t"){
        j<-21
      }
      if(Querys[i]=="u"){
        j<-22
      }
      if(Querys[i]=="v"){
        j<-23
      }
      if(Querys[i]=="w"){
        j<-24
      }
      if(Querys[i]=="x"){
        j<-25
      }
      if(Querys[i]=="y"){
        j<-26
      }
      if(Querys[i]=="z"){
        j<-27
      }
      if(Querys[i]=="*"){
        j<-1
      }
      #correction
      if(log(Bi_Prob[i,j])==-Inf){
        Bi_Prob[i,j]<-1
      }
      
      Log_Sum<-log(Bi_Prob[i,j])+Log_Sum
      i<-i+1
    }
    
    log_like[k]<-Log_Sum
    
  }
  
  return(log_like)
}