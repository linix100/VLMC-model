fintraloglike<-function(){
  #training 
  library(VLMC)
  
  matrixdata<-wmatrix(1)
  slices<-nrow(matrixdata)
  #define a vector to store result
  log_like<-c(1:slices)
  
  for(k in 1:slices){
    print(k)
    #pass original values
    m_data<-matrixdata
    #find the left strings
    Leftover<-m_data[-k,]
    training<-as.character(t(Leftover))
    t2<-unlist(training)
    #training
    vc<-vlmc(t2,3)
    
    
    print(vc)
    Querys<-matrixdata[k,]
    #predict using decision tree
    Bi_Prob<-predict(vc,Querys)
    
    #find Log-likelihood of the Query data
    t_end<-length(Querys)
    Log_Sum<-0
    i=2
    while(i<=t_end){
      if(k==1){
        if(Querys[i]=="a"){
          j<-2
        }
        if(Querys[i]=="c"){
          j<-3
        }
        if(Querys[i]=="g"){
          j<-4
        }
        if(Querys[i]=="i"){
          j<-5
        }
        if(Querys[i]=="s"){
          j<-6
        }
        if(Querys[i]=="u"){
          j<-7
        }
        if(Querys[i]=="y"){
          j<-8
        }
        if(Querys[i]=="*"){
          j<-1
        }
      }
      else{
      if(Querys[i]=="a"){
        j<-2
      }
      if(Querys[i]=="c"){
        j<-3
      }
      if(Querys[i]=="g"){
        j<-4
      }
      if(Querys[i]=="i"){
        j<-5
      }
      if(Querys[i]=="j"){
        j<-6
      }
      if(Querys[i]=="s"){
        j<-7
      }
      if(Querys[i]=="u"){
        j<-8
      }
      if(Querys[i]=="y"){
        j<-9
      }
      if(Querys[i]=="*"){
        j<-1
      }
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