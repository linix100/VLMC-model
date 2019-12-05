Intra_loglikelihood<-function(cutoff_number,Matrix_Data,slices){
  #training 
  library(VLMC)
  
  slices<-slices
 
  #define a vector to store result
  log_like<-c(1:slices)
  
  for(k in 1:slices){
    print(k)
    #pass original value
    m_data<-Matrix_Data
    #find the left strings
    Leftover<-m_data[-k,]
    training<-as.character(t(Leftover))
    #training
    vc<-vlmc(training,cutoff=cutoff_number)
    
    

    Querys<-Matrix_Data[k,]
    #predict using decision tree
    Bi_Prob<-predict(vc,Querys)
    
    #find Log-likelihood of the Query data
    t_end<-length(Querys)
    Log_Sum<-0
    i=2
    while(i<t_end){
      if(Querys[i]=="0"){
        j<-1
      }else{
        j<-2
      }
      #correction
      if(log(Bi_Prob[i,j])==-Inf){
        Bi_Prob[i,j]<-0.5
      }
      
      Log_Sum<-log(Bi_Prob[i,j])+Log_Sum
      i<-i+1
    }
    
    log_like[k]<-Log_Sum
    
  }
  
  return(log_like)
  
}