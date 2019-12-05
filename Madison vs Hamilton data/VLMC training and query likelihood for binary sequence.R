loglikelihood<-function(Training_Data,cutoff_number,Query_Data,slices){
  #training 
  library(VLMC)
  Training_Bi<-Training_Data
  vc<-vlmc(Training_Bi,cutoff=cutoff_number)
  
  slices<-slices
  
  #define a vector to store result
  log_like<-c(1:slices)
  for(k in 1:slices){
    print(k)
    tt[k]<-0
    Querys<-Query_Data[k,]
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

      
      #another correction
      
      if(log(Bi_Prob[i,j])==-Inf){
        Bi_Prob[i,j]<-0.5
      }
      #if(Bi_Prob[i,j]==0 && Bi_Prob[2,j]!=0){
      #  Bi_Prob[i,j]<-Bi_Prob[2,j]
      #}
      
      Log_Sum<-log(Bi_Prob[i,j])+Log_Sum
      i<-i+1
    }
    
    log_like[k]<-Log_Sum
    
  }
  
  return(log_like)
  
}