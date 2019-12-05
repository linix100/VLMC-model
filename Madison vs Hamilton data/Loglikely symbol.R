loglikely<-function(Training_Data,cutoff_number,Query_Data){
  
  library(VLMC)
  Training_Bi<-Training_Data
  vc<-vlmc(Training_Bi,cutoff=cutoff_number)
  
  Bi_Prob<-predict(vc,Query_Data)
  
  t_end<-length(Query_Data)
  
  Log_Sum<-0
  
  i=2
  while(i<t_end){
    if(Query_Data[i]=="@"){
      j<-1
    }else{
      j<-2
    }
    Log_Sum<-log(Bi_Prob[i,j])+Log_Sum
    i<-i+1
  }
  
  return(Log_Sum)
  
}