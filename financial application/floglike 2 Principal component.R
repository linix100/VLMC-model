flog2pc<-function(trainedmc,intra1orinter2){
  library(VLMC)
  vc<-trainedmc
  querys<-fmatrix(intra1orinter2)
  
  slices<-nrow(querys)
  k<-1
  #define a vector to store result
  log_like<-c(1:slices)
  for(k in 1:slices){
    print(k)
    
    Querys<-querys[k,]
    #predict using decision tree
    Bi_Prob<-predict(vc,Querys)
    
    #find Log-likelihood of the Query data
    t_end<-length(Querys)
    
    Log_Sum<-0
    i=2
    while(i<=t_end){
      if(Querys[i]=="a"){
        j<-1
      }
      if(Querys[i]=="e"){
        j<-2
      }
      if(Querys[i]=="g"){
        j<-3
      }
      if(Querys[i]=="u"){
        j<-4
      }
      if(Querys[i]=="y"){
        j<-5
      }
      
      
      
      #another correction
      
      if(log(Bi_Prob[i,j])==-Inf){
        Bi_Prob[i,j]<-1
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