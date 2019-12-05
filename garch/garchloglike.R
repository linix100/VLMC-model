#this function provide loglikelihood of each slice.
garchloglike<-function(trainedmc,intra1orinter2){
  library(VLMC)
  vc<-trainedmc
  querys<-garchmatrix(intra1orinter2)
  
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
      if(intra1orinter2==1){
        
        if(Querys[i]=="a"){
          j<-1
        }
        if(Querys[i]=="b"){
          j<-2
        }
        if(Querys[i]=="c"){
          j<-3
        }
        if(Querys[i]=="d"){
          j<-4
        }
        if(Querys[i]=="e"){
          j<-5
        }
        if(Querys[i]=="f"){
          j<-6
        }
        if(Querys[i]=="g"){
          j<-7
        }
        if(Querys[i]=="h"){
          j<-8
        }
        if(Querys[i]=="i"){
          j<-9
        }
        if(Querys[i]=="j"){
          j<-10
        }
        if(Querys[i]=="k"){
          j<-11
        }
        if(Querys[i]=="l"){
          j<-12
        }
        if(Querys[i]=="m"){
          j<-13
        }
        if(Querys[i]=="n"){
          j<-14
        }
        if(Querys[i]=="o"){
          j<-15
        }
        if(Querys[i]=="p"){
          j<-16
        }
        if(Querys[i]=="q"){
          j<-17
        }
        if(Querys[i]=="r"){
          j<-18
        }
        if(Querys[i]=="s"){
          j<-19
        }
        if(Querys[i]=="u"){
          j<-20
        }
        if(Querys[i]=="w"){
          j<-21
        }
        if(Querys[i]=="x"){
          j<-22
        }
      }
      if(intra1orinter2==2){
        if(Querys[i]=="b"){
          j<-1
        }
        if(Querys[i]=="d"){
          j<-2
        }
        if(Querys[i]=="f"){
          j<-3
        }
        if(Querys[i]=="g"){
          j<-4
        }
        if(Querys[i]=="h"){
          j<-5
        }
        if(Querys[i]=="i"){
          j<-6
        }
        if(Querys[i]=="j"){
          j<-7
        }
        if(Querys[i]=="k"){
          j<-8
        }
        if(Querys[i]=="l"){
          j<-9
        }
        if(Querys[i]=="m"){
          j<-10
        }
        if(Querys[i]=="n"){
          j<-11
        }
        if(Querys[i]=="o"){
          j<-12
        }
        if(Querys[i]=="q"){
          j<-13
        }
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