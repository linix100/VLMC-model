#this function provide loglikelihood of each slice.
eloglike<-function(trainedmc,intra1orinter2){
  library(VLMC)
  vc<-trainedmc
  querys<-ematrix(intra1orinter2)
  
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
              if(Querys[i]=="f"){
                j<-3
              }
              if(Querys[i]=="g"){
                j<-4
              }
              if(Querys[i]=="h"){
                j<-5
              }
              if(Querys[i]=="l"){
                j<-6
              }
              if(Querys[i]=="m"){
                j<-7
              }
              if(Querys[i]=="n"){
                j<-8
              }
              if(Querys[i]=="q"){
                j<-9
              }
              if(Querys[i]=="r"){
                j<-10
              }
              if(Querys[i]=="s"){
                j<-11
              }
              if(Querys[i]=="t"){
                j<-12
              }
              if(Querys[i]=="w"){
                j<-13
              }
      }
        if(intra1orinter2==2){
              if(Querys[i]=="b"){
                j<-1
              }
              if(Querys[i]=="f"){
                j<-2
              }
              if(Querys[i]=="g"){
                j<-3
              }
              if(Querys[i]=="h"){
                j<-4
              }
              if(Querys[i]=="l"){
                j<-5
              }
              if(Querys[i]=="m"){
                j<-6
              }
        }
      if(intra1orinter2==3){
        if(Querys[i]=="b"){
          j<-1
        }
        if(Querys[i]=="f"){
          j<-2
        }
        if(Querys[i]=="g"){
          j<-3
        }
        if(Querys[i]=="h"){
          j<-4
        }
        if(Querys[i]=="l"){
          j<-5
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