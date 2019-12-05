fintra<-function(){
  library(VLMC)
  #input data
  directory0<-"/Users/Linix100/Documents/1-150.txt"
  a1<-scan(directory0,what="")
  a2<-as.character(a1)
  a3<-strsplit(a2,"")
  a4<-unlist(a3)
  a5<-tolower(a4)
  #training
  a6<-a5[51:150]
  vc1<-vlmc(a6,3)
  print(vc1)
  a7<-a5[1:50]
  Querys<-a7
  #predict using decision tree
  Bi_Prob<-predict(vc1,Querys)
  
  #find Log-likelihood of the Query data
  t_end<-50
  Log_Sum<-0
  i<-2
  j<-0
  while(i<=t_end){
   
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
      #correction
      if(log(Bi_Prob[i,j])==-Inf){
        Bi_Prob[i,j]<-1
      }
      
      Log_Sum<-log(Bi_Prob[i,j])+Log_Sum
      i<-i+1
  }
  print(Log_Sum)
  #log_like[k]<-Log_Sum
  


return(Log_Sum)
  
  
}