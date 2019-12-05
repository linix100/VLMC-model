Random_symbol<-function(N){
  x1<-runif(N,0,1)
  k=1
  while(k<N+1){
    if(x1[k]<0.7){
      if(k>3 && x1[k-1]=="@" && x1[k-2]=="%"){
        x1[k]<-"%"
      }else{
        x1[k]<-"@"
      }
      
    }else{
        x1[k]<-"%"
      }
      
    
    k<-k+1
  }
  
  return(x1)
}