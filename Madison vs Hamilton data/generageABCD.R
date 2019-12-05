Generate4<-function(N){
  x1<-runif(N,0,1)
  k=1
  while(k<N+1){
    if(x1[k]<0.5){
      if(x1[k]<0.25){
        x1[k]<-"A"
      }else{
        x1[k]<-"B"
      }
      
    }else{
      if(x1[k]>0.75){
        x1[k]<-"C"
      }else{
        x1[k]<-"D"
      }
      
    }
    k<-k+1
  }
  
  return(x1)
}
