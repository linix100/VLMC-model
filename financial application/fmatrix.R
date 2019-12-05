fmatrix<-function(choosedir){
  numberofslices<-12
  directory2<-"/Users/Linix100/Documents/R/financial/all 498.txt"
  #directory2<-"/Users/Linix100/Documents/R/financial/2pc 498.txt"
  a1<-scan(directory2,what="")
  a2<-as.character(a1)
  a3<-strsplit(a2,"")
  a4<-unlist(a3)
  a5<-tolower(a4)
  b5<-a5[301:420]
  t_end<-length(b5)
  x<-floor(t_end/numberofslices)
  
  if(choosedir==1){
    # choose 1 to check and do inter itself
    #directory2<-"/Users/Linix100/Documents/1-150.txt"
    s5<-a5[301:420]
  }else{
    #choose 2 todo inter loglikelihood (most time use)
    #directory2<-"/Users/Linix100/Documents/301-450.txt"
    s5<-a5[1:150]
  }
  
  t_end2<-length(s5)
  y<-floor(t_end2/x)
  yn<-y*x
  #pass value to make vector2 a matrix
  s6<-s5[1:yn]
  qmatrix<-matrix(s6,ncol=x,byrow=TRUE)
  
  return(qmatrix)
}