#make 
#training 1-450 then based on that 
garchmatrix<-function(choosedir){
  numberofslices<-45
  directory2<-"/Users/Linix100/Documents/R/garch/Garch_text.txt"
  a1<-scan(directory2,what="")
  a2<-as.character(a1)
  a3<-strsplit(a2,"")
  a4<-unlist(a3)
  a5<-tolower(a4)
  # you can change the number 1:400 to before_earthquake(429:528) or after_earthquake(529:568) to compare more
  b5<-a5[1:450]
  t_end<-length(b5)
  x<-floor(t_end/numberofslices)
  
  if(choosedir==1){
    # choose 1 to check and do inter itself
    #directory2<-"/Users/Linix100/Documents/1-150.txt"
    s5<-a5[1:450]
  }else if(choosedir==2){
    #choose 2 todo inter between peaceful and before earthquake
    #directory2<-"/Users/Linix100/Documents/301-450.txt"
    s5<-a5[500:600]
  }
  
  
  t_end2<-length(s5)
  y<-floor(t_end2/x)
  yn<-y*x
  #pass value to make vector2 a matrix
  s6<-s5[1:yn]
  qmatrix<-matrix(s6,ncol=x,byrow=TRUE)
  
  return(qmatrix)
}