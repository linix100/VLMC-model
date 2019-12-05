splitintogroups<-function(vector1,numberofslices){
  #find the number of entries
  t_end<-length(vector1)
  
  #the number of eliment in each line
  x<-floor(t_end/numberofslices)
  
  #how many digits needed in each
  xn<-x*numberofslices
  
  #pass value to make vector2 a matrix
  vector2<-vector1[1:xn]
  
  x<-matrix(vector2,nrow=numberofslices,byrow=TRUE)
  
  return(x)
  
}