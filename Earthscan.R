#for scaning Earthquake .txt data into letter by letter and training the data
escan<-function(){
  library(VLMC)
  numberofslices<-1
  #this directory should be changed to 2PC ones
  directory1<-"/Users/Linix100/Documents/R/earth/earthquake.txt"
  a1<-scan(directory1,what="")
  a2<-as.character(a1)
  a3<-strsplit(a2,"")
  a4<-unlist(a3)
  #upp<-toupper(a4)
  #print(upp)
  a5<-tolower(a4)
  #print(a4)
  #a5<-a4
  a5<-a5[529:568]
  t_end<-length(a5)
  print(t_end)
  #the number of eliment in each line
  x<-floor(t_end/numberofslices)
  
  #how many digits needed in each
  xn<-x*numberofslices
  
  #pass value to make vector2 a matrix
  a6<-a5[1:xn]
  vc<-vlmc(a6,3)
  return(vc)
}