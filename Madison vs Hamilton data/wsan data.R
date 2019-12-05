wscandata<-function(){
  library(VLMC)
  numberofslices<-3
  directory2<-"/Users/Linix100/Documents/1-150.txt"
  #directory2<-"/Users/Linix100/Documents/Irosha/federalist_ Preprocessed_ papers-1/Madison/fileproc_37 copy 2.txt"
  #filename<-list.files(path=directory,pattern="*.txt",full.names=TRUE)
  
  #listfiles<-lapply(filename,function(x) scan(x,what=""))
  #
  #a1<-unlist(listfiles)
  a1<-scan(directory2,what="")
  a2<-as.character(a1)
  a3<-strsplit(a2,"")
  a4<-unlist(a3)
  a5<-tolower(a4)
  t_end<-length(a5)
  #the number of eliment in each line
  x<-floor(t_end/numberofslices)
  
  #how many digits needed in each
  xn<-x*numberofslices
  
  #pass value to make vector2 a matrix
  a6<-a5[1:xn]
  vc<-vlmc(a6,3)
  return(vc)
  
}