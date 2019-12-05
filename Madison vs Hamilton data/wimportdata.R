wimportdata<-function(){
  numberofslices<-20
  #the original one
  directory2<-"/Users/Linix100/Documents/Irosha/federalist_ Preprocessed_ papers-1/Madison/fileproc_37 copy.txt"
  
  
  a1<-scan(directory2,what="")
  a2<-as.character(a1)
  a3<-strsplit(a2,"")
  a4<-unlist(a3)
  a5<-tolower(a4)
  
  t_end<-length(a5)
  #the number of eliment in each slice
  x<-floor(t_end/numberofslices)
  print(x)
  directory2<-"/Users/Linix100/Documents/Irosha/federalist_ Preprocessed_ papers-1/Hamilton/fileproc_01 copy.txt"
  
s1<-scan(directory2,what="")
s2<-as.character(s1)
s3<-strsplit(s2,"")
s4<-unlist(s3)
s5<-tolower(s4)
t_end2<-length(s5)
y<-floor(t_end2/x)
print(y)
yn<-y*x


#pass value to make vector2 a matrix
s6<-s5[1:yn]

#temperary test
library(VLMC)
vc<-vlmc(s6,15)
return(vc)
}