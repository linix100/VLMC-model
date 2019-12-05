importdata<-function(file_address){ 
  #import raw_data
  raw_data<-scan(file_address,what="")
  
  #eliminate space ? ! " comma : - period
  #eliminate space which is "00100000"
  remove.value<-"00100000"
  raw_data<-raw_data[which(!raw_data==remove.value)]  
  #eliminate ? which is "00111111"
  remove.value<-"00111111"
  raw_data<-raw_data[which(!raw_data==remove.value)]  
  #eliminate ! which is "00100001"
  remove.value<-"00100001"
  raw_data<-raw_data[which(!raw_data==remove.value)]
  #eliminate "   which is "00100010"
  remove.value<-"00100010"
  raw_data<-raw_data[which(!raw_data==remove.value)]
  #eliminate comma   which is "00101100"
  remove.value<-"00101100"
  raw_data<-raw_data[which(!raw_data==remove.value)]
  #eliminate :   which is "00111010"
  remove.value<-"00111010"
  raw_data<-raw_data[which(!raw_data==remove.value)]
  #eliminate -   which is "00101101"
  remove.value<-"00101101"
  raw_data<-raw_data[which(!raw_data==remove.value)]
  #eliminate period   which is "00101110"
  remove.value<-"00101110"
  raw_data<-raw_data[which(!raw_data==remove.value)]
  
  #split "01110100" like strings to "0" "1" ...
  s1 <- as.character(raw_data) 
  s2 <- strsplit(s1, "") 
  s3 <- unlist(s2) 
  
  #remove other symbols,only keep 0 and 1
  s3<-s3[s3=="0" | s3=="1"]
  
  return(s3)
  
}