wcount<-function(vc){
  library(VLMC)
  #load both training and query matrix
  tri<-wmatrix(1)
  qua<-wmatrix(2)
  
  #loop to have a 
  i<-1
  trirows<-nrow(tri)
  print(trirows)
  for(i in 1:trirows){
    #find the reversed string of the VLMC context
    idnumber<-predict.vlmc(vc,tri[i,],type="id")
    idnumber<-idnumber[which(idnumber!= NA)]
    chatxt<-id2ctxt(idnumber,alpha="abcdefghijklmnopqrstuvwxyz")
    chatxt2<-sapply(strsplit(chatxt,split=""),function(str) {paste(rev(str),collapse="")})
    
    #chatxt2<-chatxt2[which(chatxt2!="AN")]
    tables[i]<-as.data.frame(table(chatxt2))
    
  }
  
  return(tables)
}