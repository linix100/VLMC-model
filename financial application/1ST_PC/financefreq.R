financial_freq<-function(){
  source('~/Documents/R/financial/1ST_PC/financialscan.R')
  source('~/Documents/R/financial/1ST_PC/financematrix.R')
  vc<-financescan()
  #load both training and query matrix
  #tri<-wmatrix(1)
  qua<-financematrix(2)
  i<-1
  #trirows<-nrow(tri)
  #trirows<-2
  #for(i in 1:trirows){
  #for(i in 1:14){
  for(i in 1:15) {
    #n1<-predict(vc,tri[i,],type="id.node")
    n1<-predict(vc,qua[i,],type="id.node")
    t_end<-length(n1)
    n2<-n1[2:t_end]
    n2<-n2[which(n2>0)]
    #abfghlmnqrstw    #bfghlm
    n3<-id2ctxt(n2,alpha="abfghlmnqrstw")
    
    n4<-sapply(strsplit(n3,split=""),function(str) {paste(rev(str),collapse="")})
    #print(n4)
    #n5<-as.data.frame(table(n4))
    #print(n5)
    if(i==1){
      mat<-n4
    }else{
      mat<-rbind(mat,n4)
    }
  }
  #print(mat)
  mat2<-t(mat)
  write.csv(mat2,"/Users/Linix100/Documents/R/financial/1ST_PC/financial_1PC12.csv")
  
  return("success!")
} 