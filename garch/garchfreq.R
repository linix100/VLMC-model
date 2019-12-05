garch_freq<-function(){
  source('~/Documents/R/garch/garchscan.R')
  source('~/Documents/R/garch/garchMatrix.R')
  vc<-garchscan()
  #load both training and query matrix
  #tri<-wmatrix(1)
  qua<-garchmatrix(1)
  i<-1
  #trirows<-nrow(tri)
  #trirows<-2
  #for(i in 1:trirows){
  #for(i in 1:14){
  for(i in 1:45) {
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
  write.csv(mat2,"/Users/Linix100/Documents/R/garch/garch_1.csv")
  
  return("success!")
}