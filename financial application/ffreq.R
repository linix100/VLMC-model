ffreq<-function(vc){
  library(VLMC)
  #load both training and query matrix
  #tri<-wmatrix(1)
  qua<-fmatrix(2)
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
    n3<-id2ctxt(n2,alpha="bdejkntw")
    n4<-sapply(strsplit(n3,split=""),function(str) {paste(rev(str),collapse="")})
    n5<-as.data.frame(table(n4))
    if(i==1){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n1.csv")
      
    }
    if(i==2){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n2.csv")
      
    }
    if(i==3){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n3.csv")
      
    }
    if(i==4){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n4.csv")
    }
    if(i==5){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n5.csv")
    }
    if(i==6){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n6.csv")
    }
    if(i==7){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n7.csv")
    }
    if(i==8){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n8.csv")
    }
    if(i==9){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n9.csv")
    }
    if(i==10){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n10.csv")
    }
    if(i==11){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n11.csv")
    }
    if(i==12){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n12.csv")
    }
    if(i==13){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n13.csv")
    }
    if(i==14){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n14.csv")
    }
    if(i==15){
      write.csv(n5,"/Users/Linix100/Documents/R/financial/most distinctive context/n15.csv")
      
    }
    
    
  }
  return("success!")
}