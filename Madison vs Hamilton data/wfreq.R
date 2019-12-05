game1<-function(vc){
  library(VLMC)
  #load both training and query matrix
  #tri<-wmatrix(1)
  qua<-wmatrix(2)
  i<-1
  #trirows<-nrow(tri)
  #trirows<-2
  #for(i in 1:trirows){
  #for(i in 1:14){
  for(i in 1:6) {
    #n1<-predict(vc,tri[i,],type="id.node")
    n1<-predict(vc,qua[i,],type="id.node")
    t_end<-length(n1)
    n2<-n1[2:t_end]
    n2<-n2[which(n2>0)]
    n3<-id2ctxt(n2,alpha="*abcdefghijklmnopqrstuvwxyz")
    n4<-sapply(strsplit(n3,split=""),function(str) {paste(rev(str),collapse="")})
    n5<-as.data.frame(table(n4))
  if(i==1){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n1.csv")
    
  }
  if(i==2){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n2.csv")
    
  }
  if(i==3){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n3.csv")
    
  }
  if(i==4){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n4.csv")
  }
  if(i==5){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n5.csv")
  }
  if(i==6){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n6.csv")
  }
  if(i==7){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n7.csv")
  }
  if(i==8){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n8.csv")
  }
  if(i==9){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n9.csv")
  }
  if(i==10){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n10.csv")
  }
  if(i==11){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n11.csv")
  }
  if(i==12){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n12.csv")
  }
  if(i==13){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n13.csv")
  }
  if(i==14){
    write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n14.csv")
  }
    if(i==15){
      write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n15.csv")
      
    }
    if(i==16){
      write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n16.csv")
      
    }
    if(i==17){
      write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n17.csv")
      
    }
    if(i==18){
      write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n18.csv")
      
    }
    if(i==19){
      write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n19.csv")
      
    }
    if(i==20){
      write.csv(n5,"/Users/Linix100/Documents/R/Madison vs Hamilton data/n20.csv")
      
    }
   
  }
    return("success!")
  #}
}
