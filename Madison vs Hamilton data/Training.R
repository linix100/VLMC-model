Training<-function(Training_Data,cutoff_number){
  library(VLMC)
  Training_Bi<-Training_Data
  vc<-vlmc(Training_Bi,cutoff=cutoff_number)
  print(vc)
  draw(vc,cumul=FALSE)
}