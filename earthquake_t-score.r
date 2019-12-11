#this function provide you the earthquake data t score
#It by default training the 1:400 area and compare with the region before and after earthquake respectively.

etscore<-function(compare_with){
  source('~/Documents/R/earth/Earthscan.R')
  source('~/Documents/R/earth/Earthquake_Matrix.r')
  source('~/Documents/R/earth/eloglike.r')
  vc<-escan()
  intra<-eloglike(vc,1)
  if(compare_with==2){
      inter<-eloglike(vc,2)
      t<-(mean(intra)-mean(inter))/sqrt(var(intra)/40+var(inter)/10)

  }else{
      inter<-eloglike(vc,3)
      t<-(mean(intra)-mean(inter))/sqrt(var(intra)/40+var(inter)/4)
  }
  return(t)
}