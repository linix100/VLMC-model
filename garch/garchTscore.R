#this function provide you the data t score

garchtscore<-function(){
  source('~/Documents/R/garch/garchscan.R')
  source('~/Documents/R/garch/garchMatrix.R')
  source('~/Documents/R/garch/garchloglike.R')
  vc<-garchscan()
  intra<-garchloglike(vc,1)
  inter<-garchloglike(vc,2)
  t<-(mean(intra)-mean(inter))/sqrt(var(intra)/45+var(inter)/10)
    
  
  return(t)
}