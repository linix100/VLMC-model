#this function provide you the data t score
financetscore<-function(){
  source('~/Documents/R/financial/1ST_PC/financialscan.R')
  source('~/Documents/R/financial/1ST_PC/financematrix.R')
  source('~/Documents/R/financial/1ST_PC/financeloglike.R')
  vc<-financescan()
  print(vc)
  draw(vc)
  intra<-financeloglike(vc,1)
  print("intra loglikelihood")
  print(intra)
  inter<-financeloglike(vc,2)
  print("inter loglikelihood")
  print(inter)
  
  t<-(mean(intra)-mean(inter))/sqrt(var(intra)/12+var(inter)/15)
  print("t-score")
  
  
  return(t)
}