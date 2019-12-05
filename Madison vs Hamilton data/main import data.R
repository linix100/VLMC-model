main_import_function<-function(data_address,slices){
   #save data into matrix x
   a<-importdata(data_address)
   x<-splitintogroups(a,slices)
   return(x)
   
   
}