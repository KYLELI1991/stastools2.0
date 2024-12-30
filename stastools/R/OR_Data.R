OR_Data <-
function(OR){
  # get the information
  text<-OR$measure
  data<-OR$data
  pval<-OR$p.value
  # trime the data form
  data2<-data[-nrow(data),-ncol(data)]
  # merge the data
  OR_data<-cbind(data2,text,pval)
  OR_data<-as.data.frame.matrix(OR_data)
  
  return(OR_data)
}
