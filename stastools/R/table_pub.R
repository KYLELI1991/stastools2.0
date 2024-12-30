table_pub <-
function(data,row,col,propdir,frname){
  if(class(data) != "data.frame")
    data<-as.data.frame(data)
  tmptable<-with(data,table(row,col))
  tmpprop<-prop.table(tmptable,propdir)
  tmp1<-as.data.frame.matrix(tmptable)
  tmp2<-as.data.frame.matrix(tmpprop)
  factornum<-length(tmp1)
  a<-cbind(tmp1,tmp2)
  b<-colnames(a)
  oldlen<-length(a)
  for(i in 1:factornum){
    
    a[,length(a)+1]<-paste0(a[,i],"(",round(a[, factornum + i] *100,2),"%",")")
    
  }
  newlen<-length(a)
  newtmp<-a[,(oldlen+1):newlen]
  colnames(newtmp)<-b[1:(length(b)/2)]
  
  headna<-c(rep(NA,factornum))
  rname<-c(frname,rownames(newtmp))
  result<-rbind(headna,newtmp)
  rownames(result)<-rname
  
  # add char name
  char<-data.frame("char"= rownames(result))
  result<-cbind(char,result)
  
  return(result)
}
