table2pubraw <-
function(data,row,col,propdir,frname){
  tmptable<-with(data,table(row,col))
  tmpprop<-prop.table(tmptable,propdir)
  tmp1<-as.data.frame.matrix(tmptable)
  tmp2<-as.data.frame.matrix(tmpprop)
  factornum<-length(tmp1)
  a<-cbind(tmp1,tmp2)
  b<-colnames(a)
  for(i in 1:factornum){
    
    a[,length(a)+1]<-paste0(a[,i],"(",round(a[, factornum + i] *100,2),"%",")")
    
  }
  name1<-paste0(colnames(tmp1),"_N")
  name2<-paste0(colnames(tmp1),"_P")
  name3<-paste0(colnames(tmp1),"_Pub")
  colnames(a)<-c(name1,name2,name3)
  
  headna<-c(rep(NA,length(colnames(a))))
  rname<-c(frname,rownames(a))
  a<-rbind(headna,a)
  rownames(a)<-rname
  
  # add char name
  char<-data.frame("char"= rownames(a))
  result<-cbind(char,a)
  fac<-rep(char[1,1],nrow(result))
  result<-cbind(fac,result)
  result<-result[-1,]
  return(result)
  
  
}
