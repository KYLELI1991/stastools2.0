table_pub_ST <-
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
  
  # R x C test
  options(warn = 2)
  chi<-try(test<-chisq.test(tmp1))
  if("try-error" %in% class(chi))
    chi<-fisher.test(tmp1,simulate.p.value = T)
  
  pval<-round(chi$p.value,3)
  pdata<-data.frame("p.value"=rep(NA,length(rownames(result))),"Method"=rep(NA,length(rownames(result))))
  rownames(pdata)<-rownames(result)
  pdata[1,1]<-pval
  pdata[1,2]<-chi$method
  options(warn = 0)
  result<-cbind(result,pdata)
  
  # add char name
  char<-data.frame("char"= rownames(result))
  result<-cbind(char,result)
  
  return(result)
  
}
