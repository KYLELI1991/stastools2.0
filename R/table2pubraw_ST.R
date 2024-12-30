table2pubraw_ST <-
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
  
  # R x C test
  options(warn = 2)
  chi<-try(test<-chisq.test(tmp1))
  if("try-error" %in% class(chi))
    chi<-fisher.test(tmp1,simulate.p.value = T)
  
  pval<-round(chi$p.value,3)
  pdata<-data.frame("p.value"=rep(NA,length(rownames(a))),"Method"=rep(NA,length(rownames(a))))
  rownames(pdata)<-rownames(a)
  pdata[1,1]<-pval
  pdata[1,2]<-chi$method
  options(warn = 0)
  result<-cbind(a,pdata)
  
  # add char name
  char<-data.frame("char"= rownames(result))
  result<-cbind(char,result)
  
  return(result)
  
  
}
