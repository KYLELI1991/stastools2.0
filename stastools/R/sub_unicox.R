sub_unicox <-
function(surtime,surstatus,var,data,subfactor,subpos="up"){
  # split the data
  sp<-split(data,data[,subfactor])
  tmp<-NULL
  submun<-NULL
  for(i in 1:length(sp)){
    unitmp<-sigunicox(surtime = surtime,surstatus = surstatus,var = var,data = sp[[i]])
    tmp<-rbind(tmp,unitmp)
    N<-nrow(sp[[i]])
    submun<-c(submun,N) # subnumber
    
  }
  # subgroupnames and num
  
  subgroup<-data.frame("Subgroup"=names(sp),"N"=submun)
  
  result<-cbind(subgroup,tmp)
  result$Subgroup<-as.character(result$Subgroup)
  
  # add if function
  if(subpos=="up"){
    # subgroup head on the top
    subhead<-c(subfactor,rep(NA,ncol(result)-1))
    coln<-colnames(result)
    result<-rbind(subhead,result)
    colnames(result)<-coln
  }
  else if(subpos=="left"){
    #subgroup on the left
    subhead<-c(subfactor,rep(NA,nrow(result)-1))
    coln<-colnames(result)
    result<-cbind(subhead,result)
    colnames(result)<-c("subcat",coln)
  }
  
  return(result)
  
}
