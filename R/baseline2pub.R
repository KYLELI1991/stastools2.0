baseline2pub <-
function(data){
  
  
  # check the NAs
  if(anyNA(data)){
    data[is.na(data)]<-"Not Availabe"
  }
  
  # make the table list, use lappy
  
  datfreq<-lapply(data,table)
  datprop<-lapply(datfreq,prop.table)
  
  # testing not run
  if(F){
    character<-c(names(datfreq[1]),names(datprop[[1]]))
    NOP<-c(NA,paste0(datfreq[[1]],"(",round(datprop[[1]]*100,2),"%",")"))
    tmp_base<-data.frame("character"=character,"N"=NOP)
  }
  
  baseline_all<-NULL #
  ncol(data)
  for(i in 1:ncol(data)){
    character<-c(names(datfreq[i]),names(datprop[[i]]))
    NOP<-c(NA,paste0(datfreq[[i]],"(",round(datprop[[i]]*100,2),"%",")"))
    tmp_base<-data.frame("Character"=character,"N"=NOP)
    baseline_all<-rbind(baseline_all,tmp_base) 
  }
  
  return(baseline_all)
}
