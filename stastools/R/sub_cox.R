sub_cox <-
function(surtime,surstatus,var,data,sub,subpos="up"){
  res<-NULL
  for (i in 1:length(sub)) {
    tmp<-sub_unicox(surtime=surtime,surstatus = surstatus,var = var,data = data,subfactor = sub[i],subpos)
    res<-rbind(res,tmp)
  }
  return(res)
}
