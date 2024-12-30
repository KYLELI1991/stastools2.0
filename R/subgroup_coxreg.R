subgroup_coxreg <-
function(surtime,surstatus,var,data,sublist,subpos="up"){
  subcox_re<-NULL
  for (i in 1:length(sublist)) {
    tmp1<-sub_cox(surtime = surtime,
                  surstatus = surstatus,
                  var = var,
                  sub = sublist[i],
                  subpos = "up",
                  data = data
    )
    subcox_re<-rbind(subcox_re,tmp1)
    
  }
  return(subcox_re)
}
