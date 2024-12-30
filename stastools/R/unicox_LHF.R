unicox_LHF <-
function(var,surtime,surstatus,Dat){
  
  sur_data<-Surv(surtime,surstatus)
  FML<-as.formula(paste0("sur_data~",var))
  uniCOX<-coxph(FML,Dat)
  sum<-summary(uniCOX)
  #pvalue
  pvalue<-c(NA,NA)
  for(i in (1:nrow(sum$coefficients))){
    tmp<-paste0("p",i)
    tmp<-round(sum$coefficients[i,5],3)
    pvalue<-c(pvalue,tmp)
  }
  #conf.int
  #LCI
  LCI<-c(NA,"ref")
  for(i in (1:nrow(sum$coefficients))){
    tmp<-paste0("con",i)
    tmp<-paste0(round(sum$conf.int[i,3],3))
    LCI<-c(LCI,tmp)
  }
  
  #UCI
  UCI<-c(NA,"ref")
  for(i in (1:nrow(sum$coefficients))){
    tmp<-paste0("con",i)
    tmp<-paste0(round(sum$conf.int[i,4],3))
    UCI<-c(UCI,tmp)
  }
  
  #CI
  CI<-c(NA,"ref")
  for(i in (1:nrow(sum$coefficients))){
    tmp<-paste0("con",i)
    tmp<-paste0(round(sum$conf.int[i,3],2),"-",round(sum$conf.int[i,4],2))
    CI<-c(CI,tmp)
  }
  #HR 
  HR<-c(NA,"ref")
  for(i in (1:nrow(sum$coefficients))){
    tmp<-paste0("HR",i)
    tmp<-round(sum$coefficients[i,2],3)
    HR<-c(HR,tmp)
  }
  #CHAR name
  Char<-c(var,uniCOX$xlevels[[1]])
  
  result<-data.frame("characteristics"=Char,"Hazard Ratio"=HR,
                     "LCI"=LCI,"UCI"=UCI,
                     "CI"=CI,"P value"=pvalue)
  return(result)
}
