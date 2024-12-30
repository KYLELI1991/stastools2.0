sigunicox <-
function(surtime,surstatus,var,data){
  if(length(table(data[,var]))==2){
    t<-grep(surtime,colnames(data),fixed = T)
    s<-grep(surstatus,colnames(data),fixed = T)
    
    sur_data<-Surv(time = data[,t],event = data[,s])
    FML<-as.formula(paste0("sur_data~",var)) # creat a formula
    uniCOX<-coxph(FML,data) # cox calling
    sum<-try(a<-summary(uniCOX))
    if("try-error" %in% class(sum)){
      arm<-"NA" # creat the Arm name
      HR<-"NA"
      pval<-"NA"
      CI<- "NA"
      LCI<-"NA"
      UCI<-"NA"
      result<-data.frame("Arm"=arm,"HR"=HR,"LCI"=LCI,"UCI"=UCI,"CI"=CI,"p.value"=pval)
    }else{
      sum<-summary(uniCOX) # get the summary list
      arm<-paste0(rev(uniCOX$xlevels[[1]]),collapse = " Versus ") # creat the Arm name
      HR<-round(sum$coefficients[2],3)
      pval<-round(sum$coefficients[5],3)
      CI<- paste0(round(sum$conf.int[3:4],3),collapse = "-")
      LCI<-round(sum$conf.int[3],3)
      UCI<-round(sum$conf.int[4],3)
      result<-data.frame("Arm"=arm,"HR"=HR,"LCI"=LCI,"UCI"=UCI,"CI"=CI,"p.value"=pval)
      
    }
    
    
  }else{
    arm<-"NA" # creat the Arm name
    HR<-"NA"
    pval<-"NA"
    CI<- "NA"
    LCI<-"NA"
    UCI<-"NA"
    result<-data.frame("Arm"=arm,"HR"=HR,"LCI"=LCI,"UCI"=UCI,"CI"=CI,"p.value"=pval)}
  
  
  
}
