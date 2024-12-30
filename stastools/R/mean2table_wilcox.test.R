mean2table_wilcox.test <-
  function(data,compare_row,compare_group){
    tmp_test <- t.test(data[,compare_row] ~ data[,compare_group])
    tmp_wilcox <- wilcox.test(data[,compare_row] ~ data[,compare_group])
    
    tmp_p <- tmp_wilcox$p.value
    tmp_ci <- tmp_test$conf.int
    tmp_mean <- tmp_test$estimate
    
    tmp <- data.frame("char" = compare_row,
                      "A" = tmp_mean[[1]],
                      "B" = tmp_mean[[2]],
                      "LCI" = tmp_ci[[1]],"UCI" = tmp_ci[[2]],
                      "CI" = paste0(signif(tmp_ci[1],2),"-",signif(tmp_ci[2],2)),
                      "pval" = signif(tmp_p))
    colnames(tmp) <- c("char",names(tmp_mean),"LCI","UCI","CI","pval")
    return(tmp)
    
  }
