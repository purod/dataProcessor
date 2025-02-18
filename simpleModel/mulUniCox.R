# library("survcomp")
# Function IV: univariate analysis
mulUniCox <- function(mydata, timen, statusn, excludeVars=NULL, pvalue_cut=1){
  
  time <- mydata[,timen] %>% unlist()
  status <- mydata[,statusn] %>% unlist() %>% as.numeric()
  varData <- apply( mydata[,!colnames(mydata)%in%c(timen, statusn, excludeVars)],2,scale)
  
  # formulas for each variable
  covariates <- colnames(varData)
  
  mydata[,covariates] <- varData
  
  uniFormulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~scale(', x,')')))
  
  uniModels <- lapply(uniFormulas, function(x){coxph(x, data = mydata)})
  # Extract data
  uniResults <- 
    lapply(uniModels, function(x){
      
      x.summary <- summary(x)
      
      # concordance index computed in a way different from coxph
      # look at Doris Gao's Blog for more details
      if(is.null(x$na.action)){
        cindex <- concordance.index(predict(x), surv.time=time, surv.event=status, 
                                    na.rm=T)
      }else{
        cindex <- concordance.index(predict(x), 
                                    surv.time=time[-as.vector(x$na.action)], 
                                    surv.event=status[-as.vector(x$na.action)])
      }
      
      
      Variablen = gsub("scale\\(|\\)","",rownames(x.summary$coefficients))
      res <- data.frame(
        
        Variable = ifelse(grepl("binary\\w+|categorical", Variablen),
                          gsub('(.*binary|.*categorical)(.*)','\\1 = \\2', Variablen),
                          Variablen),
        
        Pts_n = x.summary$n,
        Event_n = x.summary$nevent,
        Event_prop = x.summary$nevent / x.summary$n,
        
        HR = signif(x.summary$conf.int[,1], digits=3),
        HR_95percent_CI = paste0("(", signif(x.summary$conf.int[,3],digits=3), "-", 
                                 signif(x.summary$conf.int[,4], digits=3), ")"),
        
        Concordance = signif(cindex$c.index, digits=3),
        C_Index_95percent_CI = paste0("(", signif(cindex$lower, digits=3), "-", 
                                      signif(cindex$upper, digits=3), ")"),
        
        p_value = signif(x.summary$coefficients[,5], digits=3),
        
        FDR = signif(p.adjust(x.summary$coefficients[,5], 
                              method="fdr", n=length(covariates)), digits=3),
        
        stringsAsFactors = F
      ) 
      # remove rownames
      rownames(res) <-  c()  
      return(res)
    })
  
  uniResultsDF <- do.call(rbind, uniResults) 
  rownames(uniResultsDF) <- c()
  
  # filtering by p-value
  uniResultsDF %<>% filter(p_value < pvalue_cut) %>% arrange(p_value) %>% 
    dplyr::select(-Concordance, -C_Index_95percent_CI)
  
  return(uniResultsDF)
}