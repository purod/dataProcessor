mulUniLogLinear <- function(mydata, target, excludeVars=NULL, pvalue_cut=1){
  
  # formulas for each variable
  covariates <- colnames(mydata)[ !colnames(mydata)%in%c(target, excludeVars) ]
  
  uniResults <- lapply(covariates,
                       
                       function(var) {
                         
                         formula  <- as.formula(paste(target, "~", var))
                         if( length(table(mydata[,target]))==2 ){
                           res.regression <- glm(formula, data = mydata, family = binomial)
                           oddsRatio <- exp(cbind(OR = coef(res.regression), suppressMessages(confint((res.regression)))))
                           
                           x.summary <- summary(res.regression)
                           Variablen <- rownames(x.summary$coefficients)[-1]
                           
                           res <- data.frame(
                             
                             Variable = ifelse(grepl("binary\\w+|categorical", Variablen),
                                               gsub('(.*binary|.*categorical)(.*)','\\1 = \\2', Variablen),
                                               Variablen),
                             odds_ratio = signif(oddsRatio[2,1], digits=3),
                             CI95 = paste0(signif(oddsRatio[2,2],digits=3),",",
                                            signif(oddsRatio[2,3],digits=3)),
                             p_value = signif(x.summary$coefficients[,4][-1], digits=3),
                             FDR = signif(p.adjust(x.summary$coefficients[,4][-1], 
                                          method="fdr", n=length(covariates)), digits=3),
                              stringsAsFactors = F
                             )
                         }else{
                           res.regression <- lm(formula, data = mydata)
                           
                           x.summary <- summary(res.regression)
                           Variablen <- rownames(x.summary$coefficients)[-1]
                           adj.r.squared = signif(x.summary$adj.r.squared, digits=3)
                           
                           res <- data.frame(
                             
                             Variable = ifelse(grepl("binary\\w+|categorical", Variablen),
                                               gsub('(.*binary|.*categorical)(.*)','\\1 = \\2', Variablen),
                                               Variablen),
                             coeff.estimate = signif(x.summary$coefficients[,1][-1], digits=3),
                             coeff.std = signif(x.summary$coefficients[,2][-1], digits=3),
                             adj.r.squared = ifelse( adj.r.squared>0, adj.r.squared,0 ),
                             p_value = signif(x.summary$coefficients[,4][-1], digits=3),
                             FDR = signif(p.adjust(x.summary$coefficients[,4][-1], 
                                                   method="fdr", n=length(covariates)), digits=3),
                             stringsAsFactors = F
                           )
                         }
                       })

  uniResultsDF <- do.call(rbind, uniResults) 
  rownames(uniResultsDF) <- c()

  # filtering by p-value
  uniResultsDF %<>% filter(p_value < pvalue_cut) %>% arrange(p_value)

  return(uniResultsDF)
}