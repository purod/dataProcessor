knnImputePC <- function(df, excludeVars = NULL, PCVarsPattern=NULL, noImpVars = NULL){
  
  #'  @param df : input data frame for data columns imputed
  #'  @param excludeVars : variables not participating in the imputation
  #'  @param PCVarsPattern : regular expression defining variables whose principal
  #'                         component matrix will be used for imputation
  #'  @param noImpVars : variables not imputed, but provide info for imputation process
  #' 
  
  # do we have mispelled variables
  if((!is.null(excludeVars) & any(!excludeVars %in% names(df))) |
     (!is.null(noImpVars) & any(!noImpVars %in% names(df)))){
    warning("\nCan't find some variables in excludeVars or noImpVars\n")
  }
  
  # use principle components to perform the imputation
  if(!is.null(PCVarsPattern)){
    df.PCs <- df %>% dplyr::select(matches(PCVarsPattern)) %>% prcomp(.,scale=T)
    df.data <- df %>% dplyr::select(-matches(PCVarsPattern)) %>% 
      bind_cols( data.frame(df.PCs$x) )
  }else{
    df.data = df
  }
  
  df1 <- df.data %>% dplyr::select( -all_of(excludeVars) )
  df2 <- df.data %>% dplyr::select( -all_of(c(excludeVars,noImpVars)) )

  # positions of categorical variables
  categoryID <-  which(sapply(df1, is.character)) %>% unname()
  
  # imputed matrix
  dfImputed <- df1 %>% 
    mutate_if(is.character,as.factor) %>% 
    data.matrix() %>% 
    bnstruct::knn.impute(k = 10, cat.var = categoryID) %>% 
    as.data.frame()
  
  # missing variables
  charMiss <- colnames(df2)[sapply(df2,function(x){
    sum(is.na(x)) > 0 & is.character(x)
  })]
  message("\nCategorical vars imputed:", paste(charMiss, collapse = ","))
  
  conMiss <- colnames(df2)[sapply(df2,function(x){
    sum(is.na(x)) > 0 & is.numeric(x)
  })]
  message("\nContinuous vars imputed:", paste(conMiss, collapse = ","))
  
  # fill up missing values
  df.data[,conMiss] = dfImputed[,conMiss]
  df.data[,charMiss] <- data.frame(sapply(charMiss, function(x){
    levels(as.factor(df.data[[x]]))[dfImputed[[x]]]}), stringsAsFactors = F)
  
  # replace principle components with raw variables
  if(!is.null(PCVarsPattern)){
    df.final <- df.data %>% dplyr::select(-matches("^PC\\d+")) %>%
      bind_cols( df %>% dplyr::select(matches(PCVarsPattern)) )
  }else{
    df.final = df.data
  }
  return(df.final)
}