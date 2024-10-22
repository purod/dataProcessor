checkDFqc <- function(df, empty2NA=FALSE, dropMiss=NULL, dropConstant=FALSE){
  
  # QC1: convert "" and " " into NA
  if(empty2NA){ 
    df <- df %>% 
      mutate_all(na_if, "") %>% 
      mutate_all(na_if, " ")
    message("Convert empty units into NA.\n")
  }
  
  df_name <- colnames(df)
  df_nrow <- nrow(df)
  
  # Exclude variables with missing rate larger than the cutoff
  high_miss_vars <- c()
  if(!is.null(dropMiss)) {
    high_miss_vars <- df_name[ colSums(is.na(df))/df_nrow > dropMiss  ]
    message(paste0('Excluding ', length(high_miss_vars),' variables with missing rate > ',
                  dropMiss, ': ', paste(high_miss_vars, collapse=",")))
    cat('\n')
  }
  
  # Exclude constant variables 
  high_imbalance_vars <- c()
  if(dropConstant){
    high_imbalance_vars <- df_name[ apply(df,2,dplyr::n_distinct)==1 ]
    message(paste0('Excluding ', length(high_imbalance_vars),' constant variables: ',
                   paste(high_imbalance_vars, collapse=",")))
    cat('\n')
  }

  # Variables to be excluded
  exclude_vars <- unique( c(high_miss_vars, high_imbalance_vars) )

  new_df <- df %>% dplyr::select(-one_of(exclude_vars))
  
  return(new_df)
}