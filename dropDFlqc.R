dropDFlqc <- function(df, empty2NA=FALSE, dropMiss=NULL, minNonMissing=NULL, dropConstant=FALSE){
  
  df <- df %>% 
    mutate_if(is.logical, as.character) %>%
    mutate_if(is.factor, as.character)
  # convert "" and " " into NA
  if(empty2NA){ 
    df <- df %>% 
      mutate(across(where(is.character), ~na_if(., ""))) %>%
      mutate(across(where(is.character), ~na_if(., " ")))
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
  # Too few non-missing values
  low_nonmiss_vars <- c()
  if (!is.null(minNonMissing)) {
    nonmiss_count <- colSums(!is.na(df))
    low_nonmiss_vars <- df_name[nonmiss_count < minNonMissing]
    if (length(low_nonmiss_vars) > 0)
      message(sprintf("Excluding %d variables with < %d non-missing values: %s\n", 
                      length(low_nonmiss_vars), minNonMissing, paste(low_nonmiss_vars, collapse = ", ")))
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
  exclude_vars <- unique( c(high_miss_vars, high_imbalance_vars, low_nonmiss_vars) )

  new_df <- df %>% dplyr::select(-one_of(exclude_vars))
  return(new_df)
}