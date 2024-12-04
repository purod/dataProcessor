# 1.4. add suffix based on data type 
add.Type <- function(df, excludeVars=NULL){
  
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  
  tagList <- apply(df2, 2, function(x){
    if(Hmisc::all.is.numeric(x, extras = NA)){
      if(length(table(x)) == 2){
        '_binary'
      }else if(length(table(x)) == 3){
        '_categorical'
      }else{
        '_continuous'
      }
    }else if(length(table(x)) == 2){
      '_binary'
    }else{
      '_categorical' 
    }
  }) %>% unlist()
  
  colnames(df2) <- paste0(colnames(df2), tagList)
  
  df <- data.frame(df1,df2)
}

add.suffix <- function(df, suffix="NULL",excludeVars=NULL){
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  colnames(df2) <- paste0(colnames(df2), "_", suffix)
  df <- data.frame(df1,df2)
}

add.preffix <- function(df, preffix="NULL",excludeVars=NULL){
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  colnames(df2) <- paste0(preffix, "_", colnames(df2))
  df <- data.frame(df1,df2)
}

add.ffix <- function(df, preffix="NULL", suffix="NULL", excludeVars=NULL){
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  colnames(df2) <- paste0(preffix, "_", colnames(df2), "_", suffix)
  df <- data.frame(df1,df2)
}