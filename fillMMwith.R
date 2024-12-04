# simply fill in the missing values with median
fillMM <- function(df){
  miss.pos <- which(is.na(df),arr.ind=T)
  if(nrow(miss.pos) == 0){
    return(df)
  }else{
    df[miss.pos] <- unlist(apply(df,2,median,na.rm=T))[miss.pos[,2]]
    return(df)
  }
}