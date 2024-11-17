## convert table result to data frame
tableVec2df <- function(name_vector, ntop=NULL){
  ntop=ifelse(is.null(ntop),length(name_vector), ntop)
  tableSort <- head( sort(name_vector, decreasing = T), ntop)
  name <- names(tableSort)
  Frequency <- as.vector(tableSort)
  df <- data.frame(name, Frequency)
  df$name <- factor(df$name, levels=df$name)
  return(df)
}
tableVecs2df <- function(name_df){
  message("Use as.data.frame.matrix to convert into wide matrix!")
  df <- reshape2::melt(name_df)
  return(df)
}