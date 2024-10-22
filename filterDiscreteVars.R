
# Keep discrete variables whose mode category doesn't dominate
filterDiscreteVars <- function(df, min.count, min.perc) {
  # Function to calculate the mode of a vector
  getMode <- function(vec) {
    mode <- names(which.max(table(vec)))
    return(mode)
  }
  
  # Determine the threshold for filtering based on min.count and min.perc
  max.count <- nrow(df) - max(min.count, min.perc * nrow(df))
  
  # Apply filtering: Keep variables where mode occurrences are less than max.count
  geneLog <- apply(df, 2, function(x) {
    sum(x == getMode(x)) < max.count
  })
  
  return(geneLog)
}
