geneIDTrans <- function(varList, annoTable, from = "Ensembl_ID", to = "HGNC_Symbol") {
  # Message about the number of variables provided
  message(paste(length(varList), "variables were provided."))
  
  # Match the variable names with the annotation table
  match_loc <- match(varList, annoTable[,from])
  
  # Message about the number of unmatched genes
  message(paste(sum(is.na(match_loc)), "variables not matched:", 
            paste(varList[is.na(match_loc)],collapse=";")))
  
  # Replace variable names, keeping original names for unmatched cases
  res <- ifelse(is.na(match_loc), varList, annoTable[match_loc, to])
  
  return(res)
}