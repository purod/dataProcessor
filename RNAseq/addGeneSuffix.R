addGeneSuffixDF <- function(df, geneAnnoTable, excludeVars=NULL){
	  
  if(is.null(excludeVars)){
    ensembl_ID = colnames(df)
  }else{
    excludeVars.pos = match(excludeVars, colnames(df))
    ensemble.pos = c(1:ncol(df))[-excludeVars.pos]
    
	  ensembl_ID <- colnames(df)[ensemble.pos]
  }
  # duplicates
  if( sum(duplicated(ensembl_ID)) > 1 ){
	      message("There are duplicates in the ensemble ID!")
    }

  # use gene symbols as the suffixes
  ensemble_gene = geneAnnoTable$Ensembl_HGNC_dot[match(ensembl_ID, geneAnnoTable$Ensembl_ID)]
    
  if(is.null(excludeVars)){
    colnames(df) = ensemble_gene
  }else{
    colnames(df)[c(excludeVars.pos, ensemble.pos)] = c(excludeVars, ensemble_gene)
  }
  
  return(df)
}