addGeneSuffix <- function(df,excludeVars=NULL){
	  
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

    ## the loading of this library will disrupt the usage of select in dplyr
    mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", host="https://www.ensembl.org",
			                                path="/biomart/martservice", dataset="hsapiens_gene_ensembl")
    G_list = biomaRt::getBM(filters= "ensembl_gene_id", attributes=c("hgnc_symbol","ensembl_gene_id"),
			                              values=ensembl_ID, mart= mart) %>%
        # replace - with . for gene names
        mutate(hgnc_symbol=gsub( "-",".",hgnc_symbol))

  # use gene symbols as the suffixes
  ensemble_gene = gsub("_$|_NA$", "",
                       paste0(ensembl_ID,"_", 
                              G_list$hgnc_symbol[match(ensembl_ID, G_list$ensembl_gene_id)])
                       )
  
  if(is.null(excludeVars)){
    colnames(df) = ensemble_gene
  }else{
    colnames(df)[c(excludeVars.pos, ensemble.pos)] = c(excludeVars, ensemble_gene)
  }
  
  return(df)
}


