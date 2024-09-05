annoENSGs <- function(geneIDs){
  
  library(biomaRt)
  # Extract the ensemble IDs
  ENSGIDs <- gsub(".*(ENSG[0-9]{11}).*","\\1",geneIDs)
  
  # Get the results from biomaRt
  ensembl <- useEnsembl(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")
  mart_results = getBM(attributes = c("ensembl_gene_id", "description"
                                      # "mim_gene_description","entrezgene_description","wikigene_description"
  ),
  filters = "ensembl_gene_id",
  values = ENSGIDs, 
  mart = ensembl) %>% 
    mutate(description=gsub("(.*)\\[.*\\]","\\1",description))
  gene_description <- mart_results$description[match(ENSGIDs, mart_results$ensembl_gene_id)]
  
  return(gene_description)
}



