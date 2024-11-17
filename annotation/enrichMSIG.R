# Load required libraries
library("clusterProfiler")
library("msigdbr")
library("dplyr")

# Define the hallmark enrichment analysis function
enrichMSIG <- function(hgnc_list, background_list=NULL, 
                       species = "Homo sapiens", category = "H") {


  message("Options for category include: Hallmark gene sets(H), Curated gene Sets(KEGG, Reactome), GO gene sets(C5), Oncogenic signatures(C6)")
  message(paste(length(hgnc_list), "target genes were provided."))
  message(paste(length(background_list), "background genes were provided."))
  

  # Step 1: Prepare hallmark gene sets
  msig_h <- msigdbr(species = species, category = category) %>% 
    dplyr::select(gs_name, gene_symbol) 
  msig_genes <- unique(msig_h$gene_symbol)
  
  message(paste(sum(hgnc_list%in%msig_genes), "target genes were identified.")) 


  # Step 2: Convert gene symbols to Entrez IDs

  if(is.null(background_list)){
  message(paste(length(msig_genes), "background genes."))
    hallmark_results <- enricher(gene = hgnc_list,
                                TERM2GENE = msig_h) %>%
     as_tibble()
  }else{
    message(paste(sum(background_list%in%msig_genes), "background genes were identified.")) 

    # Step 4: Perform hallmark enrichment analysis
    hallmark_results <- enricher(gene = hgnc_list,
                                 universe = background_list,
                                 TERM2GENE = msig_h) %>%
     as_tibble()
  }
    
  return(hallmark_results)
}

# Usage example:
# risky_pink <- c("TP53", "BRCA1", "EGFR")  # Replace with your gene list
# hallmark_results <- hallmark_enrichment_analysis(risky_pink)
# View(hallmark_results)
