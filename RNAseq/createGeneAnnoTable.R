

createGeneAnnoTable <- function(gene_ids, genome_version = "38") {
  # Load required library
  library(biomaRt)
  library(dplyr)

  # Set the appropriate host based on the genome version
  if (genome_version == "37") {
    host_url <- "https://grch37.ensembl.org"
    message("Using genome version: GRCh37 (hg19)")
  } else if (genome_version == "38") {
    host_url <- "https://useast.ensembl.org" # https://www.ensembl.org
    message("Using genome version: GRCh38 (hg38)")
  }

  # Set up biomaRt with the selected Ensembl dataset
  mart <- biomaRt::useMart(biomart = "ENSEMBL_MART_ENSEMBL", host = host_url,
                           path = "/biomart/martservice", dataset = "hsapiens_gene_ensembl")

  # Determine filter type based on input IDs
  if (all(grepl("^ENSG", gene_ids))) {
    filter_type <- "ensembl_gene_id"
  } else {
    filter_type <- "hgnc_symbol"
  }

  # Define attributes to retrieve
  attributes <- c("hgnc_symbol", "ensembl_gene_id", "entrezgene_id", "gene_biotype")
  
  # Retrieve gene annotation information
  gene_annotations <- biomaRt::getBM(filters = filter_type, attributes = attributes,
                                     values = gene_ids, mart = mart)

  # Find any input IDs that were not matched
  unmatched_ids <- gene_ids[!gene_ids %in% gene_annotations[[filter_type]]]
  message( paste("There are", length(unmatched_ids),"unmatched rows."))


  # Format the output table
  gene_annotations <- gene_annotations %>%
    dplyr::rename(Entrez_ID = entrezgene_id,
                  Ensembl_ID = ensembl_gene_id,
                  HGNC_Symbol = hgnc_symbol,
                  Gene_Biotype = gene_biotype) 
  message( paste("There are",nrow(gene_annotations),"rows."))

  gene_remove_dupEntrez <- gene_annotations %>% 
    mutate( Entrez_ID=ifelse( is.na(Entrez_ID), 0, Entrez_ID ) ) %>% 
    mutate( Ensembl_HGNC=paste0(Ensembl_ID,"_",HGNC_Symbol)) %>% 
    arrange( Entrez_ID ) %>% filter( !(duplicated(Ensembl_HGNC) & Entrez_ID > 0) )
  message( paste("Remove",nrow(gene_annotations)-nrow(gene_remove_dupEntrez),"EntrezID, keeping the one with smaller number."))

  Anno_table <- gene_remove_dupEntrez %>% 
    filter( !duplicated(Ensembl_ID)) %>% 
    mutate( Entrez_ID=ifelse( Entrez_ID==0, NA, Entrez_ID ) ) %>% 
    mutate( HGNC_Symbol=ifelse( HGNC_Symbol=="", NA, HGNC_Symbol ) ) %>% 
    mutate( HGNC_dot = gsub("-",".", HGNC_Symbol) ) %>% 
    dplyr::select(-Ensembl_HGNC)  %>%  
    mutate( Ensembl_HGNC_dot = ifelse( is.na(HGNC_dot), Ensembl_ID,
                                            paste(Ensembl_ID, HGNC_dot, sep="_"))
          ) %>%    
    mutate( temp = ifelse( is.na(HGNC_dot), paste("Exp", Ensembl_ID, sep="_"),
                                            paste("Exp", Ensembl_ID, HGNC_dot, sep="_"))
          )       
  message( paste("Remove", nrow(gene_remove_dupEntrez)-nrow(Anno_table),"duplicated ENSG." ))
  
  # Return the results as a list with the matched annotations and unmatched IDs
  return(list(Anno_table=Anno_table, Unmatched_IDs = unmatched_ids))
}

# Example usage
# gene_list <- c("TP53", "BRCA1", "ENSG00000139618")  # Mixed Ensembl IDs and HGNC symbols
# result <- createGeneAnnoTable(gene_list, genome_version = "38")
# View(result$Gene_Annotations)  # To view matched gene annotations
# result$Unmatched_IDs          # To view any unmatched IDs

