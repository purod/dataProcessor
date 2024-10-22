geneName2ID <- function(hgnc_symbol, genome_version="38"){
  
  hgnc_symbol = gsub("\\.","-",hgnc_symbol)

  # Set the appropriate host based on the genome version
  if (genome_version == "37") {
    # Use Ensembl GRCh37
    host_url <- "https://grch37.ensembl.org"
    message("Using genome version: GRCh37 (hg19)")
  } else if (genome_version == "38") {
    # Use Ensembl GRCh38 (current)
    host_url <- "https://www.ensembl.org"
    message("Using genome version: GRCh38 (hg38)")
  } 

  mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", host=host_url,
                           path="/biomart/martservice", dataset="hsapiens_gene_ensembl")
  G_list <- biomaRt::getBM(filters= "hgnc_symbol", attributes=c("hgnc_symbol","ensembl_gene_id"),
                          values=hgnc_symbol, mart=mart)
  
  G_noF = hgnc_symbol[!hgnc_symbol%in%G_list$hgnc_symbol]
  
  return(list(G_list=G_list,G_noF=G_noF))
}

ID2geneName <- function(ensemble_ID, genome_version="38"){

   # Set the appropriate host based on the genome version
  if (genome_version == "37") {
    # Use Ensembl GRCh37
    host_url <- "https://grch37.ensembl.org"
    message("Using genome version: GRCh37 (hg19)")
  } else if (genome_version == "38") {
    # Use Ensembl GRCh38 (current)
    host_url <- "https://www.ensembl.org"
    message("Using genome version: GRCh38 (hg38)")
  } 
  
  mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", host=host_url,
                           path="/biomart/martservice", dataset="hsapiens_gene_ensembl")
  G_list <- biomaRt::getBM(filters= "ensembl_gene_id", attributes=c("hgnc_symbol","ensembl_gene_id"),
                           values=ensemble_ID, mart=mart)
  
  G_noF = ensemble_ID[!ensemble_ID%in%G_list$ensembl_gene_id]
  
  return(list(G_list=G_list,G_noF=G_noF))
}
