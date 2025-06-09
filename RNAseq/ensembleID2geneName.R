# ID2geneName <- function(ensemble_ID, genome_version="38"){

#    # Set the appropriate host based on the genome version
#   if (genome_version == "37") {
#     # Use Ensembl GRCh37
#     host_url <- "https://grch37.ensembl.org"
#     message("Using genome version: GRCh37 (hg19)")
#   } else if (genome_version == "38") {
#     # Use Ensembl GRCh38 (current)
#     host_url <- "https://www.ensembl.org"
#     message("Using genome version: GRCh38 (hg38)")
#   } 
  
#   mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", host=host_url,
#                            path="/biomart/martservice", dataset="hsapiens_gene_ensembl")
#   G_list <- biomaRt::getBM(filters= "ensembl_gene_id", attributes=c("hgnc_symbol","ensembl_gene_id"),
#                            values=ensemble_ID, mart=mart)
  
#   G_noF = ensemble_ID[!ensemble_ID%in%G_list$ensembl_gene_id]
  
#   return(list(G_list=G_list,G_noF=G_noF))
# }

ID2geneName <- function(ensemble_ID, genome_version = "38", 
                        method = c("useMart", "useEnsembl")) {
  method <- match.arg(method)

  library(biomaRt)

  # Set genome version
  if (genome_version == "37") {
    message("Using genome version: GRCh37 (hg19)")
    if (method == "useMart") {
      host_url <- "https://grch37.ensembl.org"
    }
    grch <- 37
  } else {
    message("Using genome version: GRCh38 (hg38)")
    if (method == "useMart") {
      host_url <- "https://asia.ensembl.org" # Asia mirror: "https://asia.ensembl.org"  "https://www.ensembl.org"
    }
    grch <- 38
  }

  # Connect to Ensembl
  if (method == "useMart") {
    mart <- useMart(biomart = "ENSEMBL_MART_ENSEMBL",
                    dataset = "hsapiens_gene_ensembl",
                    host = host_url,
                    path = "/biomart/martservice")
  } else if (method == "useEnsembl") {
    if(grch==38){
      mart <- useEnsembl(biomart = "ensembl", 
                         dataset = "hsapiens_gene_ensembl")
    }else{
      mart <- useEnsembl(biomart = "ensembl", 
                         dataset = "hsapiens_gene_ensembl", 
                         GRCh = grch)
    }
  }

  # Retrieve gene names
  G_list <- getBM(filters = "ensembl_gene_id", 
                  attributes = c("hgnc_symbol", "ensembl_gene_id"),
                  values = ensemble_ID, 
                  mart = mart)

  # Remove blank or NA gene symbols
  G_list <- G_list[G_list$hgnc_symbol != "" & !is.na(G_list$hgnc_symbol), ]

  # Identify IDs not found
  G_noF <- setdiff(ensemble_ID, G_list$ensembl_gene_id)

  return(list(G_list = G_list, G_noF = G_noF))
}



