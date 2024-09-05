geneName2ID <- function(hgnc_symbol){
  
  hgnc_symbol = gsub("\\.","-",hgnc_symbol)
  
  mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", host="www.ensembl.org",
                           path="/biomart/martservice", dataset="hsapiens_gene_ensembl")
  G_list <- biomaRt::getBM(filters= "hgnc_symbol", attributes=c("hgnc_symbol","ensembl_gene_id"),
                          values=hgnc_symbol, mart=mart)
  
  G_noF = hgnc_symbol[!hgnc_symbol%in%G_list$hgnc_symbol]
  
  return(list(G_list=G_list,G_noF=G_noF))
}

ID2geneName <- function(ensemble_ID){
  
  mart <- biomaRt::useMart(biomart="ENSEMBL_MART_ENSEMBL", host="www.ensembl.org",
                           path="/biomart/martservice", dataset="hsapiens_gene_ensembl")
  G_list <- biomaRt::getBM(filters= "ensembl_gene_id", attributes=c("hgnc_symbol","ensembl_gene_id"),
                           values=ensemble_ID, mart=mart)
  
  G_noF = ensemble_ID[!ensemble_ID%in%G_list$ensembl_gene_id]
  
  return(list(G_list=G_list,G_noF=G_noF))
}
