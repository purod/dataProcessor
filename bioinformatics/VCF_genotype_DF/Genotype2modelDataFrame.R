genotype2modelDf <- function(geno_file, varType="binary",
                            sampleMiss_cut=10, imbalance_cut=10){
  
  modelDf <- data.table::fread(geno_file) %>% # read in genotype matrix
    as.data.frame(stringsAsFactors = F) %>%
    select(GENE, 18:ncol(.)) %>%
    # drop individual with too much missing
    filter( rowSums(is.na(.[,2:ncol(.)])) < sampleMiss_cut ) %>% 
    # convert missing values to 0s
    mutate_at( 2:ncol(.), coalesce, 0) %>% 
    # sum up SNPs by genes
    group_by(GENE) %>% summarise_all(sum) %>% ungroup %>% as.data.frame()
    
  if( varType=="binary" ){
    modelDf <- modelDf %>% mutate_at( 2:ncol(.), ~ifelse(.>0,1,0))
  }
  
  modelDf <- modelDf %>% 
    # format
    t() %>% as.data.frame(stringsAsFactors = F) %>% setNames(.[1,] %>% unname) %>% 
    slice(c(-1)) %>% mutate_all(as.numeric) %>% 
    # drop imbalanced variables
    select_if( varKept(.,imbalance_cut) ) %>% rownames_to_column(var="SUBJID") %>%
    # update gene names
    setNames(gsub("\\\\x3","_",gsub("-","_",colnames(.)))) %>%
    select(SUBJID, everything())
  
  return(modelDf)
}
