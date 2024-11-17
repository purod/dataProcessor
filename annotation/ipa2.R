getPath = function(df_path, vars_of_interest){
  df_pathways = df_path %>% 
    dplyr::select(path_NODE_NAME, path_PATHWAY_ID, path_PATHWAY_NAME, path_HUMAN_ENTREZGENE_ID) %>%
    filter(path_HUMAN_ENTREZGENE_ID %in% vars_of_interest) %>%
    distinct()
  
  vars_no_path = vars_of_interest[!vars_of_interest %in% df_pathways$path_HUMAN_ENTREZGENE_ID]
  
  vars_path = vars_of_interest[vars_of_interest %in% df_pathways$path_HUMAN_ENTREZGENE_ID]
  
  return(list(df_pathways = df_pathways, vars_no_path = vars_no_path, vars_path = vars_path))
}


getHighRepPaths = function(df_path,
                           vars_of_interest,cutoff=0.8){
  
  pathsInfo = getPath(df_path, vars_of_interest) #Get IPA results for variables of interest
  
  df_pathways = pathsInfo$df_pathways
  
  if(nrow(df_pathways)  <= 1){
    res = data.frame(matrix(nrow = 0, ncol =5))
    colnames(res) = c("Pathway", "Count_Genes", "qval", "OR", "Genes")
    return(list(enrichmentSummary = res))}
  
  Ndriver = length(unique(df_pathways$path_NODE_NAME)) 
  
  Ntotal = length(unique(df_path$path_NODE_NAME))
  
  counts = table(df_pathways$path_PATHWAY_NAME)
  
  highreppathways = counts[counts >= round(quantile(counts, prob = cutoff))] %>% data.frame
  
  pvals = c()
  
  ORs = c()
  
  vars = c()
  
  Ndriverpath_vec = c()
  
  Nnodriverpath_vec = c()
  
  Ndrivernopath_vec = c()
  
  Nnodrivernopath_vec = c()

  vars_in_path = c()
  
  #For each pathway picked up, calculate enrichment using contingency table
  for (row in 1:nrow(highreppathways)){
    path = as.character(highreppathways$Var1[row])
    
    dfpath = df_path %>% filter(path_PATHWAY_NAME == path) %>% 
      dplyr::select(path_NODE_NAME, path_PATHWAY_ID, path_PATHWAY_NAME, path_HUMAN_ENTREZGENE_ID) %>% 
      distinct
    
    Npath = length(unique(dfpath$path_NODE_NAME))
    
    driverpath = dfpath %>% filter(path_HUMAN_ENTREZGENE_ID %in% vars_of_interest) %>% 
      pull(path_NODE_NAME)
    
    vars_in_path[[path]] = list(driverpath)
    
    Ndriverpath = length(unique(driverpath))
    
    Ndrivernopath = Ndriver - Ndriverpath
    
    Nnodriverpath = Npath - Ndriverpath
    
    Nnodrivernopath = Ntotal - Nnodriverpath - Ndrivernopath - Ndriverpath
    
    Ndriverpath_vec = c(Ndriverpath_vec, Ndriverpath)
    
    Nnodriverpath_vec = c(Nnodriverpath_vec, Nnodriverpath)
    
    Ndrivernopath_vec = c(Ndrivernopath_vec, Ndrivernopath)
    
    Nnodrivernopath_vec = c(Nnodrivernopath_vec, Nnodrivernopath)
    
    fish.df = data.frame(
      driver = c(Ndriverpath, Ndrivernopath),
      notdriver = c(Nnodriverpath, Nnodrivernopath))
    rownames(fish.df) = c("path", "nopath")
    
    fish.res = fisher.test(fish.df, alternative = "g")
    
    vars = c(vars, list(path = paste(driverpath, collapse = ",")))
    
    pvals = c(pvals, fish.res$p.value)
    
    ORs = c(ORs, fish.res$estimate)
  }
  
  highreppathways = highreppathways %>% 
    dplyr::rename("Pathway" = "Var1", "Count_Genes" = "Freq") %>% 
    mutate(pvals = signif(pvals, 2), 
           qval = signif(p.adjust(pvals, "fdr"),2), 
           OR = ORs,
           N_driver_path = Ndriverpath_vec,
           N_driver_notpath = Ndrivernopath_vec,
           N_notdriver_path = Nnodriverpath_vec,
           N_notdriver_notpath = Nnodrivernopath_vec,
           Genes = vars) %>% 
    mutate(Genes = unlist(Genes)) %>% 
    arrange(qval)

  
  return(list(enrichmentSummary = highreppathways, vars_in_paths = vars_in_path))
}

# #IPA enrichment for BCL2 family MM
# ipa2 = function(var){
#   library(GNSRE2)
#   library(REFSfs)
#   
#   #Load in IPA
#   directory = '/projects/IPA/FlatFiles/'
#   fileName = paste0(directory,'Ingenuity_PathwayNodes_09.15.2020.txt.gz')
#   df_path = fread( fileName,  sep='\t', fill=T, header=T, 
#                    skip=1, data.table = F)
#   names(df_path) = gsub(" ","_", names(df_path))
#   colnames(df_path) = paste("path", colnames(df_path), sep = "_")
#   
#   #Select drivers & targets from all2all simulations
#   alltoall <- fread("/home/qdu/Servier/data/raw/allvall_bcl2family_A19.csv", data.table = F)
#   
#   target_model_name = alltoall %>%
#     filter( tail_probability < quantile(alltoall$tail_probability, 0.01) & #filter based on top 1% CD & TP
#               abs(Cohens_D) > quantile(alltoall$Cohens_D, 0.99) & #filter based on top 1% CD & TP
#               input == var & grepl("GENE", output)) %>% #select only gene targets
#     pull(output) %>% as.character()
#   target = gsub("_continuous", "", target_model_name) %>% gsub(".*_", "", .) #extract gene name for IPA analysis
#   
#   driver_model_name = alltoall %>%
#     filter( tail_probability < quantile(alltoall$tail_probability, 0.01) & #filter based on top 1% CD & TP
#               abs(Cohens_D) > quantile(abs(alltoall$Cohens_D), 0.99) & #filter based on top 1% CD & TP
#               output == var & grepl("GENE", input)) %>% #select only gene targets
#     pull(input) %>% as.character()
#   driver = gsub("_continuous", "", driver_model_name) %>%  gsub(".*_", "", .) #extract gene name for IPA analysis
#   
#   #Get high represented pathway results for drivers & targets
#   #Get genes that are picked up by any pathway & genes not picked up by any pathway
#   res = list(Driver = getHighRepPaths(df_path, driver), Target = getHighRepPaths(df_path, target),
#              varsNoPathDrivers = getPath(df_path, driver)$vars_no_path,
#              varsPathDrivers = getPath(df_path, driver)$vars_path,
#              varsNoPathTargets = getPath(df_path, target)$vars_no_path,
#              varsPathTargets = getPath(df_path, target)$vars_path)
#   
#   return(res)
# }
