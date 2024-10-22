# %% load in libraries
source("/home/qdu/git/dataProcessor/RNAseq/geneNameEnsembleIDConversion.R")
library(tidyverse)

# %% TEAD genes for Orion project Source: Orion PPT

# TEAD signature measuring the inhibitor efficacy
TEAD_signature_raw <- c("CCN2", "CCN1", "F3", "AXL","ANKRD1", "NT5E", "IGFBP3")
# CCN2 is CTGF; CCN1 is CYR61

# Upstream Hippo genes controlling TEAD gene
TEAD_Hippo_raw <- c("NF2", "WWC1", "TAOK1", "TAOK2", "TAOK3", "FRMD6", 
                    "SAV1", "STK3", "STK4", "MOB1A", "MOB1B","LATS1", "LATS2", 
                    "YAP1","TAFAZZIN", "VGLL4", "TEAD1","TEAD2","TEAD3","TEAD4")

# GeneName + Ensemble ID
TEAD_signature = geneName2ID( TEAD_signature_raw )$G_list
TEAD_Hippo = geneName2ID( TEAD_Hippo_raw )$G_list

# task 1: 
library("xls")
data <- readxl::read_excel("/projects/oncology/clients/orion/GNS_TEAD/mmc6.xlsx", 
                   sheet = "Table S6D", skip=4)

# %% cancer gene list
Cosmic = data.table::fread("/projects/oncology/databases/knownCancerGenes/COSMIC/Census_allTue Apr  9 03_10_07 2024.csv") %>% 
  pull(`Gene Symbol`) %>% unique() # 743 genes
Cosmic_genes <- Cosmic %>% geneName2ID(genome_version="37") 
NCG = data.table::fread("/projects/oncology/databases/knownCancerGenes/NCG/NCG_cancerdrivers_annotation_supporting_evidence.tsv") %>% 
  pull(symbol) %>%  unique() # 3,347 genes
NCG_genes <- NCG %>% geneName2ID(genome_version="37")
combined_genes <- unique(c(Cosmic, NCG)) %>% geneName2ID(genome_version="37") # 3842 genes
CosmicGenes = Cosmic_genes$G_list
NCGGenes = NCG_genes$G_list
combinedGenes = combined_genes$G_list
save( CosmicGenes, NCGGenes, combinedGenes,
     file="/home/qdu/git/dataProcessor/GeneList/Cosmic_NCG_cancer_genes_V37.RData")

