# %% load in libraries
library(plyr)
library(tidyverse)
library(visdat)
# color
library(RColorBrewer)


# %% functions I wrote
theme_viss <- theme(
  plot.title = element_text(size = 20),          # Title font size
  axis.title.x = element_text(size = 16),        # X-axis title font size
  axis.title.y = element_text(size = 16),        # Y-axis title font size
  axis.text.x = element_text(size = 12),         # X-axis text font size
  axis.text.y = element_text(size = 12),         # Y-axis text font size
  legend.title = element_text(size = 14),        # Legend title font size
  legend.text = element_text(size = 12),          # Legend text font size
  plot.margin = margin(t = 5, r = 150, b = 5, l = 5)
)

# %% load in raw data
1. the data was read in correctly
a. dimension, format, missing value, abnormal values[functions: checkFileqc]
b. rename columns and rows, modify values and units
2. conciseness
a. check the consistency between different variables
b. remove duplicated rows
# %% sample annotation and sample size
1. create annotation matrix: patient ID, sample ID, key indicators, and duplicate label
2. drop patients and data modalities that would not be used in any following analysis
a. timepoint
b. clinical outcome and key variable only if this dramatically speed processing efficiency
c. missingness of all data modalities with distribution of key variables[vis_miss]
# vis_miss( paired_modalities %>% column_to_rownames("case_barcode"), cluster=T ) + theme_viss
# %% RNAseq pipeline[ you can build a function set ]
1. Starting with a raw count array (genes are rows, columns are samples (or patients)
2. Report the total number of read counts for all samples, filter out genes whose expression dominates the expression of all genes[reported to be bias to the RNAseq data]
3. Perform normalization with DESeq2
3. Transform the normalized data using log2(1+p) or whatever transform you like (DESeq2 has the built-in function to do the variance stabilization)
5. Perform PCA to check if we can simply average the duplicate samples[correlation and bioplot]
5. Average sample duplicates
6. Using the raw count array, filter out low-expressed genes (>90% zero OR maxcount < 10 OR 95th quantile count < 10 AND > 50% zero) 
7. Perform PCA and clustering analysis(t-SNE/UMAP) on normalized array to identify potential batch effect. 
7. Using linear regression function(algorithm to be determined) to regress out batch effect while keep the signals from other important factors, Perform PCA and clustering analysis after batch effect removal. This leads to the third batch free array. 
7. Perform PCA to check the batch effect again, make graphs to make sure effects of other covariates were not removed.
8. Create gene annotation table with bioMart ( ensemble name, gene name, ensemble_gene name, biotype, disease related or not)
10. For both normalized and batch free array, keep genes in raw count array. 
11. Select genes based on the standard deviation and mean of the disease genes
12.	Other kinds of explorative analysis with RNAseq data to be added. (e.g. creating topology structure between protein coding genes, miRNA, and lncRNA)

# %% Genetic mutatation SNV&INdel pipeline
1. Start with a VCF file with the right format
2. Filter out multiallelic sites
3. Filter out variant quality (QUAL < 30), genotype alternate coverage (AD < 3), genotype 
   quality (GQ < 20). Next, all variants were annotated with ENSEMBL VEP.
   A filtered list of variants was compiled based on the variant filter (PASS), 
   the protein impact (HIGH and MODERATE), and a population frequency filter 
   (gnomAD: MAX_AF < 0.01) if no normal reference exists
4. Need both wide and long table for plotting and data processing purpose 