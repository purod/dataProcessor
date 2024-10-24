variableFeatureSelect <- function(rna_data, top_percentage = NULL, 
                                  meanSd_percentage = NULL, meanSd_cut = NULL, 
                                  meanSd_prior_percentage=0.05, prior_genes,
                                  plot = TRUE) {
  # Ensure input is a matrix or data frame
  if (!is.matrix(rna_data) && !is.data.frame(rna_data)) {
    stop("Input must be a matrix or data frame with samples as rows and genes as columns.")
  }
  
  # Remove all constant variables
  rna_data <- rna_data[, apply(rna_data, 2, function(x){length(unique(x))})>1]

  # 1. Calculate the mean and variance of each gene (i.e., column-wise)
  gene_means <- colMeans(rna_data)
  gene_variances <- apply(rna_data, 2, var)
  gene_sd <- apply(rna_data, 2, sd)

  # Strategy 1: use 5% of cancer genes as cutoffs for all genes
  if(!is.null(meanSd_prior_percentage)){
    consensus_genes <- rna_data[,colnames(rna_data)%in%CosmicGenes$hgnc_symbol] 
    mean_cutoff <- apply( consensus_genes, 2, mean ) %>% unlist %>% quantile(meanSd_prior_percentage)
    sd_cutoff <- apply( consensus_genes, 2, sd ) %>% unlist %>% quantile(meanSd_prior_percentage)
  }
  # Strategy 2: use percentile of all genes as cutoffs for all genes
  if(!is.null(meanSd_percentage)){
    mean_cutoff <- quantile(gene_means, meanSd_percentage)
    sd_cutoff <- quantile(gene_sd, meanSd_percentage)
  }
  # Strategy 3: use provided cutoffs
  if(!is.null(meanSd_cut)) {
    mean_cutoff <- meanSd_cut[1]
    sd_cutoff <- meanSd_cut[2]
  }














  # 2. Calculate standardized variance (variance / mean)
  standardized_variance <- gene_variances / gene_means
    
  # 3. Create a data frame for easy handling
  variance_data <- data.frame(
    Gene = colnames(rna_data),
    Mean = gene_means,
    Standard_deviation=gene_sd,
    Variance = gene_variances,
    StandardizedVariance = standardized_variance
  )
  
  if(!is.null(top_percentage)){
    # 4. Sort by standardized variance to get the top variable genes
    top_genes <- variance_data[order(-variance_data$StandardizedVariance, na.last = NA), ]
  
    # Select the top percentage of most variable genes
    selected_genes <- top_genes[1:round(top_percentage * nrow(top_genes)), ]
  }else{
    # filter by mean and standard deviation. Keep all prior genes as well
    selected_genes <- variance_data %>% 
      mutate( Mean_filter=Mean>mean_cutoff ) %>% 
      mutate( Sd_filter=Standard_deviation>sd_cutoff ) %>% 
      mutate( prior_filter=Gene%in%prior_genes) %>% 
      filter( (Mean_filter&Sd_filter)|prior_filter ) 
  }

  # Create a column to identify whether a gene is in the selected list
  variance_data$IsSelected <- ifelse(variance_data$Gene %in% selected_genes$Gene, "Selected", "Not Selected")
  
  # 5. Plot Mean vs Standardized Variance if requested
  if (plot) {
    if(!is.null(top_percentage)){
      library(ggplot2)
      g <- ggplot(variance_data, aes(x = Mean, y = StandardizedVariance)) +
        geom_point(aes(color = IsSelected), alpha = 0.5) +
        scale_color_manual(values = c("Selected" = "red", "Not Selected" = "black")) +
        labs(
          x = "Mean Expression",
          y = "Standardized Variance",
          title = "Standardized Variance vs Mean Expression"
        ) 
    } else{

      g <- ggplot(data = variance_data, aes(x = Mean, y = Standard_deviation)) +
        geom_bin2d(bins=100) + theme_bw(base_size = 18)+
        geom_vline(aes(xintercept = mean_cutoff), colour = "red", linewidth = 2, linetype = "dashed" ) +
        geom_hline(aes(yintercept = sd_cutoff), colour = "red", linewidth = 2, linetype = "dashed") +
        xlab("mean Expression") + ylab("sd Expression") + 
        theme(
         axis.text = element_text(size=20),
         axis.title = element_text(size = 24),
         plot.title = element_text(size = 20, hjust = 0.5),
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 16))
    }
    # Return the selected genes
    return(list(selected_genes=selected_genes, g=g))
  }
}

# Example of usage:
# selected_genes <- variableFeatureSelect(rna_data, top_percentage = 0.1, plot = TRUE)


