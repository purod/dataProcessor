library(limma)

# Define the function
limmaDEG <- function(expression_data, cluster_assignments, 
                      alpha = 0.05, logFC_threshold = 1, direction="both") {
  
  # Convert cluster assignments to a factor if necessary
  cluster_assignments <- as.factor(cluster_assignments)
  
  # Set up the design matrix
  design <- model.matrix(~0 + cluster_assignments)
  cluster_names <- levels(cluster_assignments)
  colnames(design) <- cluster_names
  
  # Fit the linear model
  fit <- lmFit(expression_data, design)
  
   # Create contrasts for each cluster vs. the average of the others
  contrast_list <- list()
  for (i in 1:length(cluster_names)) {
    # Define the current cluster and other clusters
    current_cluster <- cluster_names[i]
    other_clusters <- cluster_names[-i]
    
    # Create the contrast string
    contrast <- paste(current_cluster, "-", paste0("(", paste(other_clusters, collapse = " + "), ") /", length(other_clusters)), sep = "")
    
    # Add to the contrast list
    contrast_list[[paste0(current_cluster, "_vs_Others")]] <- contrast
  }
  
  # Build the contrast matrix
  contrast_matrix <- makeContrasts(contrasts = unlist(contrast_list), levels = design)
  colnames(contrast_matrix) <- names(contrast_list)
  
  # Apply contrasts to the model fit
  fit2 <- contrasts.fit(fit, contrast_matrix)
  
  # Compute moderated t-statistics, log-fold changes, and p-values
  fit2 <- eBayes(fit2)
  
  # volcano plot for all categories: to be added

  # Extract significant genes for each cluster
  results <- list()
  for (i in 1:ncol(contrast_matrix)) {
    contrast_name <- colnames(contrast_matrix)[i]
    results[[contrast_name]] <- topTable(fit2, coef = contrast_name, 
              p.value = alpha, lfc = logFC_threshold, number = Inf) %>% 
              rownames_to_column("VariableName")
    if(direction=="positive"){
      results[[contrast_name]] <- results[[contrast_name]] %>% 
        filter(logFC>0)
    }
     if(direction=="negative"){
      results[[contrast_name]] <- results[[contrast_name]] %>% 
        filter(logFC<0)
    }
  }
  
  return(results)
}
