library(limma)

# Function to perform differential expression analysis using limma
differential_expression_limma <- function(tpm_matrix, sample_info, group_col) {
  
  # Validate input
  if (!is.matrix(tpm_matrix)) {
    stop("The tpm_matrix must be a matrix.")
  }
  if (!is.data.frame(sample_info)) {
    stop("The sample_info must be a data frame.")
  }
  if (!(group_col %in% colnames(sample_info))) {
    stop("The group_col must be a column in the sample_info data frame.")
  }
  
  # Ensure the group column is a factor
  sample_info[[group_col]] <- factor(sample_info[[group_col]])
  
  # Create design matrix
  design <- model.matrix(~ 0 + sample_info[[group_col]])
  colnames(design) <- levels(sample_info[[group_col]])
  
  v <- voom(tpm_matrix, design)

  # Fit the linear model
  fit <- lmFit(v, design)
  
  # Create contrast matrix for group comparisons
  group_levels <- levels(sample_info[[group_col]])
  if (length(group_levels) != 2) {
    stop("This function currently supports only two-group comparisons.")
  }
  contrast <- makeContrasts(
    Contrast = paste(group_levels[2], "-", group_levels[1], sep = ""),
    levels = design
  )
  
  # Apply contrasts to the model
  fit2 <- contrasts.fit(fit, contrast)
  
  # Perform empirical Bayes moderation
  fit2 <- eBayes(fit2)
  
  # Extract results
  results <- topTable(fit2, coef = 1, number = Inf)
  
  # Add gene names if rownames are available
  if (!is.null(rownames(results))) {
    results$gene <- rownames(results)
  }
  
  # Return the results
  return(results)
}