library(DESeq2)
#library(apeglm)

# Function to perform DESeq2 differential expression analysis for paired samples
differential_expression_paired <- function(count_matrix, sample_info, group_col, pair_col=NULL, FC_method="normal") {
  
  # Check if DESeq2 is installed
  if (!requireNamespace("DESeq2", quietly = TRUE)) {
    stop("Please install the DESeq2 package.")
  }
  
  # Validate input
  if (!is.matrix(count_matrix)) {
    stop("The count_matrix must be a matrix.")
  }
  if (!is.data.frame(sample_info)) {
    stop("The sample_info must be a data frame.")
  }
  if (!(group_col %in% colnames(sample_info))) {
    stop("The group_col must be a column in the sample_info data frame.")
  }
  # Create DESeq2 dataset
  if(!is.null(pair_col)){
    # compare by pairs
    dds <- DESeqDataSetFromMatrix(
        countData = count_matrix,
        colData = sample_info,
        design = as.formula(paste("~", pair_col, "+", group_col))
    )
  }else{
    # without pair
    dds <- DESeqDataSetFromMatrix(
        countData = count_matrix,
        colData = sample_info,
        design = as.formula(paste("~", group_col))
    )
  }
  
  # Pre-filtering: keep only rows with a minimum number of reads
  dds <- dds[rowSums(counts(dds)) > 10, ]
  
  # Run DESeq2 analysis
  dds <- DESeq(dds)
  
  # Get results
  res <- results(dds)
  
  # Perform log fold-change shrinkage for visualization and ranking
  resLFC <- lfcShrink(dds, coef = 2, type = FC_method)
  
  # Return the results
  list(
    raw_results = res,
    lfc_shrink_results = resLFC
  )
}


DEseqCompare <- function(count_matrix, sample_info, group_col, 
                         pair_col = NULL, covariate_col = NULL, 
                         test_interaction = FALSE, FC_method="normal") {
  
  # Check if DESeq2 is installed
  if (!requireNamespace("DESeq2", quietly = TRUE)) {
    stop("Please install the DESeq2 package.")
  }
  
  # Validate input
  if (!is.matrix(count_matrix)) {
    stop("The count_matrix must be a matrix.")
  }
  if (!is.data.frame(sample_info)) {
    stop("The sample_info must be a data frame.")
  }
  if (!(group_col %in% colnames(sample_info))) {
    stop("The group_col must be a column in the sample_info data frame.")
  }
  if (!is.null(covariate_col) && !(covariate_col %in% colnames(sample_info))) {
    stop("The covariate_col must be a column in the sample_info data frame.")
  }
  if (!is.null(pair_col) && !(pair_col %in% colnames(sample_info))) {
    stop("The pair_col must be a column in the sample_info data frame.")
  }
  
  # Construct design formula
  if (!is.null(pair_col)) {
    # Paired design
    if (!is.null(covariate_col)) {
      if (test_interaction) {
        design_formula <- as.formula(paste("~", pair_col, "+", covariate_col, "+", group_col, "+", covariate_col, ":", group_col))
      } else {
        design_formula <- as.formula(paste("~", pair_col, "+", covariate_col, "+", group_col))
      }
    } else {
      design_formula <- as.formula(paste("~", pair_col, "+", group_col))
    }
  } else {
    # Unpaired design
    if (!is.null(covariate_col)) {
      if (test_interaction) {
        design_formula <- as.formula(paste("~", covariate_col, "+", group_col, "+", covariate_col, ":", group_col))
      } else {
        design_formula <- as.formula(paste("~", covariate_col, "+", group_col))
      }
    } else {
      design_formula <- as.formula(paste("~", group_col))
    }
  }
  
  # Create DESeq2 dataset
  dds <- DESeqDataSetFromMatrix(
    countData = count_matrix,
    colData = sample_info,
    design = design_formula
  )
  
  # Pre-filtering: keep only rows with a minimum number of reads
  dds <- dds[rowSums(counts(dds)) > 10, ]
  
  # Run DESeq2 analysis
  dds <- DESeq(dds)
  
  # Get results
  if (test_interaction) {
    res <- results(dds, name = paste0(covariate_col, ".", group_col))
  } else {
    res <- results(dds)
  }
  
  # Perform log fold-change shrinkage for visualization and ranking
  resLFC <- lfcShrink(dds, coef = 2, type = FC_method)
  
  # Return the results
  list(
    raw_results = res,
    lfc_shrink_results = resLFC
  )
}
