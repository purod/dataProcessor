# library(GSVA)
# library(ggplot2)
# library(reshape2)
# library(limma)

# perform_gsva <- function(gene_sample_matrix, gene_set, group_labels) {
#   # Validate inputs
#   if (!is.matrix(gene_sample_matrix)) {
#     stop("The input gene_sample_matrix must be a matrix.")
#   }
#   if (!is.vector(gene_set)) {
#     stop("The gene_set must be a vector of gene names.")
#   }
#   if (!all(gene_set %in% rownames(gene_sample_matrix))) {
#     missing_genes <- gene_set[!gene_set %in% rownames(gene_sample_matrix)]
#     stop(paste("The following genes in the gene_set are not in the sample_gene_matrix:", 
#                paste(missing_genes, collapse = ", ")))
#   }
#   if (length(group_labels) != ncol(gene_sample_matrix)) {
#     stop("The group_labels must be a vector with length equal to the number of rows in sample_gene_matrix.")
#   }
  
#   # Ensure group labels are a factor
#   group_labels <- factor(group_labels)
  
#   # Create a gene set list for GSVA
#   gene_set_list <- list(gene_set)
  
#   # Perform GSVA
#   gsva_scores <- gsva(as.matrix(gene_sample_matrix), gene_set_list, method = "ssgsea", verbose = FALSE)
  
#   # Create a data frame for analysis
#   gsva_data <- data.frame(
#     Sample = colnames(gene_sample_matrix),
#     GSVA_Score = as.vector(gsva_scores),
#     Group = group_labels
#   )
  
#   # Perform statistical comparison using limma
#   design <- model.matrix(~ Group, data = gsva_data)
#   fit <- lmFit(gsva_scores, design)
#   fit <- eBayes(fit)
#   results <- topTable(fit, coef = 2, number = Inf)
  
#   # Plot the GSVA scores
#   p <- ggplot(gsva_data, aes(x = Group, y = GSVA_Score, fill = Group)) +
#     geom_boxplot(outlier.shape = 16, outlier.size = 2, notch = TRUE) +
#     geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
#     labs(
#       title = "GSVA Score Comparison",
#       x = "Group",
#       y = "GSVA Score"
#     ) +
#     theme_minimal() +
#     scale_fill_brewer(palette = "Set2") +
#     theme(
#       text = element_text(size = 18),
#       plot.title = element_text(hjust = 0.5, face = "bold"),
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       legend.position = "top"
#     )
  
#   # Return results and the plot
#   list(
#     gsva_scores = gsva_scores,
#     results = results,
#     plot = p
#   )
# }


library(GSVA)
library(limma)
library(ggplot2)
library(reshape2)

perform_gsva_multiple <- function(gene_sample_matrix, gene_sets, group_labels) {
  # Validate inputs
  if (!is.matrix(gene_sample_matrix)) {
    stop("The input gene_sample_matrix must be a matrix.")
  }
  if (!is.list(gene_sets) || !all(sapply(gene_sets, is.vector))) {
    stop("The gene_sets must be a list of vectors of gene names.")
  }
  if (length(group_labels) != ncol(gene_sample_matrix)) {
    stop("The group_labels must be a vector with length equal to the number of columns in gene_sample_matrix.")
  }
  
  # Ensure group labels are a factor
  group_labels <- factor(group_labels)
  
  # Filter gene sets to include only genes present in the matrix
  filtered_gene_sets <- lapply(gene_sets, function(genes) {
    intersect(genes, rownames(gene_sample_matrix))
  })
  
  # Remove empty gene sets
  non_empty_sets <- sapply(filtered_gene_sets, length) > 0
  filtered_gene_sets <- filtered_gene_sets[non_empty_sets]
  
  if (any(!non_empty_sets)) {
    warning("Some gene sets had no matching genes in the gene expression matrix and were removed.")
  }
  
  if (length(filtered_gene_sets) == 0) {
    stop("No gene sets with matching genes found in the dataset.")
  }
  
  # Perform GSVA
  gsvapar <- gsvaParam(exprData=as.matrix(gene_sample_matrix), 
                       geneSets=filtered_gene_sets)
  gsva_scores <- gsva(gsvapar)
  
  # Convert GSVA scores into a long-format data frame for plotting
  gsva_data <- melt(gsva_scores, varnames = c("GeneSet", "Sample"), value.name = "GSVA_Score")
  gsva_data$Group <- rep(group_labels, each = nrow(gsva_scores))
  
  # Plot the GSVA scores
  p <- ggplot(gsva_data, aes(x = GeneSet, y = GSVA_Score, fill = Group)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 2, notch = FALSE) +
    geom_jitter(aes(color = Group), size = 2, alpha = 0.8, color = "black",shape=21,
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)) + 
    labs(
      title = "GSVA Score Comparison Across Gene Sets",
      x = "Gene Set",
      y = "GSVA Score"
    ) +
    theme_minimal() + # "#4c72b0", 
    scale_fill_manual(values = c("#4c72b0", "#c49c94", "#D0CECE", "#C8ACE7")[1:length(filtered_gene_sets)]) + 
    scale_color_manual(values = c("#4c72b0", "#c49c94","#D0CECE", "#C8ACE7")[1:length(filtered_gene_sets)]) +
    theme(
      text = element_text(size = 18),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Return results and the plot
  list(
    gsva_scores = gsva_scores,
    plot = p
  )
}