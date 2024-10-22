# Load necessary packages
library(ConsensusClusterPlus)
library(pheatmap)
library(RColorBrewer)

# Function to perform consensus clustering and save heatmaps as PDF
consensus_clustering <- function(data, maxK = 6, bestK=5, reps = 100, 
                                 clusterAlg = "hc", distance = "binary") {
  # Ensure data is in matrix format
  data <- as.matrix(data)
  
  # Perform consensus clustering
  results <- ConsensusClusterPlus(
    d = data,
    maxK = maxK,  # Maximum number of clusters to try
    reps = reps,  # Number of resampling iterations
    clusterAlg = clusterAlg,  # Clustering algorithm (e.g., "hc" for hierarchical)
    plot = 'png',  # Do not generate internal plots
    writeTable = FALSE,  # Do not write the results to disk
    seed=1917
  )
  
  # Extract consensus matrix and cluster assignments
  consensus_matrix <- results[[bestK]]$consensusMatrix
  rownames(consensus_matrix) <- colnames(data)
  colnames(consensus_matrix) <- colnames(data)
  sample_order <- results[[bestK]]$consensusTree$order

  # Create cluster annotation
  cluster_annotation <- data.frame(Cluster=results[[bestK]]$consensusClass)
  rownames(cluster_annotation)  <- colnames(data)

  # Create a color palette for clusters
  cluster_ID <- unique(cluster_annotation$Cluster)
  cluster_colors <- brewer.pal(length(cluster_ID),name="Set1")
  cluster_color <- list(Cluster = setNames(cluster_colors, cluster_ID))

  # Open PDF device to save the heatmaps
  pdf("consensus_matrix.pdf")  # Set the desired width and height for the PDF
  
  # Create a heatmap of the consensus matrix with sample assignments as annotation
  pheatmap(consensus_matrix[sample_order, sample_order],
           cluster_rows = FALSE,
           cluster_cols = FALSE,
           display_numbers = FALSE,
           show_colnames = FALSE,
           show_rownames = FALSE,
           main = paste("Consensus Matrix (K =", bestK, ")"),
           #color = colorRampPalette(c("blue", "white", "red"))(50)#,
           annotation_col = cluster_annotation,
           annotation_colors = cluster_color
  )
  dev.off()
  
  pdf("consensus_data.pdf", width=30, height=15)
  # Create a heatmap of the original data matrix with sample assignments
  pheatmap(t(data[,sample_order]),
           cluster_rows = FALSE,
           cluster_cols = TRUE,
           display_numbers = FALSE,
           show_colnames = FALSE,
           show_rownames = TRUE,
           treeheight_row=0,
           treeheight_col=0,
           fontsize_row = 3,
           main = "Original Data Matrix with Sample Assignments",
           annotation_row = cluster_annotation,
           annotation_colors = cluster_color
  )
  
  # Close the PDF device
  dev.off()
  
  return(cluster_annotation)
}
