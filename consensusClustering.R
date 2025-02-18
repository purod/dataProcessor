# Load necessary packages
library(ConsensusClusterPlus, lib.loc="/home/qdu/R/x86_64-conda-linux-gnu-library/4.3")
library(pheatmap)
library(RColorBrewer)
library(cluster)  # For silhouette calculation and plotting
source("/home/qdu/git/dataProcessor/scaleNegPos1.R")

# Function to perform consensus clustering and save heatmaps as PDF
consensus_clustering <- function(data, maxK = 6, bestK=5, reps = 100, 
                                 clusterAlg = "hc") {
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

  data_t_winsorize <- t(data[,sample_order]) %>% as.data.frame %>% 
    mutate( across( everything(), scaleNegPos1))

  pheatmap(data_t_winsorize,
           cluster_rows = FALSE,
           cluster_cols = TRUE,
           display_numbers = FALSE,
           show_colnames = FALSE,
           show_rownames = TRUE,
           treeheight_row=0,
           treeheight_col=0,
           fontsize_row = 3,
           color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
           main = "Original Data Matrix with Sample Assignments",
           annotation_row = cluster_annotation,
           annotation_colors = cluster_color
  )
  
  # Close the PDF device
  dev.off()

   # Initialize vectors to store scores
  distortion_scores <- numeric(maxK - 1)
  silhouette_scores <- numeric(maxK - 1)

  for(k in 2:maxK){
    
   # Get consensus matrix and cluster assignments for k clusters
    consensus_matrix <- results[[k]]$consensusMatrix
    cluster_assignments <- results[[k]]$consensusClass
    
   # Calculate distortion score as within-cluster sum of squares on original data
    within_cluster_sums <- 0
    for (cluster in unique(cluster_assignments)) {
      cluster_data <- data[, cluster_assignments == cluster]
      cluster_center <- rowMeans(cluster_data)
      within_cluster_sums <- within_cluster_sums + sum((cluster_data - cluster_center)^2)
    }
    distortion_scores[k - 1] <- within_cluster_sums / length(data)

    # Calculate silhouette score
    dist_matrix <- as.dist(1 - consensus_matrix)
    sil <- silhouette(cluster_assignments, dist_matrix)
    silhouette_scores[k - 1] <- mean(sil[, "sil_width"])
  }

  # Data frame for plotting
  plot_data <- data.frame(
    k = 2:maxK,
    distortion = distortion_scores,
    silhouette = silhouette_scores
  )
  
  pdf("elbow_silhouette.pdf", height=4, width=5)

   # Plot with dual y-axes
  elbow_silhouette <- ggplot(plot_data, aes(x = k)) +
    geom_line(aes(y = distortion, color = "Distortion Score"), size = 1) +
    geom_point(aes(y = distortion, color = "Distortion Score"), size = 3, shape = 21, fill = "steelblue") +
    geom_line(aes(y = silhouette * (max(distortion) / max(silhouette)), color = "Silhouette Score"), 
              linetype = "dashed", size = 1) +
    geom_point(aes(y = silhouette * (max(distortion) / max(silhouette)), color = "Silhouette Score"), 
               size = 3, shape = 21, fill = "darkgreen") +
    scale_y_continuous(
      name = "Distortion Score",
      sec.axis = sec_axis(~ . / (max(plot_data$distortion) / max(plot_data$silhouette)), name = "Silhouette Score")
    ) +
    scale_color_manual(values = c("Distortion Score" = "steelblue", "Silhouette Score" = "darkgreen")) +
    labs(title = "Distortion and Silhouette Scores", x = "Number of Clusters") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, size=16, face = "bold"),
      axis.title.y = element_text(size = 14, color = "steelblue"),
      axis.title.y.right = element_text(size = 14, color = "darkgreen"),
      axis.title.x = element_text(size = 14),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
    )
    print(elbow_silhouette)
  dev.off()

  return(cluster_annotation)
}
