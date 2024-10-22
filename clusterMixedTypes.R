# Function to perform clustering on mixed data
library(cluster)
library(ggplot2)
library(dplyr)
library(factoextra)
library(pheatmap)

cluster_mixed_data <- function(data, max_clusters = 10, optimal_k=0) {
  # Convert discrete variables into characters
  data_factor <- data %>% mutate_if(~ !is.numeric(.), as.factor)
  data_numeric <- data %>% mutate_if(~ !is.numeric(.), as.numeric)

  # Compute Gower distance matrix
  gower_dist <- daisy(data_factor, metric = "gower")
  
  # Generate PAM clustering for a range of clusters and calculate silhouette width
  silhouette_avg <- numeric(max_clusters - 1)
  
  for (k in 2:max_clusters) {
    pam_fit <- pam(gower_dist, k = k, diss = TRUE)
    silhouette_avg[k - 1] <- pam_fit$silinfo$avg.width
  }
  
  # Plot Silhouette width to determine optimal number of clusters
  pdf("silhouette_method.pdf")  # Save silhouette plot to PDF
  plot(2:max_clusters, silhouette_avg, type = "b", pch = 19,
       frame = FALSE, xlab = "Number of Clusters",
       ylab = "Average Silhouette Width",
       main = "Silhouette Method for Optimal Clusters")
  dev.off()  # Close the PDF device
  
  # Use Elbow method to visualize within-cluster sum of squares
  wss <- sapply(1:max_clusters, function(k) {
    pam_fit <- pam(gower_dist, k = k, diss = TRUE)
    pam_fit$objective["swap"]
  })
  
  # Elbow plot for WSS
  elbow_plot <- ggplot(data.frame(k = 1:max_clusters, wss = wss), aes(x = k, y = wss)) +
    geom_point() + geom_line() +
    ggtitle("Elbow Method for Optimal Clusters") +
    xlab("Number of Clusters") +
    ylab("Total Within Sum of Squares")
  
  pdf("elbow_method.pdf")  # Save elbow plot to PDF
  print(elbow_plot)
  dev.off()  # Close the PDF device
  
  # Choose the optimal number of clusters from silhouette method
  if(optimal_k==0){
    optimal_k <- which.max(silhouette_avg) + 1
  }
  cat("Optimal number of clusters according to silhouette method:", optimal_k, "\n")
  
  # Perform final clustering using the optimal number of clusters
  final_pam <- pam(gower_dist, k = optimal_k, diss = TRUE)

  # Add cluster membership to the data
  data_numeric$Cluster <- factor(final_pam$clustering)
  
  # Create a heatmap, ordering samples by their cluster groups
  heatmap_data <- data_numeric %>% select(-Cluster)  # Exclude the Cluster column
  rownames(heatmap_data) <- rownames(data_numeric)  # Preserve original row names
  
  # Create a heatmap
  pdf("heatmap.pdf")  # Save heatmap to PDF
  pheatmap(heatmap_data, cluster_rows = TRUE, cluster_cols = TRUE,
           annotation_row = data_numeric %>% select(Cluster),
           main = "Heatmap of Clustering Results")
  dev.off()  # Close the PDF device

  # Visualize the clustering since fviz_cluster only works with numeric values
  final_pam$data = data_numeric %>% select(-Cluster)

  clustering_plot <- fviz_cluster(final_pam, geom = "point") +
    ggtitle(paste("PAM Clustering with", optimal_k, "Clusters"))
  
  pdf("clustering_plot.pdf")  # Save clustering plot to PDF
  print(clustering_plot)
  dev.off()  # Close the PDF device
  
  return(final_pam)
}


# Example of usage with mixed data
# sample_data <- data.frame(numerical_var = c(1.5, 2.3, 3.1, 4.8),
#                           categorical_var = as.factor(c("A", "B", "A", "B")))
# cluster_result <- cluster_mixed_data(sample_data)
