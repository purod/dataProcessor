library(plotly)
library(umap)

library(umap)
library(ggplot2)

plot_umap <- function(data, labels, n_components = 2, random_state = 15) {
  # Perform UMAP
  umap_result <- umap(data, n_components = n_components, random_state = random_state)
  
  # Extract the layout and convert to data frame
  layout <- as.data.frame(umap_result$layout)
  
  # Combine the layout with labels
  final_data <- cbind(layout, Labels = labels)
  colnames(final_data) <- c("UMAP1", "UMAP2", "groups")
  
  # Create the plot
  fig <- ggplot(final_data, aes(x = UMAP1, y = UMAP2, color = groups)) +
    geom_point(size = 2, alpha = 0.7) +
    labs(title = "UMAP of Data", x = "UMAP1", y = "UMAP2", color = "groups") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#e5ecf6"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
  return(fig)
}


