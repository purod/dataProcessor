
# Volcano plot function
library(ggplot2)
library(ggrepel)

# Function to create a volcano plot
plotVolcano <- function(data, logFC_col = "logFC", pvalue_col = "P.Value", 
                         logFC_threshold = 1, pvalue_threshold = 0.05, 
                         label_genes = NULL, top_n_labels = 10) {
  # Prepare data
  data$Significance <- "Not Sig"
  data$Significance[data[[logFC_col]] > logFC_threshold & data[[pvalue_col]] < pvalue_threshold] <- "Up"
  data$Significance[data[[logFC_col]] < -logFC_threshold & data[[pvalue_col]] < pvalue_threshold] <- "Down"
  
  # Label top N significant genes if no specific labels are provided
  if (is.null(label_genes)) {
    top_up <- head(data[order(data[[pvalue_col]], decreasing = FALSE) & data$Significance == "Up", ], top_n_labels)
    top_down <- head(data[order(data[[pvalue_col]], decreasing = FALSE) & data$Significance == "Down", ], top_n_labels)
    label_genes <- c(rownames(top_up), rownames(top_down))
  }
  
  # Filter data to label
  data$Label <- ifelse(rownames(data) %in% label_genes, rownames(data), NA)
  
  # Volcano plot
  ggplot(data, aes_string(x = logFC_col, y = paste0("-log10(", pvalue_col, ")"))) +
    geom_point(aes(color = Significance), alpha = 0.6) +
    scale_color_manual(values = c("Down" = "blue", "Not Sig" = "gray", "Up" = "red")) +
    geom_vline(xintercept = c(-logFC_threshold, logFC_threshold), linetype = "dashed", color = "black") +
    geom_hline(yintercept = -log10(pvalue_threshold), linetype = "dashed", color = "black") +
    #geom_text_repel(aes(label = Label), size = 3, box.padding = 0.3, max.overlaps = 15) +
    geom_label_repel(aes(label = Label), size = 3, box.padding = 0.3, max.overlaps = 15, label.size = 0.2)+
    labs(
      title = "Volcano Plot",
      x = "logFC",
      y = "-log10(p-value)"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

