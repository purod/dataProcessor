library(EnhancedVolcano)
# Function to generate a volcano plot
plot_volcano <- function(res, lfc_threshold = 1, pval_threshold = 0.05, ylimMax=10, title = "Volcano Plot") {
  
  # Convert results to a data frame (if not already)
  res_df <- as.data.frame(res)
  
  # Ensure the results contain necessary columns
  if (!("log2FoldChange" %in% colnames(res_df)) || !("padj" %in% colnames(res_df))) {
    stop("Results object must contain 'log2FoldChange' and 'padj' columns.")
  }
  
  # Create the volcano plot
  EnhancedVolcano(
    res_df,
    lab = rownames(res_df),        # Gene labels (rownames assumed to be gene names)
    x = "log2FoldChange",          # X-axis: Log2 fold change
    y = "padj",                    # Y-axis: Adjusted p-value
    xlim = c(-3, 3),               # Set limits for fold-change
    ylim = c(0, ylimMax), # Dynamic y-limits based on data
    title = title,
    pCutoff = pval_threshold,      # Significance threshold
    FCcutoff = lfc_threshold,      # Fold-change threshold
    pointSize = 2.0,
    labSize = 3.0,
    colAlpha = 1.0,
    legendPosition = "right",
    legendLabSize = 12,
    legendIconSize = 4.0,
    drawConnectors = FALSE,
    widthConnectors = 0.5
  )
}

# usage
# plot_volcano(
#   res = result$raw_results,                  # DESeq2 results object with shrinkage applied
#   lfc_threshold = 1,             # Threshold for log2 fold change
#   pval_threshold = 0.25,         # Threshold for adjusted p-value
#   title = "Volcano Plot: DESeq2 Results"
# )