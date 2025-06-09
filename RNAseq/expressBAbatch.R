expressBAbatch <- function(expr_before, expr_after, base_size=8,
    plot_file = "correlation_histograms.pdf") {
  require(ggplot2)
  require(gridExtra)

  if (!all(dim(expr_before) == dim(expr_after)))
    stop("Dimensions do not match.")
  if (!all(rownames(expr_before) == rownames(expr_after)))
    stop("Gene names (rownames) do not match.")
  if (!all(colnames(expr_before) == colnames(expr_after)))
    stop("Sample names (colnames) do not match.")

  expr_before <- as.matrix(expr_before)
  expr_after <- as.matrix(expr_after)

  # Gene-wise correlation
  gene_cor <- sapply(1:nrow(expr_before), function(i) {
    cor(expr_before[i, ], expr_after[i, ], use = "pairwise.complete.obs")
  })

  # Sample-wise correlation
  sample_cor <- sapply(1:ncol(expr_before), function(i) {
    cor(expr_before[, i], expr_after[, i], use = "pairwise.complete.obs")
  })

  # Convert to data frames for ggplot
  gene_df <- data.frame(correlation = gene_cor, type = "Gene")
  sample_df <- data.frame(correlation = sample_cor, type = "Sample")

  # Plot 1: Gene-wise correlation histogram
  p1 <- ggplot(gene_df, aes(x = correlation)) +
    geom_histogram(bins = 20, fill = "steelblue", color = "white") +
    theme_pub(base_size) +
    ggtitle("Gene-wise Correlation: Before vs After") +
    xlab("Pearson Correlation") +
    ylab("Count")

  # Plot 2: Sample-wise correlation histogram
  p2 <- ggplot(sample_df, aes(x = correlation)) +
    geom_histogram(bins = 20, fill = "tomato", color = "white") +
    theme_pub(base_size) +
    ggtitle("Sample-wise Correlation: Before vs After") +
    xlab("Pearson Correlation") +
    ylab("Count")

  # Save both on the same PDF page
  if(is.null(plot_file)){
    return(list(p1,p2))
  }else{
    pdf(plot_file, width = 10, height = 5)
    gridExtra::grid.arrange(p1, p2, ncol = 2)
    dev.off()
  }
}
