oncoplot <- function(mut_matrix, 
                     top_n = 20,
                     sample_annotation = NULL,
                     title = "Presence of Mutations in Top Genes across Samples") {
  library(ComplexHeatmap)
  library(circlize)
  library(ggplot2)
  library(grid)
  library(gridExtra)

  # 确保矩阵为样本 × 基因，后续再转置为基因 × 样本用于绘图
  if (ncol(mut_matrix) < nrow(mut_matrix)) {
    mut_matrix <- t(mut_matrix)
  }
  mut_matrix <- as.matrix(mut_matrix)
  mut_matrix <- apply(mut_matrix, 2, as.numeric)
  
  # 计算 top_n 基因
  gene_freq <- colSums(mut_matrix)
  top_genes <- names(sort(gene_freq, decreasing = TRUE))[1:min(top_n, length(gene_freq))]
  mat_top <- mut_matrix[, top_genes, drop = FALSE]
  mat_top <- t(mat_top)  # 转置：现在是 genes x samples

  # 构造 barplot 频率图（作为左注释条）
  gene_counts <- rowSums(mat_top)
  freq_df <- data.frame(Gene = factor(rownames(mat_top), levels = rev(rownames(mat_top))),
                        Count = gene_counts[rev(rownames(mat_top))])

  barplot_gg <- ggplot(freq_df, aes(x = Count, y = Gene)) +
    geom_bar(stat = "identity", fill = "#0571b0") +
    theme_minimal() +
    labs(x = "Mutation Count", y = NULL) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10))

  barplot_grob <- gridExtra::grid.arrange(barplot_gg, ncol = 1)

  # 创建左边 annotation（频率条）
  left_anno <- rowAnnotation(Mutation_Freq = anno_barplot(gene_counts,
                                                          bar_width = 0.8,
                                                          gp = gpar(fill = "red"),
                                                          axis_param = list(
                                                            side = "bottom",
                                                            labels_rot = 0  # 设置标签为水平
                                                          ),
                                                          border = FALSE),
                             width = unit(2.5, "cm"),
                             annotation_name_side = "bottom")

  # 样本注释（可选）
  top_anno <- NULL
  if (!is.null(sample_annotation)) {
    top_anno <- HeatmapAnnotation(df = sample_annotation[colnames(mat_top), , drop = FALSE])
  }

  # 画 Heatmap
  Heatmap(mat_top,
          name = "Mutation",
          col = c("0" = "white", "1" = "#0571b0"),  # 蓝色
          show_row_names = TRUE,
          show_column_names = TRUE,
          show_column_dend = FALSE,
          cluster_rows = FALSE,
          cluster_columns = TRUE,  # 聚类样本
          left_annotation = left_anno,
          top_annotation = top_anno,
          row_names_side = "left",
          column_title = title,
          rect_gp = gpar(col = "grey80", lwd = 0.5),
          heatmap_legend_param = list(title = NULL))
}



