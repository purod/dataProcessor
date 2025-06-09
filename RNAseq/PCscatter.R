PCscatter <- function(expr_matrix, clinical_df, 
                                                      n_pcs = 4,
                                                      continuous_vars = c("age", "out_os_time", "pfi_time"), 
                                                      id_col = "bcr_patient_barcode", 
                                                      plot_file = NULL,
                                                      scale_expr = TRUE) {
  require(ggplot2)
  require(dplyr)
  require(gridExtra)

  # Transpose expression matrix to samples x genes
  expr_t <- t(expr_matrix)

  # Match sample IDs
  sample_ids <- intersect(rownames(expr_t), clinical_df[[id_col]])
  expr_t <- expr_t[sample_ids, , drop = FALSE]
  clinical_sub <- clinical_df[clinical_df[[id_col]] %in% sample_ids, ]
  rownames(clinical_sub) <- clinical_sub[[id_col]]
  clinical_sub <- clinical_sub[sample_ids, , drop = FALSE]

  # PCA
  pca_res <- prcomp(expr_t, center = TRUE, scale. = scale_expr)
  pcs_df <- as.data.frame(pca_res$x[, 1:n_pcs])
  pcs_df[[id_col]] <- rownames(pcs_df)

  # Merge PCA with clinical data
  merged <- inner_join(pcs_df, clinical_sub, by = id_col)
  pcs <- paste0("PC", 1:n_pcs)

  # Loop over continuous variables and plot
  for (var in continuous_vars) {
    message("Plotting PCs vs ", var)
    plot_list <- lapply(pcs, function(pc) {
      df <- merged %>% filter(!is.na(.data[[pc]]) & !is.na(.data[[var]]))
      scatterPlot(df, xyfill = c(var, pc)) +
        ggtitle(paste0(pc, " vs ", var))
    })

    if (!is.null(plot_file)) {
      pdf(plot_file, width = 10, height = 2.5 * ceiling(n_pcs / 2))
      gridExtra::grid.arrange(grobs = plot_list, ncol = 2, top = paste("PCs vs", var))
      dev.off()
    } else {
      gridExtra::grid.arrange(grobs = plot_list, ncol = 2, top = paste("PCs vs", var))
    }
  }
}
