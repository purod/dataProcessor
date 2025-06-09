biplotByGroup <- function(df, groups = NULL, show_outliers = FALSE, 
                          legendPos = "top", ellipse = TRUE, 
                          n_pcs = 5, test_method = c("anova", "kruskal")) {
  require(ggbiplot)
  require(dplyr)
  require(ggplot2)
  require(tidyr)

  test_method <- match.arg(test_method)
  ir.pca <- prcomp(df, center = TRUE, scale. = TRUE)
  pc_df <- as.data.frame(ir.pca$x[, 1:max(2, n_pcs)])
  if (!is.null(groups)) {
    pc_df$group <- as.factor(groups)
  }

  # --- Long-format and p-value calculation ---
  box_df <- pc_df %>%
    select(1:n_pcs, group) %>%
    pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "value")

  # Calculate p-values for each PC
  pval_df <- box_df %>%
    group_by(PC) %>%
    dplyr::summarise(
      p_value = if (test_method == "anova") {
        summary(aov(value ~ group))[[1]][["Pr(>F)"]][1]
      } else {
        kruskal.test(value ~ group)$p.value
      },
      .groups = "drop"
    ) %>%
    mutate(label = paste0("p = ", signif(p_value, 3)))

  # Merge p-values back into plot data
  label_positions <- box_df %>%
    group_by(PC) %>%
    dplyr::summarise(y = max(value), .groups = "drop") %>%
    left_join(pval_df, by = "PC")

  # --- PCA Biplot ---
  if (!is.null(groups)) {
    g_pca <- ggbiplot::ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,
                                group = as.character(groups),
                                ellipse = ellipse, circle = FALSE,
                                var.axes = FALSE)
  } else {
    g_pca <- ggbiplot::ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,
                                ellipse = ellipse, circle = FALSE,
                                var.axes = FALSE)
  }

  if (show_outliers) {
    U <- as.data.frame(ir.pca$x[, 1:2])
    outlier_ID <- apply(U, 2, function(x) which(abs(x - mean(x)) > (3 * sd(x)))) %>%
      Reduce(union, .)
    cat("Outliers (3 SD beyond mean):\n")
    print(rownames(U)[outlier_ID])
    g_pca <- g_pca + geom_point(data = U[outlier_ID, ], aes(x = PC1, y = PC2), colour = "red", size = 1)
  }

  g_pca <- g_pca + theme(legend.position = legendPos,
                         panel.background = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.line.x = element_line(color = "black", size = 0.5),
                         axis.line.y = element_line(color = "black", size = 0.5))

  # --- Final Boxplot ---
  g_box <- ggplot(box_df, aes(x = PC, y = value, fill = group)) +
    geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
    #geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
    #            alpha = 0.5, size = 1, aes(color = group)) +
    geom_text(data = label_positions, aes(x = PC, y = y * 1.05, label = label), 
              inherit.aes = FALSE, size = 3.5) +
    theme_minimal() +
    labs(title = "PC Scores by Group", x = "Principal Component", y = "Score") +
    theme(legend.position = "right")

  return(list(biplot = g_pca, boxplot = g_box))
}



# Define the custom function
plot_variance_explained <- function(df, ncp = 10, add_labels = TRUE) {
  # ncp: Number of principal components to display (default is 10)
  # add_labels: Whether to add labels showing the percentage of variance explained
  
  pca_result <- prcomp(df, center = TRUE, scale. = TRUE)

  # Plot the explained variance using fviz_eig
  g <- factoextra::fviz_eig(pca_result, 
           choice = "variance",  # Plot variance
           ncp = ncp,            # Number of components to show
           addlabels = add_labels, # Add percentage labels to bars
           barfill = "steelblue",  # Color of bars
           barcolor = "black",     # Border color of bars
           linecolor = "red")      # Color of cumulative variance line
  return(g)
}

# Example usage with PCA from prcomp
# Assume you have a dataset called 'data' and you have performed PCA on it.


PCsCate <- function(expr_matrix, clinical_df,
                                                       categorical_vars,
                                                       id_col = "bcr_patient_barcode",
                                                       n_pcs = 3, base_size=8,
                                                       plot_file = NULL) {
  require(dplyr)
  require(ggplot2)

  # Match sample IDs
  sample_ids <- intersect(colnames(expr_matrix), clinical_df[[id_col]])
  expr_matrix <- expr_matrix[, sample_ids, drop = FALSE]
  clinical_sub <- clinical_df[clinical_df[[id_col]] %in% sample_ids, ]
  rownames(clinical_sub) <- clinical_sub[[id_col]]
  clinical_sub <- clinical_sub[sample_ids, , drop = FALSE]  # reorder

  # Open PDF for output


  plots_out <- list()
  if (!is.null(plot_file)) {
    pdf(plot_file, width = 8, height = 6)
  }
  for (clinic_var in categorical_vars) {
    message("Processing: ", clinic_var)

    valid_idx <- !is.na(clinical_sub[[clinic_var]])

    bimatrix <- expr_matrix[, valid_idx, drop = FALSE]
    groups <- clinical_sub[[clinic_var]][valid_idx]

    # Call biplotByGroup (assumes PCA is done inside)
    plot_results <- biplotByGroup(t(bimatrix), groups = groups,
                                  show_outliers = FALSE, legendPos = "right",
                                  ellipse = FALSE, n_pcs = n_pcs,
                                  test_method = "kruskal")

    # Plot biplot and boxplot separately
    p1 <- plot_results$biplot + theme_pub(base_size) + ggtitle(paste("Biplot -", clinic_var))
    p2 <- plot_results$boxplot + theme_pub(base_size) + ggtitle(paste("Boxplot -", clinic_var))
    
    if (!is.null(plot_file)) {
      print(p1)
      print(p2)
    } else {
      plots_out[[clinic_var]] <- list(biplot = p1, boxplot = p2)
    }
  }

  if (!is.null(plot_file)) {
    dev.off()
    return(invisible(NULL))
  } else {
    return(plots_out)
  }
}



