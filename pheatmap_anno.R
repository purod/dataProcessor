library(pheatmap)
pheatmap_anno <- function(df, genes, annotation_col, 
                          color_palette = colorRampPalette(c("blue", "white", "red"))(50),
                          annotation_colors = NULL, 
                          scale_data = TRUE) {
  
    # Check if specified genes exist in the dataframe
    missing_genes <- setdiff(genes, rownames(df))
    if (length(missing_genes) > 0) {
        stop("The following genes are not found in the dataframe: ", paste(missing_genes, collapse = ", "))
    }
    
    # Check if the annotation column exists
    if (!(annotation_col %in% colnames(df))) {
        stop("Annotation column not found in the dataframe!")
    }
  
    # Extract the expression matrix (genes as rows, samples as columns)
    expression_matrix <- df[genes, setdiff(colnames(df), annotation_col), drop = FALSE]
    
    # Extract annotation data
    annotation_df <- data.frame(Group = df[[annotation_col]], row.names = rownames(df))
  
    # Scale data if required
    if (scale_data) {
        expression_matrix <- scale(expression_matrix)
    }
  
    # Create the heatmap
    pheatmap(expression_matrix,
             annotation_col = annotation_df,  # Add annotations
             color = color_palette,  # Custom color palette
             annotation_colors = annotation_colors,  # Custom annotation colors
             clustering_distance_rows = "euclidean",
             clustering_distance_cols = "euclidean",
             clustering_method = "complete",
             show_rownames = TRUE,
             show_colnames = TRUE)
}

