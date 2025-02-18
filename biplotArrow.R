biplotArrow <- function(df, sampleType, sampleID, from, to){
    
    # input data
    # df: genes as columns
    # sampleType: sample type vector
    # sampleID: sample ID vector
    # from and to: from which type to which type
    
    # Load necessary libraries
    library(ggfortify)

    paired_sample <- intersect( sampleID[sampleType==from], sampleID[sampleType==to])

    # Perform PCA on the feature columns (excluding Patient_ID and Sample_Type)
    pca_result <- prcomp(df, scale. = TRUE)

    # Prepare the PCA data frame
    pca_df <- as.data.frame(pca_result$x)
    pca_df$Patient_ID <- sampleID
    pca_df$Sample_Type <- sampleType

    primary_points <- pca_df %>% filter(Sample_Type == from & Patient_ID %in% paired_sample ) %>% arrange(Patient_ID)
    recurrent_points <- pca_df %>% filter(Sample_Type == to & Patient_ID %in% paired_sample ) %>% arrange(Patient_ID)

    # Plot the PCA biplot
    p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Sample_Type)) +
     geom_point(size = 3) +
     theme_minimal() +
     labs(title = "PCA Biplot: Pre-treatment vs On-treatment", x = "PC1", y = "PC2")

    # Add arrows only for matched patients
    p <- p + geom_segment(data = primary_points,
                      aes(x = PC1, y = PC2,
                          xend = recurrent_points$PC1, yend = recurrent_points$PC2),
                          arrow = arrow(length = unit(0.2, "cm")), color = "black")

    # Display the plot
    return(p)
}
