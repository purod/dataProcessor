genomiclong2wide <- function(df, 
                             gene_col = "Hugo Symbol", 
                             patient_col = "Patient",
                             variant_detail_col = "HGVS_p",
                             mutation_level_genes = NULL,
                             binary = FALSE,
                             sep = "_") {
  #' Convert long-form genomic mutation table into wide matrix
  #'
  #' @param df A data frame in long format with gene, patient, and variant columns.
  #' @param gene_col Column name for gene symbol (e.g. "Hugo Symbol").
  #' @param patient_col Column name for patient/sample ID (e.g. "Patient").
  #' @param variant_detail_col Column with variant-level detail (e.g. "HGVS_p", "Taf").
  #' @param mutation_level_genes Character vector of genes for which detailed variants are included.
  #' @param binary If TRUE, converts the output matrix to 0/1 (presence/absence).
  #' @param sep Character used to join gene and variant names (default "_").
  #'
  #' @return A wide-format data frame with patients as rows and features as columns.

  library(dplyr)
  library(tidyr)
  gene_col <- rlang::sym(gene_col)
  patient_col <- rlang::sym(patient_col)
  variant_col <- rlang::sym(variant_detail_col)

  # Gene-level features
  df_gene <- df %>%
    mutate(Feature = !!gene_col)

  # Variant-level features for specified genes
  if (!is.null(mutation_level_genes)) {
    df_variant <- df %>%
      filter(!!gene_col %in% mutation_level_genes) %>%
      mutate(Feature = paste0(!!gene_col, sep, !!variant_col))

    df_combined <- bind_rows(df_gene, df_variant)
  } else {
    df_combined <- df_gene
  }

  # Wide-format feature matrix
  mat <- df_combined %>%
    group_by(!!patient_col, Feature) %>%
    dplyr::summarise(Count = n(), .groups = "drop") %>%
    pivot_wider(names_from = Feature, values_from = Count, values_fill = 0)

  # Convert to binary if specified
  if (binary) {
    mat[,-1] <- lapply(mat[,-1], function(x) as.integer(x > 0))
  }

  return(as.data.frame(mat))
}