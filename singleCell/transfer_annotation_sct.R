transfer_annotations_sct <- function(reference_seurat, query_list, refdata) {
  # Apply SCTransform to the reference dataset
  reference_seurat <- RunPCA(reference_seurat)
  
  # Process each query Seurat object
  processed_queries <- list()
  
  for (i in seq_along(query_list)) {
    query <- query_list[[i]]
    
    # Apply SCTransform to query object
    query <- RunPCA(query)
    
    # Find anchors between reference and query
    anchors <- FindTransferAnchors(reference = reference_seurat, query = query, 
               normalization.method = "SCT")
    
    # Transfer cell type labels
    modified_query <- TransferData(anchorset = anchors, reference = reference_seurat, query = query,
                                refdata = refdata)
    
    # Save updated query object
    processed_queries[[i]] <- modified_query
  }
  
  return(processed_queries)
}