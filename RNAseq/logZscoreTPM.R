# Define a function to log-transform and compute Z-scores for TPM data
transform_TPM_Zscore <- function(data, exclude_cols = NULL) {
  # Check if exclude_cols is NULL, if so, apply transformation to all columns
  if (is.null(exclude_cols)) {
    # Apply log transformation and Z-scores to all columns (assuming no annotation columns)
    tpm_cols <- names(data)
  } else {
    # Separate the annotation columns
    annotations <- data[exclude_cols]
    
    # Identify the TPM columns (i.e., exclude the specified annotation columns)
    tpm_cols <- setdiff(names(data), exclude_cols)
  }
  
  # Log-transform the TPM columns (log2(TPM + 1))
  data[tpm_cols] <- log2(data[tpm_cols] + 1)
  
  # Calculate Z-scores for each gene across samples
  data[tpm_cols] <- scale(data[tpm_cols])
  
  # If exclude_cols is not NULL, combine the annotation columns back with the transformed TPM data
  if (!is.null(exclude_cols)) {
    data <- cbind(annotations, data[tpm_cols])
  }
  
  # Return the transformed data frame
  return(data)
}

# Example usage:
# Assume we have a data frame `tpm_data` where rows are samples, and we may or may not exclude
# the "sample_id" column from transformation.



