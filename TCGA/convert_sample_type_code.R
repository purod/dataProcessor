convert_sample_type_code <- function(code_vec) {
  # Lookup table
  code_map <- c(
    "01" = "Primary Solid Tumor",
    "02" = "Recurrent Solid Tumor",
    "03" = "Primary Blood Derived Cancer – Peripheral Blood",
    "06" = "Metastatic",
    "07" = "Additional Metastatic",
    "10" = "Blood Derived Normal",
    "11" = "Solid Tissue Normal"
  )
  
  # Extract two-digit codes (handles cases like "01A")
  code_clean <- substr(code_vec, 1, 2)
  
  # Map codes to labels
  type_label <- code_map[code_clean]
  
  # Replace missing matches with NA or "Unknown"
  type_label[is.na(type_label)] <- "Unknown"
  
  return(type_label)
}
