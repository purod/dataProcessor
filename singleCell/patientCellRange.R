patientCellRange <- function(patient_list, min_cells = 50, max_cells = 5000) {
  # Get initial cell counts before filtering
  initial_cell_counts <- sapply(patient_list, function(x) ncol(x))
  df_before <- data.frame(Patient = names(initial_cell_counts), CellCount = initial_cell_counts)

  # Plot histogram BEFORE filtering
  p1 <- ggplot(df_before, aes(x = CellCount)) +
    geom_histogram(bins=30, fill = "gray", color = "black", alpha = 0.7) +
    geom_vline(xintercept = min_cells, color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = max_cells, color = "blue", linetype = "dashed", size = 1) +
    annotate("text", x = min_cells, y = max(table(df_before$CellCount)), label = paste("Min:", min_cells), color = "red", vjust = -1, size = 4) +
    annotate("text", x = max_cells, y = max(table(df_before$CellCount)), label = paste("Max:", max_cells), color = "blue", vjust = -1, size = 4) +
    labs(title = "Cell Count Distribution Before Filtering", x = "Number of Cells", y = "Frequency") +
    theme_minimal()
  
  print(p1)  # Show before-filtering histogram

  # Filter out patients with fewer than min_cells
  patient_list <- patient_list[initial_cell_counts >= min_cells]

  # Downsample patients with more than max_cells
  for (i in seq_along(patient_list)) {
    num_cells <- ncol(patient_list[[i]])
    if (num_cells > max_cells) {
      keep_cells <- sample(colnames(patient_list[[i]]), max_cells)
      patient_list[[i]] <- subset(patient_list[[i]], cells = keep_cells)
    }
  }
  return(patient_list)
}

