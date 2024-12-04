theme_pub <- function(base_size = 8, legend_position = "right", grid = FALSE) {
  print("Default ratio is main title:2, axis title:1.5, axis text: 2, legend: 2, text:3")
  
  # Base theme
  theme_obj <- theme_classic(base_size = base_size) +
    theme(
      # Axis and figure title
      plot.title = element_text(face = "bold", size = rel(2)),
      axis.title = element_text(face = "bold", size = rel(2)),
      # Axis text and lines
      axis.text.y = element_text(size = rel(2), color = "black"),
      axis.text.x = element_text(size = rel(2), color = "black", angle = 45, hjust = 1),
      axis.line = element_line(size = rel(2), linetype = "solid"),
      # Legend
      legend.position = legend_position,
      legend.title = element_text(face = "bold", size = rel(2)),
      legend.text = element_text(face = "bold", size = rel(2)),
      # strip text
      strip.text = element_text(size = rel(2), face = "bold")
    )
  
  # Add or remove grid lines based on the 'grid' argument
  if (grid) {
    theme_obj <- theme_obj + 
      theme(
        panel.grid.major = element_line(size = 0.5, linetype = "dotted", color = "gray80"),
        panel.grid.minor = element_line(size = 0.25, linetype = "dotted", color = "gray90")
      )
  } else {
    theme_obj <- theme_obj +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  
  return(theme_obj)
}
