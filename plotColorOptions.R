plot_colors <- function() {

  color <- list()

  # Create a data frame with the colors and labels
  df <- data.frame(
    color = c("#0073C2FF", "#619CFF", "#56B4E9FF", "#9CC0F4",  # blue
              "#B79F00", "#EFC000FF", "#F7D75C", "#FFEA7D", "#E0E97F",   # yellow
              "#00BA38", "#92D050", "#B5E3C3", "#ACF2AD", # green
              "#868686FF", "#D0CECE", # grey
              "#CD534CFF", "#F24000", "#F8766D", "#FFB3A7", "#FFD8C9",   # red
              "#C8ACE7" ), # purple
    label = c(1:21)
  )

  # Ensure the 'label' column is a factor in the original order
  df$label <- factor(df$label, levels = df$label)
  
  # Create a simple bar plot using ggplot2
  g <- ggplot(df, aes(x = label, y = 1, fill = color)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    labs(title = "Color Palette", x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none")
  
  color$color <- df$color
  color$g <- g

  return(color)
  
}

# color scale that could be added to theme
#library(RColorBrewer)  # For Brewer color palettes
#cluster_colors <- brewer.pal(color_number, name="Set1")
# colorRampPalette(c("blue", "white", "red"))(50)
# colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100) # RdBu, PiYG, or PRGn
#scale_color_brewer(palette = "Set1")

#library(viridis)       # For Viridis color palettes
#scale_color_viridis_d(option = "plasma")
