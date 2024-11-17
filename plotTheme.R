theme_pub <- function(legend.position = "right" ){
  print("Default ratio is main title:2, axis title:1.5, axis text: 2, legend: 2, text:3")
  theme_classic( base_size = 8 ) +
    theme(
      # axis and fig title
      plot.title = element_text(face = "bold",size=rel(2)),
      axis.title = element_text(face = "bold",size=rel(2)),
      # axis text and lines
      axis.text.y = element_text(size=rel(2),color="black"),
      axis.text.x = element_text(size=rel(2),color="black",angle=45,hjust=1),
      axis.line = element_line(size=rel(2),linetype="solid"),
      # legend 
      legend.position = legend.position,
      legend.title = element_text(face = "bold",size=rel(2)),
      legend.text = element_text(face = "bold",size=rel(2)),
    ) 
}