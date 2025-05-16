# venn diagram
venn_diagram <- function(dl){
  #'  @param dl : input data list
  #'              e.g. dl=list(a=,b=,c=)
  #'              
  ggvenn::ggvenn(
    dl, show_percentage = FALSE,
    fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#56B4E9FF")[1:length(dl)],
    stroke_size = 0.5, set_name_size = 8, text_size=6
  )
}