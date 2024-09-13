# CNV input
mat <- modelDFTumorID %>% select( TumorID, matches("CNV_") ) %>% 
  pivot_longer(cols = -TumorID, names_to = "Variable", values_to = "Value") %>%
  mutate(Value=ifelse( Value==0, "background", 
                       ifelse( grepl("loss", Variable), "loss", "gain" ))) %>% 
  pivot_wider(names_from = TumorID, values_from = Value) %>% 
  column_to_rownames("Variable") 

# Create a column annotation for cohort
cohort_annotation <- HeatmapAnnotation(
    Cohort = modelDFTumorID$TumorType,
    col = list(Cohort = c("NSCLC" = "#0073C2FF", "colon cancer" = "#EFC000FF", "pancreatic cancer"="#868686FF",
                          "breast cancer"="#CD534CFF", "renal cancer"="#56B4E9FF")),
    annotation_legend_param = list(Cohort = list(title = "Cohort"))
)

library(ComplexHeatmap)
alter_fun <- list( background = function(x, y, w, h) { grid.rect(x, y, w-unit(2, "mm"), h-unit(0.5, "mm"), gp = gpar(fill="lightgrey", col = NA)) },
        loss = function(x, y, w, h) { grid.rect(x, y, w-unit(0.1, "mm"), h*0.33, gp = gpar(fill="red", col = NA)) },
        gain = function(x, y, w, h) { grid.rect(x, y, w-unit(0.1, "mm"), h*0.33, gp = gpar(fill="blue", col = NA)) })
getOrder = oncoPrint(mat, 
    get_type = function(x) x, # how to get the different types of mutations for each entry in the matrix; in our case there are just two types "" and "MUT"
        alter_fun = alter_fun, # to represent each mutation type in the plot; a mapping from mutation name to a function
    col=c("loss"="red", "gain"="blue"), # legend color of the mutation types; should match what is given in alter_fun
    column_title = "Total number of CNVs per patient",  # a title
    top_annotation = cohort_annotation,
    # column_order = test,
    heatmap_legend_param = list(title = "", at = c("loss", "gain"), labels = c("loss","gain")))    # some labels for the legend

order_label = modelDFTumorID$TumorType[ as.vector(getOrder@column_order) ]
orders = c( as.vector(getOrder@column_order)[order_label == "NSCLC"],
            as.vector(getOrder@column_order)[order_label == "colon cancer"],  
            as.vector(getOrder@column_order)[order_label == "pancreatic cancer"], 
            as.vector(getOrder@column_order)[order_label == "breast cancer"],
            as.vector(getOrder@column_order)[order_label == "renal cancer"] )  
oncoPrint(mat, 
    get_type = function(x) x, # how to get the different types of mutations for each entry in the matrix; in our case there are just two types "" and "MUT"
        alter_fun = alter_fun, # to represent each mutation type in the plot; a mapping from mutation name to a function
    col=c("loss"="red", "gain"="blue"), # legend color of the mutation types; should match what is given in alter_fun
    column_title = "Total number of CNVs per patient",  # a title
    top_annotation = cohort_annotation,
    column_order = orders,
    heatmap_legend_param = list(title = "", at = c("loss", "gain"), labels = c("loss","gain")))    # some labels for the legend

