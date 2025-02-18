# load in libraries
library(data.table)
library(plyr)
library(tidyverse)
library(DT)
library('survcomp')
library(viridis)
library(readxl)

## add ffix 
add.Type <- function(df, excludeVars=NULL){
  
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  
  tagList <- apply(df2, 2, function(x){
    if(Hmisc::all.is.numeric(x, extras = NA)){
      if(length(table(x)) == 2){
        '_binary'
      }else if(length(table(x)) == 3){
        '_categorical'
      }else{
        '_continuous'
      }
    }else if(length(table(x)) == 2){
      '_binary'
    }else{
      '_categorical' 
    }
  }) %>% unlist()
  
  colnames(df2) <- paste0(colnames(df2), tagList)
  
  df <- data.frame(df1,df2)
}
add_ffix <- function(df, prefix=NULL, suffix=NULL, excludeVars=NULL) {
  
  # Separate included and excluded variables
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  
  # Modify column names by adding prefix and suffix if they are provided
  colnames(df2) <- paste0(if (!is.null(prefix)) paste0(prefix, "_") else "",
                          colnames(df2),
                          if (!is.null(suffix)) paste0("_", suffix) else "")
  
  # Combine back the included and excluded variables
  df <- data.frame(df1, df2)
  
  return(df)
}
get.conciseName <- function(dfName, suffix="_continuous|_binary|_categorical"){
  dfName_nosuffix = gsub( suffix, "", dfName )
  concise_name <- sapply( dfName_nosuffix, 
          function(name_unit){ 
            string_split = strsplit(name_unit,"_")[[1]]
            if( length(string_split) > 1 & grepl("^ENSG",name_unit) ){
              return(string_split[2])
            }else{
              return(name_unit)
            }
          }
  )
}


# tables
# DT::datatable()

# histgram
horiHist <- function(df,flip=T){
  g <- ggplot(df, aes(x =name , y = Frequency)) +
    geom_bar(stat = "identity", position = "identity", fill = "lightblue")
  if(flip){g <- g + coord_flip()}else{g}
  return(g)
} 
# barplot
barPlot <- function(df, xyfill, stacked=T){
  g <- ggplot(df, aes_string(fill=xyfill[3], y=xyfill[2], x=xyfill[1])) + 
    geom_bar(position="stack", stat="identity") # stacked barplot
  if(!stacked){g=g+position_dodge()}  # barplot side by side    
  g
}
# boxplot
boxPlot <- function(df, xyfill){
  g <- ggplot(df, aes_string(fill=xyfill[3], y=xyfill[2], x=xyfill[1])) + 
    geom_boxplot() # boxplot side by side
}
# scatter plot with correlation values
scatterPlot <- function(df, xyfill){
  
  x=xyfill[1];y=xyfill[2];
  dfx=df[,xyfill[1]];dfy=df[,xyfill[2]];
  
  ggplot( df, aes_string(x = xyfill[1], y = xyfill[2])) +
    geom_point(size=2) + 
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    #ggrepel::geom_text_repel(aes(x=model, y=malig, label = GeneName),
    #                box.padding = 0.1,size=2, max.overlaps=20)
    annotate("text", x = max(dfx), y = max(dfy), 
             label = paste("Pearson Cor =", round(cor(dfx, dfy), 2), "\n",
                           "Spearman Cor =", round(cor(dfx, dfy, method="spearman"), 2)),  
             hjust = 1, vjust = 1, size =5) + 
    theme_pub() 
}

# scale_fill_viridis(discrete = T) 
# titles: labs(x = x_label, y = y_label, title = title)
# outlier show: geom_boxplot(outlier.shape = NA)
# scale of x and y: scale_y_continuous(limits = c(0,0.0025))
# gragh groups: gridExtra::grid.arrange(grobs = lapply_obj, ncol = 4, top="top title")

# C. multiple graphs
## Approach 1
# scatterPlots <- lapply( c(), function(auc){ scatterPlot() }) %>% 
# gridExtra::grid.arrange(grobs = ., ncol = 3, top = "Correlation")

boxplotMatrixbyGroup <- function(sample_feature, group, features){
  matrix_group <- cbind(sample_feature, Group = group)
  
  # Convert the wide matrix (including group) to a long format
  plot_data <- as.data.frame(matrix_group) %>%
    rownames_to_column(var = "Sample") %>%
    pivot_longer(
      cols = -c(Sample, Group),
      names_to = "Gene",
      values_to = "Expression"
    ) %>%
    mutate(Group = factor(Group)) %>%   # Ensure Group is treated as a factor
  filter(Gene%in%features) %>% 
  mutate(Gene = factor(Gene, levels=features))

  # Plot using ggplot2
  ggplot(plot_data, aes(x = Gene, y = Expression, fill = Group)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 2, notch = FALSE) +
    #geom_jitter(width = 0.2, size = 2, alpha = 0.6, aes(color = Group)) +
    labs(
      title = "Gene Expression Across Groups",
      x = "Gene",
      y = "Normalized Expression"
    ) +
    theme_pub(grid=TRUE, legend_position = "top")
} 



library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

boxplotMatrixbyGroupCovariate <- function(sample_feature, group, covariate, features, legend="right") {
  matrix_group <- cbind(sample_feature, Group = group, Covariate = covariate)
  
  # Convert the wide matrix (including group and covariate) to a long format
  plot_data <- as.data.frame(matrix_group) %>%
    rownames_to_column(var = "Sample") %>%
    pivot_longer(
      cols = -c(Sample, Group, Covariate),
      names_to = "Gene",
      values_to = "Expression"
    ) %>%
    filter(Gene %in% features) %>%
    mutate(
      Gene = factor(Gene, levels = features),
      Group = factor(Group),
      Covariate = factor(Covariate)
    )

  # Plot using ggplot2
  ggplot(plot_data, aes(x = Gene, y = Expression, fill = interaction(Group, Covariate))) +
    geom_boxplot(outlier.shape = 16, outlier.size = 2, notch = FALSE, position = position_dodge(0.8)) +
    labs(
      title = "Gene Expression Across Groups and Covariates",
      x = "Gene",
      y = "Normalized Expression",
      fill = "Group & Covariate"
    ) +
    theme_pub(grid = TRUE, legend = legend) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# multiple graphs
# 1. multiple graph on the same page
# library(patchwork)
# (p1|p2)/(p3/p4)