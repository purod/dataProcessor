# load in libraries
library(data.table)
library(plyr)
library(tidyverse)
library(DT)
library('survcomp')
library(viridis)
library(readxl)

# A.data processing
## convert table result to data frame
tableVec2df <- function(name_vector, ntop=NULL){
  ntop=ifelse(is.null(ntop),length(name_vector), ntop)
  tableSort <- head( sort(name_vector, decreasing = T), ntop)
  name <- names(tableSort)
  Frequency <- as.vector(tableSort)
  df <- data.frame(name, Frequency)
  df$name <- factor(df$name, levels=df$name)
  return(df)
}
tableVecs2df <- function(name_df){
  df <- reshape2::melt(name_df)
  return(df)
}

# B. themes
theme_pub <- function(ratio=c(14,14,16,16,10), legend.position = "right"){
  print("Default ratio is main title:2, axis title:1.5, axis text: 2, legend: 2, text:3")
  theme_classic() +
    theme(
      # axis and fig title
      plot.title = element_text(face = "bold",size=ratio[1]),
      axis.title = element_text(face = "bold",size=ratio[2]),
      # axis text and lines
      axis.text.y = element_text(size=ratio[3],color="black"),
      axis.text.x = element_text(size=ratio[3],color="black",angle=45,hjust=1),
      axis.line = element_line(size=rel(0.2),linetype="solid"),
      # legend 
      legend.position = legend.position,
      legend.title = element_text(face = "bold",size=ratio[4]),
      legend.text = element_text(face = "bold",size=ratio[4]),
      # text
      text = element_text(size = ratio[5]),
      plot.margin=unit(c(10,5,5,5),"mm")
      
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

