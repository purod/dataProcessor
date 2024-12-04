# 1.5. barplot and density plot
# library(cowplot)
var_dist <- function(mydata,col_id,box_var=NULL,con_cut=15,
                     nrow=3,ncol=3,title_rel=1,text_rel=0.8){
  
  
  # continuous variables suitable for histogram
  hist_var = names(mydata)[sapply(mydata,function(x){
    is.numeric(x)&length(table(x))<con_cut})]
  # continuous variables suitable for density plot
  den_var = names(mydata)[sapply(mydata,function(x){
    is.numeric(x)&length(table(x))>=con_cut})]
  
  my_plots <- lapply(names(mydata[,col_id]), function(var_x){
    
    p <- ggplot(mydata[,col_id]) +
      aes_string(var_x)
    
    if(var_x%in%box_var){
      p <- p + geom_boxplot(na.rm = TRUE) 
    }else if(var_x%in%den_var){
      p <- p + geom_density(na.rm = TRUE)
    }else if(var_x%in%hist_var){
      p <- p + geom_bar(na.rm = TRUE)+
        scale_x_continuous()
    }else{
      p <- p + geom_bar(na.rm = TRUE) +
        scale_x_discrete(na.translate = FALSE,label=abbreviate)
    } 
    
    p <- p + 
      theme(axis.title = element_text(size=rel(title_rel),face="bold"),
            axis.text = element_text(size=rel(text_rel))
      )
  })
  
  plot_grid(plotlist = my_plots,nrow=nrow,ncol=ncol,labels="auto")
  
}