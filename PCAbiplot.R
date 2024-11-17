biplotByGroup <- function(df, groups=NULL, show_outliers=F, legendPos="top", ellipse=TRUE){
  
  ir.pca <- prcomp(df, center = TRUE, scale. = TRUE)
  
  # 2D-PC plot with package rgl
  if(!is.null(groups)){
    g <- ggbiplot::ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                            group = groups %>% as.character,
                            ellipse = ellipse, circle = F,
                            var.axes = F)
  }else{
    g <- ggbiplot::ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                            ellipse = ellipse, circle = F,
                            var.axes = F)
  }
  if(show_outliers){
    U <- ir.pca$x[,1:2] %>% data.frame
    outlier_ID <- apply(U, 2, function(x) which( abs(x - mean(x)) > (3 * sd(x)) )) %>% 
      Reduce(union, .)
    print(rownames(U)[outlier_ID])
    
    g <- g + geom_point(data=U[outlier_ID,], aes(x=PC1, y=PC2), colour="red", size=1)
  } 
 
  # g <- g + scale_color_discrete(name = '')
  g <- g + theme( legend.position = legendPos)
  g <- g + theme(panel.background = element_blank()) # remove the grey background
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove the grid
  g<- g + theme(axis.line.x = element_line(color="black", size = 0.5),
                axis.line.y = element_line(color="black", size = 0.5)) 
  return(g)
}

# Define the custom function
plot_variance_explained <- function(df, ncp = 10, add_labels = TRUE) {
  # ncp: Number of principal components to display (default is 10)
  # add_labels: Whether to add labels showing the percentage of variance explained
  
  pca_result <- prcomp(df, center = TRUE, scale. = TRUE)

  # Plot the explained variance using fviz_eig
  g <- factoextra::fviz_eig(pca_result, 
           choice = "variance",  # Plot variance
           ncp = ncp,            # Number of components to show
           addlabels = add_labels, # Add percentage labels to bars
           barfill = "steelblue",  # Color of bars
           barcolor = "black",     # Border color of bars
           linecolor = "red")      # Color of cumulative variance line
  return(g)
}

# Example usage with PCA from prcomp
# Assume you have a dataset called 'data' and you have performed PCA on it.


