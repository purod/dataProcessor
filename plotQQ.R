# 1.6. QQplot
var_QQ <- function(mydata, con_cut=5, 
                   nrow=3, ncol=2){
  
  plotData <- mydata %>% 
    # dplyr::select only continuous variables
    dplyr::select_if(sapply(.,function(x){
      is.numeric(x)&length(table(x))>con_cut})) %>% 
    setNames(gsub("_categorical|_binary|_continuous","",names(.))) %>% 
    # log2 tranformation for all variables
    bind_cols(dataTransform(., pValThres=1, transform=T, chooseTransform="Log", 
                            BC.lambda=NULL, rseed=5067)) %>% 
    # sort alphabetically
    dplyr::select(sort(colnames(.)))
  
  # b. QQ plots with QQ lines
  for(chunk in split(1:ncol(plotData),ceiling(seq_along(1:ncol(plotData))/(nrow*ncol)))){
    chunkData <- plotData[, chunk]
    
    par(mfrow=c(nrow, ncol))
    
    for (var in names(chunkData)){
      varData <- chunkData[!is.na(chunkData[, var]), var]
      qqnorm(varData, main = var, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
      qqline(varData, col="red")
    }
  }
}

# if stat_qq_line from ggplot doesn't work
# QQ plot with QQ line
# for(chunk in split(2:ncol(plotData),ceiling(seq_along(2:ncol(plotData))/8))){
#  chunkData <- plotData[, chunk]

#  par(mfrow=c(2,4))

#  for (var in names(chunkData)){
#    varData <- chunkData[!is.na(chunkData[, var]), var]
#    qqnorm(varData, main = var, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
#    qqline(varData, col="red")
#  }
#}