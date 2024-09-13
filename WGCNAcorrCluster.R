corrClusterWGCNA <- function( df.input, excludeVars=NULL, keepVars=c(""), 
                           all.cut=0.95, patternCor=NULL, pattern.cut=0.95, tag="_set" ){
  
  #'  @param all.cut : variables with correlatin above all.cut are clustered
  #'  @param patternCor : regular patterns of variable sets you want to use different correlation cutoffs
  #'  @param pattern.cut : variables matching patternCor with correlatin above pattern.cut are clustered
  
  # minimum correlation we need to calculate
  cor.min = min(c(all.cut, pattern.cut))
  
  # input format  
  if(is.null(excludeVars)){
    
    df <- df.input %>% mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }else{
    
    df <- df.input %>% dplyr::select(-one_of(excludeVars)) %>% 
      mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }
  
  # use fred's function to speed up the calculation
  topCorrelations <- WGCNA::cor(df, use = "pairwise.complete.obs")
  
  # Get the upper triangle of the correlation matrix, excluding the diagonal
  topCorrelations[lower.tri(topCorrelations, diag = TRUE)] <- NA
  # Convert the upper triangle matrix to a long format data frame
  topCorrelations <- as.data.frame(as.table(topCorrelations)) %>%
    filter(!is.na(Freq))  # Remove NA values
  # Rename columns for clarity
  colnames(topCorrelations) <- c("Var1", "Var2", "Cor")
  topCorrelations <- topCorrelations %>% arrange(desc(Cor)) 
  
  # variable pairs with cor > cutoff
  if( !is.null(topCorrelations) ){
    
    # print(knitr::kable(head(cor.table),row.names=F))
    # cat('\n')
    
    # Group correlated variables...  
    if(is.null(patternCor)){
      cor.table <- topCorrelations %>% filter( abs(Cor) > all.cut )
    }else{
      cor.table.comm <- topCorrelations %>% filter( abs(Cor) > all.cut )
      cor.table.patt <- lapply(1:length(patternCor), function(x){
        cor.table.x <- topCorrelations %>% 
          filter( grepl(patternCor[x],Var1) &
                    grepl(patternCor[x],Var2) &
                    ( (abs(Cor) > pattern.cut[x]) & (abs(Cor)<=all.cut) )
          ) 
      }) %>% do.call(rbind,.)
      cor.table <- rbind(cor.table.comm, cor.table.patt)
    }
    
    # construct the correlation matrix for clustering
    cor.table.rev = cor.table
    colnames(cor.table.rev) = c("Var2","Var1","Cor")
    cor.pre = spread(rbind(cor.table,cor.table.rev) %>% 
                       mutate(Cor=abs(Cor)),key=Var2, value=Cor, fill=0) %>% 
      column_to_rownames(var="Var1") %>% as.matrix()
    un1 <- unique(c(colnames(cor.pre), rownames(cor.pre)))
    cor.vars <- matrix(0, length(un1), length(un1), dimnames = list(un1, un1))
    cor.vars[rownames(cor.pre), colnames(cor.pre)] <- cor.pre
    cor.vars = as.data.frame(cor.vars)
    
    #cor.vars = cor(df %>% select(one_of(unique(c(cor.table$Var1,cor.table$Var2)))))
    
    # clustering
    cor.class <- as.dist(1 - cor.vars) %>% hclust(.,method="complete") %>% 
      cutree(.,h=(1-cor.min))
    
    groups.cor = split(names(cor.class), cor.class)
    groups.cor.varnames = groups.cor[lapply(groups.cor, function(x){length(x)>1}) %>% unlist]
    names(groups.cor.varnames)=1:length(groups.cor.varnames)
    
    message(paste0('Identified ', nrow(cor.table), 
                   ' pairs of correlated variables',
                   '. There are in total ', length(unique(unlist(groups.cor.varnames))),
                   ' unique variables, which can be grouped by ', 
                   length(groups.cor.varnames), ' clusters:\n\n',
                   paste(lapply(1:length(groups.cor.varnames), 
                                function(x){paste0("set ", x, ": ", 
                                                   paste0(groups.cor.varnames[[x]], 
                                                          collapse=", "))}), 
                         collapse = "\n")
    ))
    
    # keep only one variable from each cluster
    corr.vars.drop = setdiff(unique(unlist(groups.cor.varnames)), 
                             sapply(groups.cor.varnames,function(x){
                               ifelse(length(intersect(keepVars, x))>=1,
                                      intersect(keepVars, x)[1], x[1])}))
    
    names(groups.cor.varnames)=unname(sapply(groups.cor.varnames,function(x){
      ifelse(length(intersect(keepVars, x))>=1,
             intersect(keepVars, x)[1], x[1])}))
    
    drop.cor=list()
    drop.cor$vars = corr.vars.drop # list of variables to be dropped
    drop.cor$relation = groups.cor.varnames # relationship among variables
    kept.names = names(drop.cor$relation)
    if(grepl("_continuous|_binary|_categorical",kept.names[1])){
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        gsub( "(_continuous|_binary|_categorical)$", 
              paste0(tag,setid,"\\1"), kept.names[setid] )})
    }else{
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        paste0( kept.names[setid],tag,setid )   })
    }
    names(drop.cor$setName) = kept.names
    
    return(drop.cor)
  }
  message(paste0('Identified ', 0,
                 ' pairs of correlated variables with r > ',corr.cut))
  cat('\n')
}

