
removeBatch_RNA <- function(expNormal, 
                            batch_category_vector=NULL
                            batch_continue=NULL, 
                            covMatrix=NULL, 
                            noCombat=T){
  
  if(!is.null(batch_category_vector) & ){
    expNormal <- limma::removeBatchEffect( expNormal,
                                             covariates=batchMatrix)

  }



  batchMatrix <- batchMatrix %>% 
    mutate_if(is.character,as.factor) %>% 
    data.matrix()
  
  if(noCombat){
    if(is.null(covMatrix)){
      expNormal <- limma::removeBatchEffect( expNormal,
                                             covariates=batchMatrix)
    }else{
      covMatrix <- covMatrix %>% 
        mutate_if(is.character,as.factor) %>% 
        data.matrix()

      expNormal <- limma::removeBatchEffect( expNormal,
                                           covariates=batchMatrix,
                                           design = covMatrix )
    }
  }else{
    nbatch = ncol(batchMatrix)
    
    for(i in 1:nbatch){
      expNormal <- sva::ComBat( expNormal, batch=batchMatrix[,i], mod=covMatrix )
    }
  }
  
  return(expNormal)
}
