removeBatch_RNA <- function(expNormal, batchMatrix, covMatrix=NULL, noCombat=T){
  
  if(noCombat){
    if(is.null(covMatrix)){
      expNormal <- limma::removeBatchEffect( expNormal,
                                             covariates=batchMatrix)
    }else{
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
