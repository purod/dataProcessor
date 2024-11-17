normalize.DEseq2 <- function(in.count.df, matrixDesignType=NULL){
    
    # experimental design
    if ( is.null(matrixDesignType) ){
      colData <- S4Vectors::DataFrame(condition=factor(rep('Pat',ncol(in.count.df))))
      dds <- DESeq2::DESeqDataSetFromMatrix(data.matrix(in.count.df),
                                    colData=colData,design=~1)
    }else{
      colData <- S4Vectors::DataFrame(condition=factor(matrixDesignType))
      dds <- DESeq2::DESeqDataSetFromMatrix(data.matrix(in.count.df),
                                    colData=colData,design=~condition)
    }
    
    # normalization
    if ( is.null(matrixDesignType) ){
      dds1 <- DESeq2::varianceStabilizingTransformation(dds)
    }else{
      message("Running DESeq normalization using blind=FALSE")
      dds1 <- DESeq2::varianceStabilizingTransformation(dds,blind=FALSE)
    }
  
    return(SummarizedExperiment::assay(dds1))
}

