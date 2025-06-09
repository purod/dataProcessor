filter_rnaseq_count <- function(dfcount, max_value=10){

  # genes as columns
  if( nrow(dfcount) > ncol(dfcount) ){
	      message("Transpose the data frame")
      dfcount <- t(dfcount)
        }
    
    dfcount <- as.data.frame(dfcount)
    Npat = nrow(dfcount)
      genevars = colnames(dfcount)

      message("Get statistics of counts")
        ## remove zero variance genes
        genevar = unlist(plyr::colwise(function(cc)var(cc))(dfcount))
        genes_zerovar = names(genevar)[genevar == 0]
	  toremove = genes_zerovar
	  ## percentage of zeros
	  numzero = unlist(plyr::colwise(function(cc){
						     sum(cc == 0)
						       })(dfcount))
	    ## maximum count
	    maxcount = unlist(plyr::colwise(function(cc){
						        max(cc)
							  })(dfcount))
	    ## 95% quantile of the nonzero values
	    qcount95nz = unlist(plyr::colwise(function(cc){
						         as.numeric(quantile(cc[cc > 0], 0.95))
							   })(dfcount))
	      message("select genes to filter")
	      #####################################
	      ## Start selecting genes to filter ##
	      #####################################
	      ##
	      ## - Remove genes with less than 10 counts
	      ##   - Common empirical thershold:
	      ##     - https://www.researchgate.net/post/In_differential_expression_analysis_why_are_sequences_with_less_than_10_reads_filtered_out
	      ##   - Need to transform back the rnaseq to counts. 
	      ##     - Even vst is essentially in log2 scale asymptotically for large numbers:
	      ##       - From variaceStabilizingTransformation help: "In all cases, the transformation is scaled such that for larger
	      ##         counts, it becomes asymptotically (for large values) equal to the
	      ##         logarithm to base 2 of normalized counts."
	      ##       - Clearly this does not apply to small counts. For this reason decided to require the raw counts.
	      genes_less10counts = genevars[which(maxcount< max_value)]
	        genes_less10counts = setdiff(genes_less10counts, toremove)
	        toremove = c(toremove, genes_less10counts)
		  ##
		  ## Remove genes where 
		  ## - the 95% quantile of patients with  nonzero expression is less than 10 and
		  ## - at least 50% of patients have zero expression
		  genes_95qless10counts_50pzero = setdiff(genevars[which(qcount95nz< max_value & numzero/Npat > 0.50)],
							                                            toremove)
		  toremove = c(toremove, genes_95qless10counts_50pzero)
		    ##
		    ## Remove genes where:
		    ## - 90% of patients have zero expression
		    genes_90pzero = setdiff(genevars[which(numzero / Npat > 0.9)], toremove)
		    toremove = c(toremove, genes_90pzero)
		      
		      df <- dfcount[,!colnames(dfcount)%in%toremove]
		      
		        dfhighExpre <- t(df)

			  attributes(dfhighExpre)$filtered_genes = rbind(
									     tibble(
										          Filtered_genes = genes_less10counts,
											       Reason = "Maximum count over patients is less than 10"
											     ),
									     tibble(
										          Filtered_genes = genes_95qless10counts_50pzero,
											        Reason = "the 95% quantile of patients with nonzero expression is less than 10 and at least 50% of patients have zero expression"
											     ),
									    tibble(
										         Filtered_genes = genes_90pzero,
											       Reason = "90% of patients have zero expression"
											    )
									      )

			  return(dfhighExpre)
}

filter_tpm <- function(df_tpm, min_tpm = 1, min_samples = 0.2, log_filtered = TRUE) {
  # ----------------------------------------
  # INPUT:
  #   df_tpm       : A data.frame or matrix of TPM values (genes x samples or samples x genes)
  #   min_tpm      : TPM threshold to consider a gene "expressed" (default: 1)
  #   min_samples  : Minimum fraction of samples where gene must be expressed (default: 0.2 = 20%)
  #   log_filtered : If TRUE, return a list with filtered data and gene log
  #
  # OUTPUT:
  #   If log_filtered == FALSE: Filtered expression matrix (genes x samples)
  #   If log_filtered == TRUE : List with:
  #     - $exprs_filtered: filtered TPM matrix
  #     - $filtered_genes: data.frame of genes removed and reasons
  # ----------------------------------------

  # Step 1: Ensure input is a data.frame
  if (!is.data.frame(df_tpm)) df_tpm <- as.data.frame(df_tpm)

  # Step 2: Check orientation: genes should be rows
  transpose_flag <- FALSE
  if (nrow(df_tpm) < ncol(df_tpm)) {
    message("Transposing: assuming genes are in columns, samples in rows.")
    df_tpm <- t(df_tpm)
    transpose_flag <- TRUE
  }

  # Step 3: Basic QC checks
  if (any(is.na(df_tpm))) warning("NA values detected in TPM matrix.")
  if (!is.numeric(as.matrix(df_tpm))) stop("Input must be numeric (TPM values).")

  # Step 4: Define expression threshold per gene
  nsamples <- ncol(df_tpm)
  gene_pass <- apply(df_tpm, 1, function(x) {
    sum(x >= min_tpm) >= min_samples * nsamples
  })

  # Step 5: Split into kept and removed genes
  df_filtered <- df_tpm[gene_pass, , drop = FALSE]
  genes_removed <- rownames(df_tpm)[!gene_pass]

  # Step 6: Return output
  if (transpose_flag) df_filtered <- t(df_filtered)

  if (!log_filtered) {
    return(df_filtered)
  } else {
    log_df <- data.frame(
      Gene = genes_removed,
      Reason = paste0("TPM < ", min_tpm, " in more than ", round((1 - min_samples) * 100), "% of samples")
    )
    return(list(
      exprs_filtered = df_filtered,
      filtered_genes = log_df
    ))
  }
}


