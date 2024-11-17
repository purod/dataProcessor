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

