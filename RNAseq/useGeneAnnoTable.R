geneIDTrans <- function(geneList, annoTable, from="Ensembl_ID",to="HGNC_Symbol"){
    message(paste(length(geneList),"genes were provided."))
    match_loc <- match(geneList, annoTable[,from])
    message(paste(sum(is.na(match_loc)),"genes not matched."))
    res <- annoTable[match_loc, to]
    return(res)
}