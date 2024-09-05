ensemble2entrezID <- function(ensembleID){

	library("AnnotationDbi")
	library("org.Hs.eg.db")
	#columns(org.Hs.eg.db) # returns list of available keytypes
	entrezID = mapIds(org.Hs.eg.db,
                keys=ensembleID, #Column containing Ensembl gene ids
                column="ENTREZID",
                keytype="ENSEMBL",
                multiVals="first")
}
