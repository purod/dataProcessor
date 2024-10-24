## reference paper: A Multi-Marker Genetic Association Test Based on the Rasch Model Applied to Alzheimer’s Disease 

rm(list = ls(all = T))
chrom=as.numeric((commandArgs(TRUE)))
print(chrom)

#library(mirt, lib.loc='/users/rdas/R/x86_64-pc-linux-gnu-library/3.3/')
library(mirt, lib.loc='/home/jtu/R/x86_64-pc-linux-gnu-library/3.6/')
library(data.table)
library(impute, lib.loc='/home/jtu/R/x86_64-pc-linux-gnu-library/3.6/')
library(plyr)


proj.dir = "/projects/AnswerALS/Data/wgs_sep/plink" ### change this for the ADNI


geneSet=data.table::fread(paste0(proj.dir, "/Genesets/common.genesets.chr",chrom,".QC.LD08.set.table"),header=T,data.table=F)
Snps=data.table::fread(paste0(proj.dir,"/common/chr",chrom,".common.exclude.rare.known.variants.maf.QC.LD08.output.raw"),head=T,data.table=F)


# geneSet=data.table::fread(paste0(proj.dir, "/Genesets/rare.genesets.chr",chrom,".QC.LD08.set.table"),header=T,data.table=F)
# Snps=data.table::fread(paste0(proj.dir,"/coding_rare/chr",chrom,"rare.known.variants.maf.QC.LD08.output.raw"),head=T,data.table=F)


snpList1=colnames(Snps)
rownames(Snps)<-Snps[,c("FID")]

geneList1=colnames(geneSet)
geneList=geneList1[4:length(geneList1)]


snps1=Snps[,-c(1:6) ]

##1. convert to a numeric matrix
snpsNumeric = as.matrix(snps1)
snpsNumeric = data.matrix(snpsNumeric)
snpsNumeric = apply(snpsNumeric, 2, function(x) { as.numeric(x) })

##2. drop snps with high missingness
cNa = apply(snpsNumeric, 2, function(x) { length(which(is.na(x))) } )
dropIdx = which(cNa > 0.05 * nrow(snpsNumeric))
if (length(dropIdx) > 0) {
  snpsNumeric = snpsNumeric[, -dropIdx]
}

#impute
imputed = impute.knn(t(snpsNumeric), k = 5)
imputedSnps = imputed$data
colnames(imputedSnps) = rownames(Snps)
imputedSnps = round(imputedSnps)
finalSnps=t(imputedSnps)
newnames = unname(sapply(colnames(finalSnps), function(x) paste(head(unlist(strsplit(x,'_')),4),collapse='_')))
newnames = unname(sapply(newnames, function(x) gsub('X','',x)))
colnames(finalSnps) = newnames

df = list()
geneyes = c()
for (i in 1:length(geneList)) {
  print(paste0(i,"/",length(geneList)) )
  gene=geneList[[i]]
  index=i+3
  genesnps=as.character(geneSet[which(geneSet[,c(index)]==1),1])
  # print(genesnps)
  if (length(genesnps)<=1) {next}
  
  #tmp_genesnps = paste(genesnps,collapse="|")
  #geno=as.data.frame(finalSnps[, grep(tmp_genesnps, colnames(finalSnps))])
  modified_name = sapply(colnames(finalSnps), function(dd){paste(unlist(strsplit(dd, "_", fixed = TRUE))[c(1,2)],collapse="_")})    
  geno=as.data.frame(finalSnps[, which(modified_name %in% genesnps)])
  # print(dim(geno))
  
  ### This part has been updated - check my comment on the confluence page
  ### https://gnshealthcare.atlassian.net/wiki/spaces/NUM/pages/96436394/Multi-marker+testing+for+platform+incorporation
  tc_out <-tryCatch(
    { rasch.model=mirt(geno,1,itemtype="Rasch",verbose=FALSE) ### use this - this is right
    #score=personfit(rasch.model) ### this is wrong
    score=as.numeric(fscores(rasch.model, IRTpars=TRUE)) ### use this
    # score=as.numeric(fscores(rasch.model))
    }
    ,error=function(err) NULL
  )
  if (is.null(tc_out)) {next}
  #df=cbind(df,score)
  #colnames(df)[colnames(df) == "Zh"] <- gene
  #df[[i]] = score[,ncol(score)]
  df[[i]] = score
  
  geneyes = c(geneyes,gene)
  
  
}
df = do.call(cbind,df)
colnames(df) = geneyes
rownames(df) = rownames(finalSnps)

save(df,file=paste0(proj.dir, "/Rasch_score/common/Rasch_scores_mirt_chr",chrom,".QC.RData"))
