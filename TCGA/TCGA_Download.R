library(TCGAbiolinks, lib.loc = "~/R/TCGAbiolinks")
library(tidyverse)
source("TCGA_download_utils.R")

cancer = "TCGA-SKCM"
# cancer = "CPTAC-3"
typesample = "TP" #TB (primary blood) or TBM (primary bone marrow) for blood cancer, TP for solid tumor

fileDir <- paste0("/projects/tcga/", cancer, "/")
if (!dir.exists(fileDir)){
    dir.create(fileDir)
}

# get available data categories
temp <- TCGAbiolinks::getDataCategorySummary(project = cancer)
available_cat <- colnames(temp)

#### Download and save raw clinical data ####
if ("Clinical" %in% available_cat){
    clinic1 <- retry(
        GDCquery(project = cancer, data.category = "Clinical", data.type = "Clinical Supplement", data.format = "BCR XML")
    )
    #download compressed data
    retry(
        GDCdownload(clinic1, directory = paste0(fileDir, "raw"))
    )
    
    #prepare data
    clinic.patient <- GDCprepare_clinic(clinic1, "patient", directory = paste0(fileDir, "raw")) %>% unique() # remove duplicated rows
    clinic.admin <- GDCprepare_clinic(clinic1, "admin", directory = paste0(fileDir, "raw"))
    clinic.followup <- GDCprepare_clinic(clinic1, "follow_up", directory = paste0(fileDir, "raw"))
    clinic.drug <- GDCprepare_clinic(clinic1, "drug", directory = paste0(fileDir, "raw"))
    clinic.radiation <- GDCprepare_clinic(clinic1, "radiation", directory = paste0(fileDir, "raw"))
    clinic.stage <- GDCprepare_clinic(clinic1, "stage_event", directory = paste0(fileDir, "raw"))
    clinic.tumor <- GDCprepare_clinic(clinic1, "new_tumor_event", directory = paste0(fileDir, "raw"))
    # clinic.indexed <- GDCquery_clinic(project = cancer, type = "clinical")
    
    #get subtypes
    if (grepl("TCGA", cancer)) {
        subtypes <- PanCancerAtlas_subtypes() %>%
            filter(cancer.type == sub("TCGA-", "", cancer))
    }
    samplesTP <- TCGAquery_SampleTypes(barcode = subtypes$pan.samplesID, typesample = typesample)
    # subtypes_primary <- subtypes %>%
    #     filter(pan.samplesID %in% samplesTP)
    inter_save_dir <- paste0(fileDir, "inter/clinical/")
    if (!dir.exists(inter_save_dir)){
        dir.create(inter_save_dir, recursive = TRUE)
    }
    
    if (!is.null(clinic.patient)) {write_csv(clinic.patient, paste0(inter_save_dir, "clinic_patient.csv"))}
    if (!is.null(clinic.admin)) {write_csv(clinic.admin, paste0(inter_save_dir, "clinic_admin.csv"))}
    if (!is.null(clinic.followup)) {write_csv(clinic.followup, paste0(inter_save_dir, "clinic_followup.csv"))}
    if (!is.null(clinic.drug)) {write_csv(clinic.drug, paste0(inter_save_dir, "clinic_drug.csv"))}
    if (!is.null(clinic.radiation)) {write_csv(clinic.radiation, paste0(inter_save_dir, "clinic_radiation.csv"))}
    if (!is.null(clinic.stage)) {write_csv(clinic.stage, paste0(inter_save_dir, "clinic_stage.csv"))}
    if (!is.null(clinic.tumor)) {write_csv(clinic.tumor, paste0(inter_save_dir, "clinic_tumor.csv"))}
    if (!is.null(clinic.indexed)) {write_csv(clinic.indexed, paste0(inter_save_dir, "clinic_indexed.csv"))}
    if (!is.null(subtypes)) {write_csv(subtypes, paste0(inter_save_dir, "clinic_subtypes.csv"))}
    # if (!is.null(subtypes_primary)) {write_csv(subtypes_primary, paste0(inter_save_dir, "clinic_subtypes_primarytumor.csv"))}
}

#### Download and save raw RNA data ####
if ("Transcriptome Profiling" %in% available_cat){
    query.gene <- retry(
        GDCquery(
            project = cancer, data.category = "Transcriptome Profiling",
            data.type = "Gene Expression Quantification", workflow.type = "STAR - Counts"
        )
    )
    query.mirna <- retry(
        GDCquery(
            project = cancer, data.category = "Transcriptome Profiling",
            data.type = "miRNA Expression Quantification"
        )
    )
    
    #download compressed data
    GDCdownload(query.gene, directory = paste0(fileDir, "raw"))
    GDCdownload(query.mirna, directory = paste0(fileDir, "raw"))
    
    #prepare data
    data.gene <- GDCprepare(query.gene, directory = paste0(fileDir, "raw"))
    data.mirna <- GDCprepare(query.mirna, directory = paste0(fileDir, "raw"))
    
    #filter assay for unstranded and primary tumor
    samplesTP <- TCGAquery_SampleTypes(barcode = colnames(data.gene), typesample = typesample)
    gene_counts <- SummarizedExperiment::assay(data.gene, "unstranded") %>%
        as.data.frame %>%
        rownames_to_column("Gene") %>%
        relocate(Gene)
    gene_tpm <- SummarizedExperiment::assay(data.gene, "tpm_unstrand") %>%
        as.data.frame %>%
        rownames_to_column("Gene") %>%
        relocate(Gene)
    gene_counts_tp <- SummarizedExperiment::assay(data.gene, "unstranded") %>%
        as.data.frame %>%
        select(any_of(samplesTP)) %>%
        rownames_to_column("Gene") %>%
        relocate(Gene)
    gene_tpm_tp <- SummarizedExperiment::assay(data.gene, "tpm_unstrand") %>%
        as.data.frame %>%
        select(any_of(samplesTP)) %>%
        rownames_to_column("Gene") %>%
        relocate(Gene)
    
    #filter mirna for primary tumor
    samplesTP <- TCGAquery_SampleTypes(barcode = colnames(data.mirna), typesample = typesample)
    data.mirna.filt <- data.mirna[, c("miRNA_ID", samplesTP)]
    
    inter_save_dir <- paste0(fileDir, "inter/rna/")
    if (!dir.exists(inter_save_dir)){
        dir.create(inter_save_dir)
    }
    saveRDS(data.gene, file = paste0(inter_save_dir, "RNA_SummaryExperiment.rds"))
    if (!is.null(gene_counts)) {write_csv(gene_counts, paste0(inter_save_dir, "RNA_counts_unstranded.csv"))}
    if (!is.null(gene_tpm)) {write_csv(gene_tpm, paste0(inter_save_dir, "RNA_tpm_unstranded.csv"))}
    if (!is.null(gene_counts_tp)) {write_csv(gene_counts_tp, paste0(inter_save_dir, "RNA_counts_primarytumor_unstranded.csv"))}
    if (!is.null(gene_tpm_tp)) {write_csv(gene_tpm_tp, paste0(inter_save_dir, "RNA_tpm_primarytumor_unstranded.csv"))}
    if (!is.null(data.mirna)) {write_csv(data.mirna, paste0(inter_save_dir, "miRNA_reads.csv"))}
    if (!is.null(data.mirna.filt)) {write_csv(data.mirna.filt, paste0(inter_save_dir, "miRNA_reads_primarytumor.csv"))}
}

if ("DNA Methylation" %in% available_cat){
    query.meth.intensities <- retry(
        GDCquery(
            project = cancer, data.category = "DNA Methylation",
            data.type = "Masked Intensities"
        )
    )
    query.meth.beta <- retry(
        GDCquery(
            project = cancer, data.category = "DNA Methylation",
            data.type = "Methylation Beta Value"
        )
    )
    
    #download compressed data
    GDCdownload(query.meth.intensities, directory = paste0(fileDir, "raw"))
    GDCdownload(query.meth.beta, directory = paste0(fileDir, "raw"))
    
    #prepare data
    data.meth.intensities <- GDCprepare(query.meth.intensities, directory = paste0(fileDir, "raw"))
    data.meth.beta <- GDCprepare(query.meth.beta, directory = paste0(fileDir, "raw"))
    
    #filter meth
    samplesTP <- TCGAquery_SampleTypes(barcode = colnames(data.meth.intensities), typesample = typesample)
    data.meth.intensities.TP <- data.meth.intensities[, samplesTP]
    
    samplesTP <- TCGAquery_SampleTypes(barcode = colnames(data.meth.beta), typesample = typesample)
    data.meth.beta.TP <- data.meth.beta[, samplesTP]
    

    inter_save_dir <- paste0(fileDir, "inter/methylation")
    if (!dir.exists(inter_save_dir)){
        dir.create(inter_save_dir)
    }
    
    saveRDS(data.meth.intensities, file = paste0(inter_save_dir, "/Meth_Intensities.rds"))
    saveRDS(data.meth.intensities.TP, file = paste0(inter_save_dir, "/Meth_Intensities_primarytumor.rds"))
    saveRDS(data.meth.beta, file = paste0(inter_save_dir, "/Meth_Beta.rds"))
    saveRDS(data.meth.beta.TP, file = paste0(inter_save_dir, "/Meth_Beta_primarytumor.rds"))

}
#### Download and save raw CNV data ####
if ("Copy Number Variation" %in% available_cat){
    query.cnv.gene <- retry(
        GDCquery(
            project = cancer, data.category = "Copy Number Variation",
            data.type = "Gene Level Copy Number"
        )
    )
    query.cnv.allele <- retry(
        GDCquery(
            project = cancer, data.category = "Copy Number Variation",
            data.type = "Allele-specific Copy Number Segment"
        )
    )
    query.cnv.segment <- retry(
        GDCquery(
            project = cancer, data.category = "Copy Number Variation",
            data.type = "Copy Number Segment"
        )
    )
    query.cnv.segment.masked <- retry(
        GDCquery(
            project = cancer, data.category = "Copy Number Variation",
            data.type = "Masked Copy Number Segment"
        )
    )
    
    GDCdownload(query.cnv.gene, directory = paste0(fileDir, "raw"))
    GDCdownload(query.cnv.allele, directory = paste0(fileDir, "raw"))
    GDCdownload(query.cnv.segment, directory = paste0(fileDir, "raw"))
    GDCdownload(query.cnv.segment.masked, directory = paste0(fileDir, "raw"))
    
    temp <- query.cnv.gene$results[[1]] %>%
        arrange(desc(analysis_workflow_type)) 
    temp <- temp[!duplicated(temp$sample.submitter_id), ]
    query.cnv.gene$results[[1]] <- temp
    
    temp <- query.cnv.allele$results[[1]] %>%
        arrange(desc(analysis_workflow_type)) 
    temp <- temp[!duplicated(temp$sample.submitter_id), ]
    query.cnv.allele$results[[1]] <- temp
    
    data.cnv.gene <- GDCprepare(query.cnv.gene, directory = paste0(fileDir, "raw"))
    data.cnv.gene.noexp <- GDCprepare(query.cnv.gene, directory = paste0(fileDir, "raw"), summarizedExperiment = FALSE)
    data.cnv.allele <- GDCprepare(query.cnv.allele, directory = paste0(fileDir, "raw"))
    data.cnv.segment <- GDCprepare(query.cnv.segment, directory = paste0(fileDir, "raw"))
    data.cnv.segment.masked <- GDCprepare(query.cnv.segment.masked, directory = paste0(fileDir, "raw"))
    
    samplesTP <- TCGAquery_SampleTypes(barcode = colnames(data.cnv.gene), typesample = typesample)
    cnv_gene_copy_number <- SummarizedExperiment::assay(data.cnv.gene, "copy_number") %>%
        as.data.frame() %>%
        rownames_to_column("Gene") %>%
        relocate(Gene)
    cnv_gene_copy_number_primarytumor <- SummarizedExperiment::assay(data.cnv.gene, "copy_number") %>%
        as.data.frame() %>%
        select(any_of(samplesTP)) %>%
        rownames_to_column("Gene") %>%
        relocate(Gene)
    
    inter_save_dir <- paste0(fileDir, "inter/cnv/")
    if (!dir.exists(inter_save_dir)){
        dir.create(inter_save_dir)
    }
    
    saveRDS(data.cnv.gene, paste0(inter_save_dir, "cnv_gene.rds"))
    write_csv(cnv_gene_copy_number, paste0(inter_save_dir, "cnv_gene_copy_number.csv"))
    write_csv(cnv_gene_copy_number_primarytumor, paste0(inter_save_dir, "cnv_gene_copy_number_primarytumor.csv"))
    samplesTP <- TCGAquery_SampleTypes(barcode = colnames(data.cnv.gene), typesample = typesample)
    saveRDS(data.cnv.gene[, samplesTP], paste0(inter_save_dir, "cnv_gene_primarytumor.rds"))
    
    write_csv(data.cnv.allele, paste0(inter_save_dir, "cnv_allele.csv"))
    samplesTP <- TCGAquery_SampleTypes(barcode = data.cnv.allele$Sample, typesample = typesample)
    write_csv(data.cnv.allele[data.cnv.allele$Sample %in% samplesTP, ], paste0(inter_save_dir, "cnv_allele_primarytumor.csv"))
    
    write_csv(data.cnv.segment, paste0(inter_save_dir, "cnv_segment.csv"))
    samplesTP <- TCGAquery_SampleTypes(barcode = data.cnv.segment$Sample, typesample = typesample)
    write_csv(data.cnv.segment[data.cnv.segment$Sample %in% samplesTP, ], paste0(inter_save_dir, "cnv_segment_primarytumor.csv"))
    
    write_csv(data.cnv.segment.masked, paste0(inter_save_dir, "cnv_segment_masked.csv"))
    samplesTP <- TCGAquery_SampleTypes(barcode = data.cnv.segment.masked$Sample, typesample = typesample)
    write_csv(data.cnv.segment.masked[data.cnv.segment.masked$Sample %in% samplesTP,], paste0(inter_save_dir, "cnv_segment_masked_primarytumor.csv"))
}
#### Download and save raw SNV data ####
if ("Simple Nucleotide Variation" %in% available_cat){
    query.snv <- retry(
        GDCquery(
            project = cancer, data.category = "Simple Nucleotide Variation",
            data.type = "Masked Somatic Mutation"
        )
    )
    
    #download compressed data
    GDCdownload(query.snv, directory = paste0(fileDir, "raw"))
    
    #prepare data
    data.snv <- GDCprepare(query.snv, directory = paste0(fileDir, "raw"))
    
    samplesTP <- TCGAquery_SampleTypes(barcode = unique(data.snv$Tumor_Sample_Barcode), typesample = typesample)
    data.snv.filt <- data.snv[data.snv$Tumor_Sample_Barcode %in% samplesTP, ]
    
    #save data
    inter_save_dir <- paste0(fileDir, "inter/snv/")
    if (!dir.exists(inter_save_dir)){
        dir.create(inter_save_dir)
    }
    
    write_csv(data.snv, file = paste0(inter_save_dir, "snv_gene.csv"))
    write_csv(data.snv.filt, file = paste0(inter_save_dir, "snv_gene_primarytumor.csv"))
}
#### Download and save raw protein data ####
if ("Proteome Profiling" %in% available_cat) {
    query.prot <- retry(
        GDCquery(
            project = cancer, data.category = "Proteome Profiling",
            data.type = "Protein Expression Quantification"
        )
    )
    
    #download raw data
    GDCdownload(query.prot, directory = paste0(fileDir, "raw"))
    
    #prepare data
    data.prot <- GDCprepare(query.prot, directory = paste0(fileDir, "raw"))
    
    #primary tumor samples
    samplesTP <- TCGAquery_SampleTypes(barcode = colnames(data.prot), typesample = typesample)
    data.prot.filt <- cbind(data.prot[, 1:5], data.prot[, samplesTP])
    
    #save data
    inter_save_dir <- paste0(fileDir, "inter/protein/")
    if (!dir.exists(inter_save_dir)){
        dir.create(inter_save_dir)
    }
    
    write_csv(data.prot, file = paste0(inter_save_dir, "protein_expression.csv"))
    write_csv(data.prot.filt, file = paste0(inter_save_dir, "protein_expression_primarytumor.csv"))
}
