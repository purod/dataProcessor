
highImpactVariantFromVCF <- function(vcf_file, GenomeVersion="hg19"){ 

    library(VariantAnnotation)
    select=dplyr::select

    vcf <- readVcf(vcf_file, GenomeVersion)
    element_len <- sapply( info(vcf)@listData$CSQ, length )
    # Extract chromosome, position, REF, and ALT from VCF
    chromosome <- rep( as.character(seqnames(rowRanges(vcf))), element_len)  # Chromosome
    position <- rep( start(rowRanges(vcf)), element_len)   # Position
    ref <- rep( as.character(ref(vcf)), element_len)       # Reference allele
    alt <- rep( as.character(unlist(alt(vcf))), element_len)

    # Extract the INFO fields metadata
    Description <- info(header(vcf))$Description
    INFO_names <- strsplit( gsub(".*Format:","",Description), "\\|" )[[1]]
    # Extract INFO fields
    info_fields <- strsplit( info(vcf)@listData$CSQ@unlistData, "\\|") %>% 
        do.call(rbind,.) %>% data.frame
    message("ignore this warning.")
    colnames(info_fields) <- INFO_names

    # Create a data frame with the extracted fields
    extracted_data <- data.frame(
     CHROM = chromosome,
     POS = position,
     REF = ref,
     ALT = alt,
    Consequence = info_fields$Consequence,
    IMPACT = info_fields$IMPACT,
    SYMBOL = info_fields$SYMBOL,
    Gene = info_fields$Gene,
    BIOTYPE = info_fields$BIOTYPE,
    SIFT = info_fields$SIFT, # deleterious
    PolyPhen = info_fields$PolyPhen, # damaging
    CLIN_SIG = info_fields$CLIN_SIG,
    SOMATIC = info_fields$SOMATIC
    )

    # filtering criteria
    impact_variant <- extracted_data %>% 
        filter( IMPACT%in%c("MODERATE", "HIGH") ) %>% 
        select( CHROM, POS, REF, ALT, Gene, SYMBOL) %>% distinct() %>% 
        filter(Gene!="")
    
    return(impact_variant)
}
