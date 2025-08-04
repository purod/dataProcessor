
# highImpactVariantFromVCF <- function(vcf_file, GenomeVersion="hg19"){ 

#     library(VariantAnnotation)
#     select=dplyr::select

#     vcf <- readVcf(vcf_file, GenomeVersion)
#     element_len <- sapply( info(vcf)@listData$CSQ, length )
#     # Extract chromosome, position, REF, and ALT from VCF
#     chromosome <- rep( as.character(seqnames(rowRanges(vcf))), element_len)  # Chromosome
#     position <- rep( start(rowRanges(vcf)), element_len)   # Position
#     ref <- rep( as.character(ref(vcf)), element_len)       # Reference allele
#     alt <- rep( as.character(unlist(alt(vcf))), element_len)

#     # Extract the INFO fields metadata
#     Description <- info(header(vcf))$Description
#     INFO_names <- strsplit( gsub(".*Format:","",Description), "\\|" )[[1]]
#     # Extract INFO fields
#     info_fields <- strsplit( info(vcf)@listData$CSQ@unlistData, "\\|") %>% 
#         do.call(rbind,.) %>% data.frame
#     message("ignore this warning.")
#     colnames(info_fields) <- INFO_names

#     # Create a data frame with the extracted fields
#     extracted_data <- data.frame(
#      CHROM = chromosome,
#      POS = position,
#      REF = ref,
#      ALT = alt,
#     Consequence = info_fields$Consequence,
#     IMPACT = info_fields$IMPACT,
#     SYMBOL = info_fields$SYMBOL,
#     Gene = info_fields$Gene,
#     BIOTYPE = info_fields$BIOTYPE,
#     SIFT = info_fields$SIFT, # deleterious
#     PolyPhen = info_fields$PolyPhen, # damaging
#     CLIN_SIG = info_fields$CLIN_SIG,
#     SOMATIC = info_fields$SOMATIC
#     )

#     # filtering criteria
#     impact_variant <- extracted_data %>% 
#         filter( IMPACT%in%c("MODERATE", "HIGH") ) %>% 
#         select( CHROM, POS, REF, ALT, Gene, SYMBOL) %>% distinct() %>% 
#         filter(Gene!="")
    
#     return(impact_variant)
# }


highImpactSNVFromVCF <- function(vcf_file,
                                     GenomeVersion = "hg19",
                                     impact_levels = c("MODERATE", "HIGH"),
                                     require_protein_coding = TRUE,
                                     max_af = 0.01,
                                     sift_filter = c("deleterious"),
                                     polyphen_filter = c("possibly_damaging", "probably_damaging")) {
  library(VariantAnnotation)
  library(dplyr)
  library(stringr)

  message("📥 Reading VCF file...")
  vcf <- readVcf(vcf_file, GenomeVersion)

  # Parse CSQ header format
  csq_header <- info(header(vcf))["CSQ", "Description"]
  csq_format <- strsplit(gsub(".*Format: ", "", csq_header), "\\|")[[1]]
  n_cols <- length(csq_format)

  # Split and pad CSQ values
  csq_data <- strsplit(unlist(info(vcf)$CSQ), "\\|")
  csq_data_padded <- lapply(csq_data, function(x) { length(x) <- n_cols; x })
  csq_df <- as.data.frame(do.call(rbind, csq_data_padded), stringsAsFactors = FALSE)
  colnames(csq_df) <- csq_format

  # Expand core VCF fields
  element_len <- sapply(info(vcf)$CSQ, length)
  gr <- rowRanges(vcf)
  df <- data.frame(
    CHROM = rep(as.character(seqnames(gr)), element_len),
    POS = rep(start(gr), element_len),
    REF = rep(as.character(ref(vcf)), element_len),
    ALT = rep(as.character(unlist(alt(vcf))), element_len),
    csq_df,
    stringsAsFactors = FALSE
  )
  message("✅ Total transcript-level annotations: ", nrow(df))

  # Filter: protein_coding
  if (require_protein_coding && "BIOTYPE" %in% colnames(df)) {
    df <- df %>% filter(BIOTYPE == "protein_coding")
    message("🔬 After filtering for BIOTYPE == 'protein_coding': ", nrow(df))
  }

  # Filter: IMPACT
  if ("IMPACT" %in% colnames(df)) {
    df <- df %>% filter(IMPACT %in% impact_levels)
    message("🔥 After filtering by IMPACT (", paste(impact_levels, collapse = ", "), "): ", nrow(df))
  }

  # Filter: AF or gnomAD_AF
  if ("AF" %in% colnames(df)) {
    df$AF <- suppressWarnings(as.numeric(df$AF))
    df <- df %>% filter(is.na(AF) | AF <= max_af)
    message("🧬 After filtering AF ≤ ", max_af, ": ", nrow(df))
  } else if ("gnomAD_AF" %in% colnames(df)) {
    df$gnomAD_AF <- suppressWarnings(as.numeric(df$gnomAD_AF))
    df <- df %>% filter(is.na(gnomAD_AF) | gnomAD_AF <= max_af)
    message("🧬 After filtering gnomAD_AF ≤ ", max_af, ": ", nrow(df))
  }

  # Filter: SIFT
  if ( ("SIFT" %in% colnames(df)) & !is.null(sift_filter)) {
    df <- df %>% filter(!str_detect(SIFT, paste(sift_filter, collapse = "|")))
    message("🧪 After filtering out SIFT (", paste(sift_filter, collapse = ", "), "): ", nrow(df))
  }

  # Filter: PolyPhen
  if ( ("PolyPhen" %in% colnames(df)) & !is.null(polyphen_filter)) {
    df <- df %>% filter(!str_detect(PolyPhen, paste(polyphen_filter, collapse = "|")))
    message("🧪 After filtering out PolyPhen (", paste(polyphen_filter, collapse = ", "), "): ", nrow(df))
  }

  # Final clean-up and select
  df <- df %>%
    filter(!is.na(SYMBOL) & SYMBOL != "") %>%
    mutate(Protein_change=paste0(Protein_position, Amino_acids)) %>% 
    select(CHROM, POS, REF, ALT, SYMBOL, Gene, Consequence, IMPACT,
           AF = any_of(c("AF", "gnomAD_AF")), SIFT, PolyPhen, Protein_change) %>%
    distinct() 

  message("✅ Final number of unique filtered rows: ", nrow(df))

  return(df)
}
