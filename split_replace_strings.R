# Replace periods with hyphens
split_replace_strings <- function(input_vector, loc=c(1:4), symb=c("\\.","-")) {
  # Replace periods with hyphens and extract the first four parts
  sapply(input_vector, function(x) {
    #modified_string <- gsub(symb[1], symb[2], x)    # Replace periods with hyphens
    parts <- strsplit(x, symb[1])[[1]]              # Split the string by hyphen
    final_result <- unname(paste(parts[loc], collapse = symb[2]))        # Combine the first four parts
    return(final_result)
  }) %>% unname()
}
