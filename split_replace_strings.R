# Replace periods with hyphens
split_replace_strings <- function(input_vector, loc=c(1:4), symb=c("\\.","-")) {
  # Replace periods with hyphens and extract the first four parts
  sapply(input_vector, function(x) {
    modified_string <- gsub(symb[1], symb[2], x)    # Replace periods with hyphens
    parts <- strsplit(modified_string, symb[2])[[1]]              # Split the string by hyphen
    final_result <- unname(paste(parts[loc], collapse = "-"))         # Combine the first four parts
    return(final_result)
  })
}
