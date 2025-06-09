simplify_stage <- function(stage_vector) {
  case_when(
    grepl("Stage IV", stage_vector, ignore.case = TRUE)   ~ "IV",
    grepl("Stage III", stage_vector, ignore.case = TRUE)  ~ "III",
    grepl("Stage II", stage_vector, ignore.case = TRUE)   ~ "II",
    grepl("Stage I", stage_vector, ignore.case = TRUE)    ~ "I",
    grepl("Stage 0", stage_vector, ignore.case = TRUE)    ~ "0",
    grepl("I/II NOS", stage_vector, ignore.case = TRUE)   ~ "I/II",
    TRUE ~ NA_character_
  )
}
