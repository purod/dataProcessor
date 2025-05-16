library("gtsummary")
isDiscreteVar <- function(x, con_cut = 5){
  is_numeric <- ((length(table(x)) >= con_cut) & 
    (Hmisc::all.is.numeric(x, extras = c(NA,"","unknown") ))) 
  return(!is_numeric)
}

reportVarDistTable <- function(df, group = NULL) {
    
    # making the NA value explicit level of factor with `forcats::fct_explicit_na()`
    df <- df %>% 
        mutate_if( isDiscreteVar, as.factor) %>% 
        mutate_if( is.factor, forcats::fct_explicit_na, na_level = "Missing" )

    library(gtsummary)
    # Create the summary table with or without the `by` parameter depending on `strata`
    if (is.null(group)) {
        table <- df %>%    
            tbl_summary(
                statistic = list(
                    all_continuous() ~ c("{median} ({p25}, {p75})",
                                         "{mean} ({min}, {max})"),
                    all_categorical() ~ "{n} / {N} ({p}%)"
                ),
                type = all_continuous() ~ "continuous2", # allow multiple-line summary
                digits = all_continuous() ~ 2,
                missing = "ifany", # only show summary of NA when it exists
                missing_text = "Missing",
                percent = "column"  # This will calculate percentages within each column
            )
    } else {
        table <- df %>%    
            tbl_summary(
                by = group,  # Stratify by the provided variable
                statistic = list(
                    all_continuous() ~ c("{median} ({p25}, {p75})",
                                         "{mean} ({min}, {max})"),
                    #all_categorical() ~ "{n} / {N} ({p}%)"
                    all_categorical() ~ "{n}"
                ),
                type = all_continuous() ~ "continuous2",
                digits = all_continuous() ~ 2,
                missing = "ifany",
                missing_text = "Missing",
                percent = "column"  # This will calculate percentages within each column
            ) %>%
            add_overall()  # Adds an overall column (only when `by` is specified)
    }

    return(table)
}
