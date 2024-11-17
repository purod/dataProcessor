reportDTtable <- function (df, numdig = 2, filter = "bottom", ...){
        DT::datatable(df, extensions = "Buttons", options = list(dom = "lBfrtip", 
        buttons = c("pageLength", "copy", "csv", "excel", "pdf", 
            "print"), search = list(regex = T)), filter = filter,
        ...) %>% DT::formatRound(purrr::map_lgl(.$x$data, is.numeric),
        digits = numdig)
}

checkFileqc <- function(df, id="bcr_patient_barcode", ignoreRegex=NULL,
                            patients=NULL, returnDT=TRUE){
  vars_ignore = id
  if(!is.null(ignoreRegex)){
    vars_ignore = c(vars_ignore, grep(ignoreRegex, colnames(df), value=T) )
  }
  message("ignoring:\n", paste0(vars_ignore, collapse="\n"))
  if(is.null(patients))
    Npats = length(unique(df[[id]]))
  else{
    Npats = length(unique(patients))
    df = df %>% filter(.data[[id]] %in%
                       patients)
    misspats = dplyr::setdiff(patients, df[[id]])
    df = df %>% add_row("{id}":=misspats) 
  }
  allvars = dplyr::setdiff(colnames(df), vars_ignore)
  res = purrr::map_df(allvars,
               function(vv){
                 print(vv)
                 val = df[[vv]]
                 Nmiss = sum(is.na(val))
                 Pmiss = Nmiss / Npats * 100
                 myclass = class(val)
                 Nunique = length(unique(val))
                 if(is(val, "logical") |
                    is(val, "character") |
                    is(val, "factor") |
                    Nunique < 10
                    ){
                   ## consider it discrete
                   tab = gnsutils::table2(val)
                   minlev = min(tab)
                   minlev_name = which.min(tab) %>% names %>% as.character()
                   minlev_name = ifelse(minlev_name == "NA", 'missing', minlev_name)
                   maxlev_name = which.max(tab) %>% names %>% as.character()
                   alllev =  paste0(
                     glue::glue("{names(tab)}({as.numeric(tab)})"), 
                     collapse=", ")
                   minlev_count = minlev
                   minlev_perc = minlev_count / Npats * 100
                   maxlev_count = tab[maxlev_name]
                 }else{
                   ## consider it continuous
                   minlev_count = NA
                   minlev_name = ""
                   alllev = NA
                   maxlev_count = NA
                   maxlev_name = ""
                   minlev_perc = NA
                 }
                 tibble(Variable=vv,
                        `Number of missing values`=Nmiss,
                        `Percentage of missing values`=Pmiss,
                        `Type of variable`=myclass,
                        `Number of unique values`=Nunique,
                        `If discrete, min level name` = minlev_name,
                        `If discrete, min level count`=minlev_count,
                        `If discrete, min level perc`=minlev_perc,
                        `If discrete, max level name` = maxlev_name,
                        `if discrete, max level count`=maxlev_count, 
                        `If discrete, levels`=alllev
                        )
               })  %>%
    arrange(`Number of missing values`,
            desc(`If discrete, min level count`), 
            desc(`Number of unique values`)
            )
  if(!returnDT)
    return(res)
  message("Converting to DT")
  resdt = res %>% reportDTtable() %>%
    DT::formatStyle(
      "Percentage of missing values",
      backgroundColor=DT::styleInterval(c(20, 50, 90), c("white", "yellow", "orange", "red"))
    ) %>%
    DT::formatStyle(
      "Number of unique values",
      backgroundColor=DT::styleEqual(1, c("red"))      
    ) %>%
    DT::formatStyle(
      "If discrete, min level count",
      backgroundColor=DT::styleInterval(c(2, 4, 6), c("red", "orange", "yellow", "white"))      
    )
}
