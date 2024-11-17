reportDTtable <- function (df, numdig = 2, filter = "bottom", ...){
    numeric_cols <- which(sapply(df, is.numeric))
	DT::datatable(df, extensions = "Buttons", options = list(dom = "lBfrtip", 
        buttons = c("pageLength", "copy", "csv", "excel", "pdf", 
            "print"), search = list(regex = T)), filter = filter, 
        ...) %>% formatRound(purrr::map_lgl(.$x$data, is.numeric), 
        digits = numdig)
}
