isContinuVar <- function(x, con_cut = 5){
  is_numeric <- ((length(table(x)) >= con_cut) & 
    (Hmisc::all.is.numeric(x, extras = c(NA,"","unknown") ))) 
  return(is_numeric)
}

isDiscreteVar <- function(x, con_cut = 5){
  is_numeric <- ((length(table(x)) >= con_cut) & 
    (Hmisc::all.is.numeric(x, extras = c(NA,"","unknown") ))) 
  return(!is_numeric)
}
