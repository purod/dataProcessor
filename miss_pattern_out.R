miss_pattern_out <- function(df, end.points, out.type="normal"){
  # dataframe containing all variables with missing samples > 5 and endpoints
  df1 = data.frame(is.na(df)[,colSums(is.na(df))>5]*1,df[,end.points,drop=F])
  vars = setdiff(names(df1),end.points)
  
  if(out.type=="binary"){
    
    # convert response into 1/0
    name.cate = names(table(df1[[end.points]]))
    df1[[end.points]][df1[[end.points]]==name.cate[1]] = 1
    df1[[end.points]][df1[[end.points]]==name.cate[2]] = 0
    df1[[end.points]] = as.numeric(df1[[end.points]])
    
    vars.p = lapply(vars,
                    function(var) {
                      formula    <- as.formula(paste(end.points, "~", var))
                      res.logist <- glm(formula, data = df1, family = binomial)
                      
                      summary(res.logist)$coefficients[2,4]
                    }) %>% unlist()
    
  }else if(out.type=="normal"){
    
    vars.p = lapply(vars,
                    function(var) {
                      formula    <- as.formula(paste(end.points, "~", var))
                      res.gaus <- glm(formula, data = df1, family = gaussian)
                      
                      summary(res.gaus)$coefficients[2,4]
                    }) %>% unlist()
    
  }else if(out.type=="survival"){
    
    time=df1[[end.points[1]]]
    status=as.numeric(df1[[end.points[2]]])
    
    vars.p = lapply(vars,
                    function(var) {
                      formula    <- as.formula(paste('Surv(time, status)~', var))
                      res.surv <- coxph(formula, data = df1)
                      summary(res.surv)$coefficients[5]
                    }) %>% unlist()
  }
  names(vars.p)=vars
  return(vars.p)
}