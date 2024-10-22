# Part I. set up

# 1. functions
#library(readxl)
#library(xlsx)
#library(cowplot) # load ggplot2
#library(plyr) # load plyr before dplyr
#library(dplyr) # load magrittr
# library(tidyr)
#library(stringr)
#library(lubridate)
#library(data.table)
# library(naniar) # missingness
# survival analysis
#library(ggpubr)
#library(survminer)
#library(survival)
#library(survcomp)
# correlated variables
# library(ggcorrplot)
#library(igraph)
#library(DT)
# presentation
#library(kableExtra)


# 1. commonly used Rmarkdown settings
# 1.1 fig caption 
#     fig.cap=c("a", "b")


# 1.x set up git
# library(workflowr)
# wflow_start("project_name")
# wflow_build()
# wflow_open("analysis/first-analysis.Rmd")
# wflow_view()
# wflow_status()


# 2. common mistake
# 2.1 when reading excel file, specify type as "text" to keep the original format; 
## keep the original date format as a reference
# 2.1 distinguish the difference between combined filtering and sequential filtering where
# using filter
# 2.2 ifelse or if_else with missing values
# ifelse won't compare NA in NA==1, so the returned value will still be NA. However, NA
# will be treated as false if you use %in%
# ifelse(1==NA&1==2,1,0) will return 0 since it knows that latter situation fails
# for complex situation, especially with missingness, it's better to use  case_when
# compared with ifelse
# 2.3 levels(mydata$factor_var) = c("different order") will totally change the data
#     avoid using factor unless you have to do that 
# 2.4 it is easy to ignore '' in the data, so use mutate_all(na_if,"") to replace
# '' with NA
# 2.5 if you use . in dplyr, then don't use %>%, for example . %>% dplyr::select()
# there's some unknow result or error. Instead, try using .[,grepl()] or .[,20:30]
# to dplyr::select variables 
# 2.6 use pmax, pmin, if_else to keep original data format
# 2.7 pay attention to the package version you are using, especially those packages
# related to data preprocessing
# 2.8 if you download some dataset from online, be careful if the data is being updated 
# in real time
# 2.9 check if the data has missing values
# matrix(c(1,2,3,4),ncol=2) %>% colSums(is.na(.))
# The reason this doesn't work is that colSums treated is.na(.) as a parameter instead of
# the input. The right way is matrix(c(1,2,3,4),ncol=2) %>% is.na() %>% colSums()
# 3. strsplit would take in the column instead of doing a rowwise operation.
# df %>% filter( "1"%in%unlist(strsplit(col,"\\|")))
# to fix it, you need 
# df %>% group_by(patient_ID) %>% filter( "1"%in%unlist(strsplit(col,"\\|")))
# 4. rownames_to_column would cause some problems with seurat object
#.   use mutate to create cellID column

# 3. sanity check
# I recommend doing sanity check on all raw data rather than only the data for model
# some mistakes might be hidden
# 3.1 duplicated ID/rows, date format, weird values/characters
# 3.2 a. Chronology: combine all date columns and calculate the day difference between
#     the two ajacent columns
#     b. missing lines
#     b. treatment trajectory and events
#     c. appropriate usage of regimens(patient status, FDA approval time, regimen combination)
#     d. 
# 3.3 a. relationship between endpoints 
#     b. extremely short or long endpoints
#     c. right censoring
# 3.4 information provided is consistent with our own derivation/known facts
# 3.5 effect orientation is correct
# 3.6 definition of columns

# 4. common steps in one project
# 4.1 patient inclusion/exclusion, what kind of pateint cohort
#     a. disease stage and patient status
#     c. definition of endpoints and their signals
#     d. completeness of key known predictors
#     e. treatment information
#     f. data consistence across data modalities
# 4.2     b. whether or not baseline measurement




## ggplot theme
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(face = "bold",angle=90,vjust =2),
            axis.title.x = element_text(face = "bold",vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  discrete_scale("fill","Publication",
                 scales::manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c",
                                       "#662506","#a6cee3","#fb9a99","#984ea3",
                                       "#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  discrete_scale("colour","Publication",
                 scales::manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c",
                                       "#662506","#a6cee3","#fb9a99","#984ea3",
                                       "#ffff33")), ...)
  
}


theme_classicM <- function(ratio=c(8,12,14,14,8), legend.position = "right"){
  print("Default ratio is main title:2, axis title:1.5, axis text: 2, legend: 2, text:3")
  theme_classic() +
    theme(
      # axis and fig title
      plot.title = element_text(face = "bold",size=ratio[1]),
      axis.title = element_text(face = "bold",size=ratio[2]),
      # axis text and lines
      axis.text.y = element_text(size=ratio[3],color="black"),
      axis.text.x = element_text(size=ratio[3],color="black",angle=45,hjust=1),
      axis.line = element_line(size=rel(0.2),linetype="solid"),
      # legend 
      legend.position = legend.position,
      legend.title = element_text(face = "bold",size=ratio[4]),
      legend.text = element_text(face = "bold",size=ratio[4]),
      # text
      text = element_text(size = ratio[5]),
      plot.margin=unit(c(10,5,5,5),"mm")
      
    )
  
}

wrapper <- function(x, ...){
  
  paste(strwrap(x, ...), collapse="\n")
}

# 2.2. Some quick graphs

# a. scatter plot

# ggplot(plot_Data, aes(x=ttd, y=ttnt, color=Status)) +
#   geom_point() + xlim(0,1200) +                                   # scatter plot
#  labs(title = "Relationship between ttnt and\nttd for 167 pts",   # title, xylab, legend
#       x = "ttd(days)", y = "ttnt(days)", color="Status") +   
#  scale_color_discrete(labels=paste(levels(plot_Data$Status),      # count number 
#                                    paste0("(", table(plot_Data$Status),"pts)"))) +
#  theme_classicM(legend.position = "bottom") +                     # legend position           
#  guides(color=guide_legend(nrow=4))                               # legend organization

# b. histogram plot
ggplot_hist <- function(df, x, status="none",
                        legend.position="none", bindwidth=1){
  if(status == "none"){
    
    ggplot(data = df, aes_string(x=x)) +
      geom_histogram(binwidth=bindwidth) + 
      theme_classicM(legend.position = legend.position)
    
  }else{
    
    ggplot(data = df, aes_string(x=x, fill=status)) +
      geom_histogram(binwidth=bindwidth,alpha=0.4,position = "dodge") + 
      #scale_fill_manual(values=c("#69b3a2", "#404080"))+
      theme_classicM(legend.position = legend.position)
    
  }
}

# c. Horizontal barplot
ggplot_bar <- function(df, x, y, prop,
                       legend.position="none"){
  
  ggplot(data = df, aes_string(x=x, y=y)) +
    geom_bar(stat='identity') + 
    geom_text(aes_string(label=prop), 
              vjust=0.5, hjust= 1.2, color="white") +
    theme_classicM(legend.position = legend.position) +
    coord_flip()
}


horiBar4vector <- function(tableVec, ntop){
  # Convert the table to a data frame
  tableSort <- head( sort(tableVec, decreasing = T), ntop)
  varName <- names(tableSort)
  value <- as.vector(tableSort)
  mydata <- data.frame(varName, value)
  mydata$varName <- factor(mydata$varName, levels=mydata$varName)
  
  # Create a horizontal bar plot using ggplot2
  ggplot(mydata, aes(x = varName, y = value)) +
    geom_bar(stat = "identity", position = "identity", fill = "lightblue") +
    labs(title = "Horizontal Bar Plot with ggplot2",
         x = "Variables",
         y = "Frequency") + 
    coord_flip()
}

# vertical barplot, y axis would be count of each x
# ggplot() + geom_bar(aes(as.character(year(test$LOT_Start_Date_fixed)))) + 
#  xlab("Start_year_0L(Out of 130 patients)") +
#  theme_classicM(legend.position = "none")

# d. barplot with multiple groups
ggplot_mulbars <- function(df, x, y, group, prop, 
                           stack=F, barheight, legend.position="none"){
  
  if(!stack){ # non-stack
    
    ggplot(data = df, aes_string(x=x, y=y, 
                                 fill=group)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_text(aes_string(label=prop), vjust=1.6, color="black",
                position=position_dodge(0.9)) +
      scale_fill_brewer(palette="Accent") +
      theme_classicM(legend.position = legend.position)
    
  }else{    # stacked
    
    # sort both group and y using arrange: arrange(y, group)
    ggplot(data = df, aes_string(x=x, y=y, fill=group)) +
      geom_bar(stat="identity") +
      geom_text(aes_string(y=barheight, label=prop), 
                vjust=1.6, color="black") +
      theme_classicM(legend.position = legend.position)
    
  }
}

# boxplot by two factors
# bxp <- ggpubr::ggboxplot(
#  df.selection.rna, x = "Time", y = "ENSG00000141971_MVB12A",
#  color = "Adalimumab", palette = "jco"
# )
# bxp

# e. venn plot
venn_diagram <- function(dl){
  #'  @param dl : input data list
  #'              e.g. dl=list(a=,b=,c=)
  #'              
  library(ggvenn,lib="/home/qdu/R/x86_64-pc-linux-gnu-library/3.6")
  ggvenn::ggvenn(
    dl, show_percentage = FALSE,
    fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")[1:length(dl)],
    stroke_size = 0.5, set_name_size = 6, text_size=4
  )
}


# Part II. Preprocessing

# 1. Preprocessing

# 1.0 commonly used  maniputation techniques

# a. dplyr::select specific columns/rows

# a.1. dplyr::select top n rows ordered by specified variable
# top_n(mydata, n, variable_to_order)

# a.2. dplyr::select duplicated rows
# group_by(a,b) %>% filter(n() > 1) 
# dplyr::select(!isUnique(col))

# a.3. dplyr::select rows by index
# mydata %>% .[c(1,2,3),]

# a.4. dplyr::select columns by dplyr::select_if
# comments: create a new function if you want to specify some parameters
# dplyr::select_if(~is.NumCon(., con_cut = con_cut))
# or
# dplyr::select_if(function(x) is.NumCon(x, con_cut = con_cut))

# a.5. dplyr::select columns using a character vector
# dplyr::select(-all_of(NULL))

# a.6. convert into factors
# convert_as_factor( a,b,c )



# b. Creat new rows/columns

# b.1. condition for multiple logic columns
# mutate(a_alone = a & rowSums(data[,c(1:4)]==0))
# mutate(AA=rowSums(dplyr::select(.,starts_with("AA")))>0) %>% 

# b.2. create a new column and its row values are based on different columns
# mydata %>% mutate(new_col=apply(.,1,function(x){manipulation}))
# mutate(Historical_regimens = paste(Tx_regimen[LOT < LOT_ini], collapse = '&'))

# b.3. add count column
# add_count(ID)
# count(ID)

# b.4. add missing LOT rows for each SubjectID
# group_by(SubjectID) %>% tidyr::complete(LOT=full_seq(min(LOT):max(LOT),1))

# b.5 keep the original order in a by turning into a factor
# mutate(a = factor(a,levels=unique(a))) 

# b.6. add more rows
# bind_rows()

# b.7. average duplicated rows by groups, slow with large data frame
# group_by(group_ID) %>% summarise_all(mean)
# average data frame by rownames, colnames, or a specific column
averColDup.byName <- function(mat){
  
  sampleName <- colnames(mat)
  sampleNameDup <- sampleName[duplicated(sampleName)]
  sampleNameUni <- setdiff(sampleName, sampleNameDup)
  
  umat <- matrix(-1,nrow=nrow(mat),ncol=length(sampleNameDup), 
                 dimnames=list(rownames(mat),sampleNameDup))
  
  for(x in 1:length(sampleNameDup)){
    
    umat[,x] <- rowMeans(mat[,which(sampleName==sampleNameDup[x])])
    
  }
  
  fmat <- cbind(umat, mat[,sampleName %in% sampleNameUni])[,unique(sampleName)]
  
  if(is.data.frame(mat)){
    return(as.data.frame(fmat,stringsAsFactors = F))
  }else{
    return(fmat)
  }
  
}

averRowDup.byName <- function(mat){
  
  sampleName <- rownames(mat)
  sampleNameDup <- sampleName[duplicated(sampleName)]
  sampleNameUni <- setdiff(sampleName, sampleNameDup)
  
  umat <- matrix(-1,nrow=length(sampleNameDup), ncol=ncol(mat), 
                 dimnames=list(sampleNameDup, colnames(mat)))
  
  for(x in 1:length(sampleNameDup)){
    
    umat[x,] <- colMeans(mat[which(sampleName==sampleNameDup[x]), ])
    
  }
  
  fmat <- rbind(umat, mat[sampleName %in% sampleNameUni,])[unique(sampleName),]
  
  if(is.data.frame(mat)){
    return(as.data.frame(fmat,stringsAsFactors = F))
  }else{
    return(fmat)
  }
  
}

averRowDup.byCol <- function(df, colVar){
  
  dfUnique <- df %>% filter(isUnique(df[,colVar])) %>% 
    bind_rows(df %>% filter(!isUnique(df[,colVar])) %>% 
                group_by_at(colVar) %>% 
                summarise_all(mean)) 
  
  dfUnique <- dfUnique[match(dfUnique[,colVar], unique(df[,colVar])),]
  
  return(dfUnique)
}

# c. Modify rows/columns already existed

# c.1. columns as function parameters
# Comment: col1 is the first parameter; col2 is the second, col3 is the third
# mutate_at(vars(col1),funs,.$col2,.$col3,other_fun_parameters)  

# c.2. minus some columns by one column
# Comment: this creates extra columns with suffix dif
# mutate_at(vars(cols),list(dif=~.-col1))

# c.3 convert patient with gene expression higher than the median as "High"
#     and lower than the median as "low"
# mutate_at(vars(var1:varn),~ifelse(.>median(.),"High", "Low"))

# c.4. convert "" into NA
# mutate_all(na_if,"")

# c.5. replace missing values by column means
#  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

# c.6. replace missing values with 0s
# mutate_at(vars(var1:var2), coalesce, 0)

# c.6 use abbreviations 
# mutate_at(vars(primary_therapy_outcome_success), ~unname(abbreviate(.)))


# d. position and names of columns

# d.1. dplyr::select(col1,everything())

# d.2. rename columns
# Method1: plyr::rename(c("old_name"="new_name"))
# for those names containing a, b, or c, replace with d
# Method2: mydata %>% setNames(gsub("a|b|c","d",names(.)))
#          mydata %>% setNames(paste0('cars.', names(.)))
# Method3: dplyr::select(newname=oldname)

# d.3. Assign column names
# mydata %>% `colnames<-`col_names %>% other_manipulation

# d.4. convert rowname to a column
# add_rownames(var = 'Predictors')


## other tricks
# if_else to keep original format
## transpose a dataframe
# The function transpose is faster the t()
# transpose(keep.names="assign colnames a name", 
#           make.names = "which column to be used as new column names")
# There are two melt function, one is from the reshape2 and another is from the
# DT package, use the latter one
# setDT(mCRPC) %>% melt(id.vars='SubjectID',variable.factor=F)


# 1.1 Commonly used scripts to process text

# a. gsub
# a. Insert characters in a sentence
# gsub('(.*binary|.*categorical)(.*)','\\1 = \\2', Variablen)
# b. trimws() remove leading and trailing whitespace

# calculate the mode of a vector
getMode <- function(vec){
  mode <- table(vec) %>% which.max %>% names
  return(mode)
}

geneKept <- function(mut, min.count, min.perc ){
  max.count <- nrow(mut)-max( min.count, min.perc*nrow(mut))
  geneLog <- apply(mut, 2, function(x) {sum(x==getMode(x)) < max.count})
}

# calculate the confidence interval
getCI <- function(vec, nsample, alpha){
  Q = qnorm(1-alpha/2)
  se = sd(vec)/sqrt(nsample)
  interval = c( mean(vec)-Q*se, mean(vec)+Q*se )
  return(interval)
}

getBtQuantile <- function(vec, range=c(0.025,0.975)){
  vec_range = unname( quantile(vec, range) )
  vec_select = vec[vec > vec_range[1] & vec < vec_range[2]]
  return(vec_select)
}

# get level numbers of variable
getLevelNum <- function(df, variables){
  numberLevels <- sapply(variables, function(var){length(unique(df[,var]))})
  return(numberLevels)
}


# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}

# drop imlalanced variables
varKept <- function(df, cut){
  
  if(cut>1){ # if cut is number of the minor category
             # only works with data frame without missing values
             # variable with cut number is not kept
    varLog <- apply(df, 2, function(x) 
      {sum(as.character(x)==getMode(x), na.rm=T) < (nrow(df)-cut) })
  }else{ # if cut is the proportion of the minor category
         # variable with cut number is not kept
    varLog <- apply(df, 2, function(x) 
    { mode_number <- sum(as.character(x)==getMode(x), na.rm=T)
      NoNA_number <- sum(!is.na(x))
      mode_number/NoNA_number < (1-cut)}) 
  }
  return(unlist(varLog))
}

# 1.2 features dropped at the beginning

## drop features with high missing rate
## drop imbalanced features

dqCheck <- function(df, cut_off=0.25){
  
  # convert "" and " " into NA
  df <- df %>% 
    mutate_all(na_if, "") %>% 
    mutate_all(na_if, " ")
  
  df.name <- colnames(df)
  df.nrow <- nrow(df)
  
  # Exclude variables with missing rate larger than the cutoff
  highRate <- which( colSums(is.na(df))/df.nrow > cut_off )
  message(paste0('excluding ', length(highRate),' variables with missing rate > ',
                 cut_off, ': ', paste(df.name[highRate], collapse=",")))
  cat('\n')
  
  # Exclude variables with imbalanced levels or zero variance
  df1 <- df %>% dplyr::select(-one_of(df.name[highRate]))
  df1.name <- colnames(df1)
  
  len.level <- sapply(df1, function(x) length(table(x)))
  len.var <- sapply(df1, function(x) sum(!is.na(x)))
  min.level <- sapply(df1, function(x) min(table(x))) 
  max.level <- sapply(df1, function(x) max(table(x))) 
  min.level.p <- sapply(df1, function(x) min(table(x)) / sum(!is.na(x)))
  max.level.p <- sapply(df1, function(x) max(table(x)) / sum(!is.na(x))) 
  
  imbalanced = which(len.level == 1 | 
                       ((min.level < 10 | min.level.p < 0.025) | 
                          (max.level > (len.var-10) | max.level.p > 0.975)))
  
  message(paste0('excluding ', length(imbalanced),' imbalanced variables: ',
                 paste(df1.name[imbalanced], collapse=",")))
  cat('\n')
  
  # return a new data frame
  df2 <- df %>% dplyr::select(-one_of(union(df.name[highRate], df1.name[imbalanced])))
  
  return(df2)
}

# 1.3. Numeric or Discrete
is.NumCon <- function(x, con_cut = 5){
  is_numeric <- ((length(table(x)) >= con_cut) & (Hmisc::all.is.numeric(x, extras = NA))) |
    identical(names(table(x)), c("0","1"))
  return(is_numeric)
}

# 1.4. add suffix based on data type 
add.Type <- function(df, excludeVars=NULL){
  
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  
  tagList <- apply(df2, 2, function(x){
    if(Hmisc::all.is.numeric(x, extras = NA)){
      if(length(table(x)) == 2){
        '_binary'
      }else if(length(table(x)) == 3){
        '_categorical'
      }else{
        '_continuous'
      }
    }else if(length(table(x)) == 2){
      '_binary'
    }else{
      '_categorical' 
    }
  }) %>% unlist()
  
  colnames(df2) <- paste0(colnames(df2), tagList)
  
  df <- data.frame(df1,df2)
}

add.suffix <- function(df, suffix="NULL",excludeVars=NULL){
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  colnames(df2) <- paste0(colnames(df2), "_", suffix)
  df <- data.frame(df1,df2)
}

add.preffix <- function(df, preffix="NULL",excludeVars=NULL){
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  colnames(df2) <- paste0(preffix, "_", colnames(df2))
  df <- data.frame(df1,df2)
}

add.ffix <- function(df, preffix="NULL", suffix="NULL", excludeVars=NULL){
  df1 <- df %>% dplyr::select(all_of(excludeVars))
  df2 <- df %>% dplyr::select(-one_of(excludeVars))
  colnames(df2) <- paste0(preffix, "_", colnames(df2), "_", suffix)
  df <- data.frame(df1,df2)
}

get.conciseName <- function(dfName, suffix="_continuous|_binary|_categorical"){
  dfName_nosuffix = gsub( suffix, "", dfName )
  concise_name <- sapply( dfName_nosuffix, 
          function(name_unit){ 
            string_split = strsplit(name_unit,"_")[[1]]
            if( length(string_split) > 1 & grepl("^ENSG",name_unit) ){
              return(string_split[2])
            }else{
              return(name_unit)
            }
          }
  )
}

mgsub <- function(old_patterns, new_replacements, target){
  for(i in 1:length(old_patterns)){
    target = gsub(old_patterns[i], new_replacements[i], target)
  }
  return(target)
}

vector2df <- function(vector,varnames){
  df <- as.data.frame(vector,) %>%
    rownames_to_column(var=varnames[1])
  colnames(df) <- varnames
  return(df)
}


# 1.4. Missingness
# a. Drop patients with >25% missingness and then variales with >25% missingness, 
# but it also depends on how important the variable is
# b. categories with representations less than 10 or 2.5%, combine with other category

#library(naniar)
missing_perc_pattern <- function(mydata){
  
  # barplot showing missing percentage
  mydata.missing_summary <- mydata %>% 
    miss_var_summary() %>% 
    filter(pct_miss>0) %>% 
    arrange(n_miss) %>% 
    mutate(variable=factor(variable, levels = variable))
  
  # missing percentage
  miss_perc <- ggplot_bar(mydata.missing_summary, 
                          "variable", "pct_miss", "n_miss") +
    labs(x = "Variables with missing values",y = "Missing Percentage(%)")
  
  print(miss_perc)
  
  # missing pattern
  miss_patt <- vis_miss(mydata[,levels(mydata.missing_summary$variable)], cluster=TRUE)+
    theme(axis.title = element_text(color = "black",size=rel(2),face = "bold"),
          axis.text.x = element_text(color = "black",hjust = 0.5,size=rel(2)),
          axis.text.y = element_blank(),
          legend.position = "right",
          legend.text=element_text(size=rel(1.2))) +
    coord_flip()
  print(miss_patt)
}

# simply fill in the missing values with median
fillMM <- function(df){
  miss.pos <- which(is.na(df),arr.ind=T)
  if(nrow(miss.pos) == 0){
    return(df)
  }else{
    df[miss.pos] <- unlist(apply(df,2,median,na.rm=T))[miss.pos[,2]]
    return(df)
  }
}
# simply fill in the missing values with specified value, here is zero
# mutate_at(vars(`CASE-NEUMN012EVP-00187-G`:`CASE-NEUEU419NMF-04089-G`),
#          coalesce, 0)


# fill up missing values
# library(bnstruct)
knnImpute <- function(df, excludeVars = NULL, noImpVars = NULL){
  
  #'  @param df : input data frame for data columns imputed
  #'              todo in the future
  #'              message which variables are identified as categorical/continuous variabels
  #'  @param excludeVars : variables not participating in the imputation
  #'  @param noImpVars : variables not imputed, but provide info for imputation process
  
  # do we have mispelled variables
  if((!is.null(excludeVars) & any(!excludeVars %in% names(df))) |
     (!is.null(noImpVars) & any(!noImpVars %in% names(df)))){
    warning("\nCan't find some variables in excludeVars or noImpVars\n")
  }
  
  
  df1 <- df %>% dplyr::select_if(!(names(.) %in% excludeVars))
  df2 <- df %>% dplyr::select_if(!(names(.) %in% c(excludeVars, noImpVars)))
  
  # positions of categorical variables
  categoryID <-  which(sapply(df1, is.character)) %>% unname()
  
  
  # imputed matrix
  dfImputed <- df1 %>% 
    mutate_if(is.character,as.factor) %>% 
    data.matrix() %>% 
    bnstruct::knn.impute(k = 10, cat.var = categoryID) %>% 
    as.data.frame()
  
  # missing variables
  charMiss <- colnames(df2)[sapply(df2,function(x){
    sum(is.na(x)) > 0 & is.character(x)
  })]
  message("\nCategorical vars imputed:", paste(charMiss, collapse = ","))
  
  conMiss <- colnames(df2)[sapply(df2,function(x){
    sum(is.na(x)) > 0 & is.numeric(x)
  })]
  message("\nContinuous vars imputed:", paste(conMiss, collapse = ","))
  
  # fill up missing values
  df[,conMiss] = dfImputed[,conMiss]
  df[,charMiss] <- data.frame(sapply(charMiss, function(x){
    levels(as.factor(df[[x]]))[dfImputed[[x]]]}),stringsAsFactors = F)
  
  df
}

knnImputePC <- function(df, excludeVars = NULL, PCVarsPattern=NULL, noImpVars = NULL){
  
  #'  @param df : input data frame for data columns imputed
  #'  @param excludeVars : variables not participating in the imputation
  #'  @param PCVarsPattern : regular expression defining variables whose principal
  #'                         component matrix will be used for imputation
  #'  @param noImpVars : variables not imputed, but provide info for imputation process
  
  # do we have mispelled variables
  if((!is.null(excludeVars) & any(!excludeVars %in% names(df))) |
     (!is.null(noImpVars) & any(!noImpVars %in% names(df)))){
    warning("\nCan't find some variables in excludeVars or noImpVars\n")
  }
  
  # use principle components to perform the imputation
  if(!is.null(PCVarsPattern)){
    df.PCs <- df %>% dplyr::select(matches(PCVarsPattern)) %>% prcomp(.,scale=T)
    df.data <- df %>% dplyr::select(-matches(PCVarsPattern)) %>% 
      bind_cols( data.frame(df.PCs$x) )
  }else{
    df.data = df
  }
  
  df1 <- df.data %>% dplyr::select( -all_of(excludeVars) )
  df2 <- df.data %>% dplyr::select( -all_of(c(excludeVars,noImpVars)) )

  # positions of categorical variables
  categoryID <-  which(sapply(df1, is.character)) %>% unname()
  
  # imputed matrix
  dfImputed <- df1 %>% 
    mutate_if(is.character,as.factor) %>% 
    data.matrix() %>% 
    bnstruct::knn.impute(k = 10, cat.var = categoryID) %>% 
    as.data.frame()
  
  # missing variables
  charMiss <- colnames(df2)[sapply(df2,function(x){
    sum(is.na(x)) > 0 & is.character(x)
  })]
  message("\nCategorical vars imputed:", paste(charMiss, collapse = ","))
  
  conMiss <- colnames(df2)[sapply(df2,function(x){
    sum(is.na(x)) > 0 & is.numeric(x)
  })]
  message("\nContinuous vars imputed:", paste(conMiss, collapse = ","))
  
  # fill up missing values
  df.data[,conMiss] = dfImputed[,conMiss]
  df.data[,charMiss] <- data.frame(sapply(charMiss, function(x){
    levels(as.factor(df.data[[x]]))[dfImputed[[x]]]}), stringsAsFactors = F)
  
  # replace principle components with raw variables
  if(!is.null(PCVarsPattern)){
    df.final <- df.data %>% dplyr::select(-matches("^PC\\d+")) %>%
      bind_cols( df %>% dplyr::select(matches(PCVarsPattern)) )
  }else{
    df.final = df.data
  }
  return(df.final)
}

knnImputeTime <- function(df, excludeVars = NULL, noImpVars = NULL, timePattern=NULL){
  # impute for each time point
  imputeList <- lapply(timePattern, function(pattern){
    keptPattern = setdiff(timePattern, pattern)
    dfSubset <- df %>% select(-matches(paste0(keptPattern,collapse = "|"))) %>% 
      knnImpute(., excludeVars = excludeVars, noImpVars=noImpVars ) %>% 
      select( contains(pattern) )
  })
  imputeTime <- do.call(cbind, imputeList)
  
  dfImputed <- df %>% select( -matches(paste0(timePattern,collapse = "|")) ) %>% 
    bind_cols( imputeTime ) %>% 
    knnImpute(., excludeVars = excludeVars, noImpVars=noImpVars )
  return( dfImputed )
}

# 1.5. barplot and density plot
# library(cowplot)
var_dist <- function(mydata,col_id,box_var=NULL,con_cut=15,
                     nrow=3,ncol=3,title_rel=1,text_rel=0.8){
  
  
  # continuous variables suitable for histogram
  hist_var = names(mydata)[sapply(mydata,function(x){
    is.numeric(x)&length(table(x))<con_cut})]
  # continuous variables suitable for density plot
  den_var = names(mydata)[sapply(mydata,function(x){
    is.numeric(x)&length(table(x))>=con_cut})]
  
  my_plots <- lapply(names(mydata[,col_id]), function(var_x){
    
    p <- ggplot(mydata[,col_id]) +
      aes_string(var_x)
    
    if(var_x%in%box_var){
      p <- p + geom_boxplot(na.rm = TRUE) 
    }else if(var_x%in%den_var){
      p <- p + geom_density(na.rm = TRUE)
    }else if(var_x%in%hist_var){
      p <- p + geom_bar(na.rm = TRUE)+
        scale_x_continuous()
    }else{
      p <- p + geom_bar(na.rm = TRUE) +
        scale_x_discrete(na.translate = FALSE,label=abbreviate)
    } 
    
    p <- p + 
      theme(axis.title = element_text(size=rel(title_rel),face="bold"),
            axis.text = element_text(size=rel(text_rel))
      )
  })
  
  plot_grid(plotlist = my_plots,nrow=nrow,ncol=ncol,labels="auto")
  
}
# 1.6. QQplot
var_QQ <- function(mydata, con_cut=5, 
                   nrow=3, ncol=2){
  
  plotData <- mydata %>% 
    # dplyr::select only continuous variables
    dplyr::select_if(sapply(.,function(x){
      is.numeric(x)&length(table(x))>con_cut})) %>% 
    setNames(gsub("_categorical|_binary|_continuous","",names(.))) %>% 
    # log2 tranformation for all variables
    bind_cols(dataTransform(., pValThres=1, transform=T, chooseTransform="Log", 
                            BC.lambda=NULL, rseed=5067)) %>% 
    # sort alphabetically
    dplyr::select(sort(colnames(.)))
  
  # b. QQ plots with QQ lines
  for(chunk in split(1:ncol(plotData),ceiling(seq_along(1:ncol(plotData))/(nrow*ncol)))){
    chunkData <- plotData[, chunk]
    
    par(mfrow=c(nrow, ncol))
    
    for (var in names(chunkData)){
      varData <- chunkData[!is.na(chunkData[, var]), var]
      qqnorm(varData, main = var, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
      qqline(varData, col="red")
    }
  }
}

# if stat_qq_line from ggplot doesn't work
# QQ plot with QQ line
# for(chunk in split(2:ncol(plotData),ceiling(seq_along(2:ncol(plotData))/8))){
#  chunkData <- plotData[, chunk]

#  par(mfrow=c(2,4))

#  for (var in names(chunkData)){
#    varData <- chunkData[!is.na(chunkData[, var]), var]
#    qqnorm(varData, main = var, cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
#    qqline(varData, col="red")
#  }
#}



# 1.6. survival plot
#library("ggpubr")
#library("survminer")
#library(survival)

survPlot <-function(fit, ncol=1, legend_position="top", break_time=100){
  survminer::ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    risk.table = TRUE,       # show risk table.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = FALSE,         # show confidence intervals for 
    # point estimaes of survival curves.
    xlim = c(0,max(fit$time)),        # present narrower X axis, but not affect
    # survival estimates.
    break.time.by = break_time,     # break X axis in time intervals by 500.
    risk.table.y.text.col = T, # colour risk table text annotations.
    risk.table.y.text = F, # show bars instead of names in text annotations
    font.x = c('bold'),
    font.y = c('bold'),
    font.stickslab = c('bold'),
    surv.plot.height=0.65,
    risk.table.height=0.35,
    legend = legend_position
    # legend.labs=lab.legend
    # in legend of risk table
  ) +
    guides(colour=guide_legend(ncol=ncol))
}

# 1.7. detect and remove highly correlated variables
## old version to be deprecated
corrCluster <- function( df.input, excludeVars=c(""), keepVars=c(""), 
                         corr.cut=0.95, tag="_set" ){
  # input format  
  if(excludeVars[[1]]==""){
    
    df <- df.input %>% mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }else{
    
    
    df <- df.input %>% dplyr::select(-one_of(excludeVars)) %>% 
      mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }
  
  # use fred's function to speed up the calculation
  dfCheck <- refsfsutils::checkotfdf(df, thmin=corr.cut)
  
  # variable pairs with cor > cutoff
  if( !is.null(dfCheck$topCorrelations) ){
    
    # print(knitr::kable(head(cor.table),row.names=F))
    # cat('\n')
    
    # Group correlated variables...  
    cor.table <- dfCheck$topCorrelations %>% filter( abs(Cor) > corr.cut )
    vars.correlation <- data.frame( Var1.pos=match(cor.table$Var1,colnames(df)),
                                    Var2.pos=match(cor.table$Var2,colnames(df))) %>% 
      data.matrix()
    graph.cor = igraph::graph.data.frame(vars.correlation, directed = FALSE)
    groups.cor = split(unique(as.vector(vars.correlation)),igraph::clusters(graph.cor)$membership)
    groups.cor.varnames = lapply(groups.cor,FUN=function(list.cor){colnames(df)[list.cor]})
    message(paste0('Identified ', nrow(cor.table), 
                   ' pairs of correlated variables with r > ', corr.cut,
                   '. There are in total ', sum(sapply(groups.cor.varnames,length)),
                   ' correlated variables, which can be grouped by ', 
                   length(groups.cor.varnames), ' clusters:\n\n',
                   paste(lapply(1:length(groups.cor.varnames), 
                                function(x){paste0("set ", x, ": ", 
                                                   paste0(groups.cor.varnames[[x]], 
                                                          collapse=", "))}), 
                         collapse = "\n")
    ))
    
    # keep only one variable from each cluster
    corr.vars.drop = setdiff(unique(unlist(groups.cor.varnames)), 
                             sapply(groups.cor.varnames,function(x){
                               ifelse(length(intersect(keepVars, x))>=1,
                                      intersect(keepVars, x)[1], x[1])}))
    
    names(groups.cor.varnames)=unname(sapply(groups.cor.varnames,function(x){
      ifelse(length(intersect(keepVars, x))>=1,
             intersect(keepVars, x)[1], x[1])}))
    
    drop.cor=list()
    drop.cor$vars = corr.vars.drop # list of variables to be dropped
    drop.cor$relation = groups.cor.varnames # relationship among variables
    kept.names = names(drop.cor$relation)
    if(grepl("_continuous|_binary|_categorical",kept.names[1])){
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        gsub( "(_continuous|_binary|_categorical)$", 
              paste0(tag,setid,"\\1"), kept.names[setid] )})
    }else{
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
              paste0( kept.names[setid],tag,setid )   })
    }
    names(drop.cor$setName) = kept.names
    drop.cor$imbalance = dfCheck$ImbalancedVars
    
    return(drop.cor)
  }
  message(paste0('Identified ', 0,
                 ' pairs of correlated variables with r > ',corr.cut))
  cat('\n')
}
## version 2
corrClusterP <- function( df.input, excludeVars=NULL, keepVars=c(""), 
                          all.cut=0.95, patternCor=NULL, pattern.cut=NULL ){
  
  #'  @param all.cut : variables with correlatin above all.cut are clustered
  #'  @param patternCor : regular patterns of variable sets you want to use different correlation cutoffs
  #'  @param pattern.cut : variables matching patternCor with correlatin above pattern.cut are clustered
  
  # minimum correlation we need to calculate
  cor.min = min(c(all.cut, pattern.cut))
  
  # input format  
  if(is.null(excludeVars)){
    
    df <- df.input %>% mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }else{
    
    df <- df.input %>% dplyr::select(-one_of(excludeVars)) %>% 
      mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }
  
  # use fred's function to speed up the calculation
  dfCheck <- refsfsutils::checkotfdf(df, thmin=cor.min)
      
  # variable pairs with cor > cutoff
  if( !is.null(dfCheck$topCorrelations) ){
    
    # print(knitr::kable(head(cor.table),row.names=F))
    # cat('\n')
    
    # Group correlated variables...  
    if(is.null(patternCor)){
      cor.table <- dfCheck$topCorrelations %>% filter( abs(Cor) > all.cut )
    }else{
      cor.table.comm <- dfCheck$topCorrelations %>% filter( abs(Cor) > all.cut )
      cor.table.patt <- lapply(1:length(patternCor), function(x){
        cor.table.x <- dfCheck$topCorrelations %>% 
          filter( grepl(patternCor[x],Var1) &
                    grepl(patternCor[x],Var2) &
                     ( (abs(Cor) > pattern.cut[x]) & (abs(Cor)<=all.cut) )
          ) 
      }) %>% do.call(rbind,.)
      cor.table <- rbind(cor.table.comm, cor.table.patt)
    }
    
    vars.correlation <- data.frame( Var1.pos=match(cor.table$Var1,colnames(df)),
                                    Var2.pos=match(cor.table$Var2,colnames(df))) %>% 
      data.matrix()
    graph.cor = igraph::graph.data.frame(vars.correlation, directed = FALSE)
    
    if(cor.min < 0.95){
      c1 = cluster_fast_greedy(graph.cor)
      c2 = cluster_leading_eigen(graph.cor)
      c1.modularity = modularity(c1)
      c2.modularity = modularity(c2)
    
      if( c1.modularity > c2.modularity ){
        groups.cor = split(unique(as.vector(vars.correlation)),c1$membership)
      }else{
        groups.cor = split(unique(as.vector(vars.correlation)),c2$membership)
      }
    }else{
      groups.cor = split(unique(as.vector(vars.correlation)),igraph::clusters(graph.cor)$membership)
    }
    
    groups.cor.varnames = lapply(groups.cor,FUN=function(list.cor){colnames(df)[list.cor]})
    message(paste0('Identified ', nrow(cor.table), 
                   ' pairs of correlated variables',
                   '. There are in total ', sum(sapply(groups.cor.varnames,length)),
                   ' correlated variables, which can be grouped by ', 
                   length(groups.cor.varnames), ' clusters:\n\n',
                   paste(lapply(1:length(groups.cor.varnames), 
                                function(x){paste0("set ", x, ": ", 
                                                   paste0(groups.cor.varnames[[x]], 
                                                          collapse=", "))}), 
                         collapse = "\n")
    ))
    
    # keep only one variable from each cluster
    corr.vars.drop = setdiff(unique(unlist(groups.cor.varnames)), 
                             sapply(groups.cor.varnames,function(x){
                               ifelse(length(intersect(keepVars, x))>=1,
                                      intersect(keepVars, x)[1], x[1])}))
    
    names(groups.cor.varnames)=unname(sapply(groups.cor.varnames,function(x){
      ifelse(length(intersect(keepVars, x))>=1,
             intersect(keepVars, x)[1], x[1])}))
    
    drop.cor=list()
    drop.cor$vars = corr.vars.drop # list of variables to be dropped
    drop.cor$relation = groups.cor.varnames # relationship among variables
    kept.names = names(drop.cor$relation)
    drop.cor$setName = sapply( 1:length(kept.names), function(setid){
      gsub( "(_continuous|_binary|_categorical)$", 
            paste0("_set",setid,"\\1"), kept.names[setid] )})
    names(drop.cor$setName) = kept.names
    drop.cor$imbalance = dfCheck$ImbalancedVars
    
    return(drop.cor)
  }
  message(paste0('Identified ', 0,
                 ' pairs of correlated variables with r > ',corr.cut))
  cat('\n')
}

corrClusterP2 <- function( df.input, excludeVars=NULL, keepVars=c(""), ncores="local",
                          all.cut=0.95, patternCor=NULL, pattern.cut=0.95, tag="_set" ){
  
  #'  @param all.cut : variables with correlatin above all.cut are clustered
  #'  @param patternCor : regular patterns of variable sets you want to use different correlation cutoffs
  #'  @param pattern.cut : variables matching patternCor with correlatin above pattern.cut are clustered
  
  # minimum correlation we need to calculate
  cor.min = min(c(all.cut, pattern.cut))
  
  # input format  
  if(is.null(excludeVars)){
    
    df <- df.input %>% mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }else{
    
    df <- df.input %>% dplyr::select(-one_of(excludeVars)) %>% 
      mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }
  
  # use fred's function to speed up the calculation
  dfCheck <- refsfsutils::checkotfdf(df, thmin=cor.min, ncores = ncores)
  
  # variable pairs with cor > cutoff
  if( !is.null(dfCheck$topCorrelations) ){
    
    # print(knitr::kable(head(cor.table),row.names=F))
    # cat('\n')
    
    # Group correlated variables...  
    if(is.null(patternCor)){
      cor.table <- dfCheck$topCorrelations %>% filter( abs(Cor) > all.cut )
    }else{
      cor.table.comm <- dfCheck$topCorrelations %>% filter( abs(Cor) > all.cut )
      cor.table.patt <- lapply(1:length(patternCor), function(x){
        cor.table.x <- dfCheck$topCorrelations %>% 
          filter( grepl(patternCor[x],Var1) &
                    grepl(patternCor[x],Var2) &
                    ( (abs(Cor) > pattern.cut[x]) & (abs(Cor)<=all.cut) )
          ) 
      }) %>% do.call(rbind,.)
      cor.table <- rbind(cor.table.comm, cor.table.patt)
    }
    
    # construct the correlation matrix for clustering
    cor.table.rev = cor.table
    colnames(cor.table.rev) = c("Var2","Var1","Cor")
    cor.pre = spread(rbind(cor.table,cor.table.rev) %>% 
                       mutate(Cor=abs(Cor)),key=Var2, value=Cor, fill=0) %>% 
      column_to_rownames(var="Var1") %>% as.matrix()
    un1 <- unique(c(colnames(cor.pre), rownames(cor.pre)))
    cor.vars <- matrix(0, length(un1), length(un1), dimnames = list(un1, un1))
    cor.vars[rownames(cor.pre), colnames(cor.pre)] <- cor.pre
    cor.vars = as.data.frame(cor.vars)
    
    #cor.vars = cor(df %>% select(one_of(unique(c(cor.table$Var1,cor.table$Var2)))))
    
    # clustering
    cor.class <- as.dist(1 - cor.vars) %>% hclust(.,method="complete") %>% 
      cutree(.,h=(1-cor.min))

    groups.cor = split(names(cor.class), cor.class)
    groups.cor.varnames = groups.cor[lapply(groups.cor, function(x){length(x)>1}) %>% unlist]
    names(groups.cor.varnames)=1:length(groups.cor.varnames)
    
    message(paste0('Identified ', nrow(cor.table), 
                   ' pairs of correlated variables',
                   '. There are in total ', length(unique(unlist(groups.cor.varnames))),
                   ' unique variables, which can be grouped by ', 
                   length(groups.cor.varnames), ' clusters:\n\n',
                   paste(lapply(1:length(groups.cor.varnames), 
                                function(x){paste0("set ", x, ": ", 
                                                   paste0(groups.cor.varnames[[x]], 
                                                          collapse=", "))}), 
                         collapse = "\n")
    ))
    
    # keep only one variable from each cluster
    corr.vars.drop = setdiff(unique(unlist(groups.cor.varnames)), 
                             sapply(groups.cor.varnames,function(x){
                               ifelse(length(intersect(keepVars, x))>=1,
                                      intersect(keepVars, x)[1], x[1])}))
    
    names(groups.cor.varnames)=unname(sapply(groups.cor.varnames,function(x){
      ifelse(length(intersect(keepVars, x))>=1,
             intersect(keepVars, x)[1], x[1])}))
    
    drop.cor=list()
    drop.cor$vars = corr.vars.drop # list of variables to be dropped
    drop.cor$relation = groups.cor.varnames # relationship among variables
    kept.names = names(drop.cor$relation)
    if(grepl("_continuous|_binary|_categorical",kept.names[1])){
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        gsub( "(_continuous|_binary|_categorical)$", 
              paste0(tag,setid,"\\1"), kept.names[setid] )})
    }else{
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        paste0( kept.names[setid],tag,setid )   })
    }
    names(drop.cor$setName) = kept.names
    drop.cor$imbalance = dfCheck$ImbalancedVars
    
    return(drop.cor)
  }
  message(paste0('Identified ', 0,
                 ' pairs of correlated variables with r > ',corr.cut))
  cat('\n')
}

# refsfsutils::checkotfdf was replace with fastCorTable for wider application
corrClusterP3 <- function( df.input, excludeVars=NULL, keepVars=c(""), 
                           all.cut=0.95, patternCor=NULL, pattern.cut=0.95, tag="_set" ){
  
  #'  @param all.cut : variables with correlatin above all.cut are clustered
  #'  @param patternCor : regular patterns of variable sets you want to use different correlation cutoffs
  #'  @param pattern.cut : variables matching patternCor with correlatin above pattern.cut are clustered
  
  # minimum correlation we need to calculate
  cor.min = min(c(all.cut, pattern.cut))
  
  # input format  
  if(is.null(excludeVars)){
    
    df <- df.input %>% mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }else{
    
    df <- df.input %>% dplyr::select(-one_of(excludeVars)) %>% 
      mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }
  
  # use fred's function to speed up the calculation
  topCorrelations <- suppressWarnings(fastCorTable(df, thmin = cor.min, 
                                                   nblocks = 20))
  
  # variable pairs with cor > cutoff
  if( !is.null(topCorrelations) ){
    
    # print(knitr::kable(head(cor.table),row.names=F))
    # cat('\n')
    
    # Group correlated variables...  
    if(is.null(patternCor)){
      cor.table <- topCorrelations %>% filter( abs(Cor) > all.cut )
    }else{
      cor.table.comm <- topCorrelations %>% filter( abs(Cor) > all.cut )
      cor.table.patt <- lapply(1:length(patternCor), function(x){
        cor.table.x <- topCorrelations %>% 
          filter( grepl(patternCor[x],Var1) &
                    grepl(patternCor[x],Var2) &
                    ( (abs(Cor) > pattern.cut[x]) & (abs(Cor)<=all.cut) )
          ) 
      }) %>% do.call(rbind,.)
      cor.table <- rbind(cor.table.comm, cor.table.patt)
    }
    
    # construct the correlation matrix for clustering
    cor.table.rev = cor.table
    colnames(cor.table.rev) = c("Var2","Var1","Cor")
    cor.pre = spread(rbind(cor.table,cor.table.rev) %>% 
                       mutate(Cor=abs(Cor)),key=Var2, value=Cor, fill=0) %>% 
      column_to_rownames(var="Var1") %>% as.matrix()
    un1 <- unique(c(colnames(cor.pre), rownames(cor.pre)))
    cor.vars <- matrix(0, length(un1), length(un1), dimnames = list(un1, un1))
    cor.vars[rownames(cor.pre), colnames(cor.pre)] <- cor.pre
    cor.vars = as.data.frame(cor.vars)
    
    #cor.vars = cor(df %>% select(one_of(unique(c(cor.table$Var1,cor.table$Var2)))))
    
    # clustering
    cor.class <- as.dist(1 - cor.vars) %>% hclust(.,method="complete") %>% 
      cutree(.,h=(1-cor.min))
    
    groups.cor = split(names(cor.class), cor.class)
    groups.cor.varnames = groups.cor[lapply(groups.cor, function(x){length(x)>1}) %>% unlist]
    names(groups.cor.varnames)=1:length(groups.cor.varnames)
    
    message(paste0('Identified ', nrow(cor.table), 
                   ' pairs of correlated variables',
                   '. There are in total ', length(unique(unlist(groups.cor.varnames))),
                   ' unique variables, which can be grouped by ', 
                   length(groups.cor.varnames), ' clusters:\n\n',
                   paste(lapply(1:length(groups.cor.varnames), 
                                function(x){paste0("set ", x, ": ", 
                                                   paste0(groups.cor.varnames[[x]], 
                                                          collapse=", "))}), 
                         collapse = "\n")
    ))
    
    # keep only one variable from each cluster
    corr.vars.drop = setdiff(unique(unlist(groups.cor.varnames)), 
                             sapply(groups.cor.varnames,function(x){
                               ifelse(length(intersect(keepVars, x))>=1,
                                      intersect(keepVars, x)[1], x[1])}))
    
    names(groups.cor.varnames)=unname(sapply(groups.cor.varnames,function(x){
      ifelse(length(intersect(keepVars, x))>=1,
             intersect(keepVars, x)[1], x[1])}))
    
    drop.cor=list()
    drop.cor$vars = corr.vars.drop # list of variables to be dropped
    drop.cor$relation = groups.cor.varnames # relationship among variables
    kept.names = names(drop.cor$relation)
    if(grepl("_continuous|_binary|_categorical",kept.names[1])){
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        gsub( "(_continuous|_binary|_categorical)$", 
              paste0(tag,setid,"\\1"), kept.names[setid] )})
    }else{
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        paste0( kept.names[setid],tag,setid )   })
    }
    names(drop.cor$setName) = kept.names

    return(drop.cor)
  }
  message(paste0('Identified ', 0,
                 ' pairs of correlated variables with r > ',corr.cut))
  cat('\n')
}

corrClusterWGCNA <- function( df.input, excludeVars=NULL, keepVars=c(""), 
                           all.cut=0.95, patternCor=NULL, pattern.cut=0.95, tag="_set" ){
  
  #'  @param all.cut : variables with correlatin above all.cut are clustered
  #'  @param patternCor : regular patterns of variable sets you want to use different correlation cutoffs
  #'  @param pattern.cut : variables matching patternCor with correlatin above pattern.cut are clustered
  
  # minimum correlation we need to calculate
  cor.min = min(c(all.cut, pattern.cut))
  
  # input format  
  if(is.null(excludeVars)){
    
    df <- df.input %>% mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }else{
    
    df <- df.input %>% dplyr::select(-one_of(excludeVars)) %>% 
      mutate_at( vars(matches("_categorical|_binary")), as.factor) %>% 
      data.matrix() %>% as.data.frame()
  }
  
  # use fred's function to speed up the calculation
  topCorrelations <- WGCNA::cor(df, use = "pairwise.complete.obs")
  
  # Get the upper triangle of the correlation matrix, excluding the diagonal
  topCorrelations[lower.tri(topCorrelations, diag = TRUE)] <- NA
  # Convert the upper triangle matrix to a long format data frame
  topCorrelations <- as.data.frame(as.table(topCorrelations)) %>%
    filter(!is.na(Freq))  # Remove NA values
  # Rename columns for clarity
  colnames(topCorrelations) <- c("Var1", "Var2", "Cor")
  topCorrelations <- topCorrelations %>% arrange(desc(Cor)) 
  
  # variable pairs with cor > cutoff
  if( !is.null(topCorrelations) ){
    
    # print(knitr::kable(head(cor.table),row.names=F))
    # cat('\n')
    
    # Group correlated variables...  
    if(is.null(patternCor)){
      cor.table <- topCorrelations %>% filter( abs(Cor) > all.cut )
    }else{
      cor.table.comm <- topCorrelations %>% filter( abs(Cor) > all.cut )
      cor.table.patt <- lapply(1:length(patternCor), function(x){
        cor.table.x <- topCorrelations %>% 
          filter( grepl(patternCor[x],Var1) &
                    grepl(patternCor[x],Var2) &
                    ( (abs(Cor) > pattern.cut[x]) & (abs(Cor)<=all.cut) )
          ) 
      }) %>% do.call(rbind,.)
      cor.table <- rbind(cor.table.comm, cor.table.patt)
    }
    
    # construct the correlation matrix for clustering
    cor.table.rev = cor.table
    colnames(cor.table.rev) = c("Var2","Var1","Cor")
    cor.pre = spread(rbind(cor.table,cor.table.rev) %>% 
                       mutate(Cor=abs(Cor)),key=Var2, value=Cor, fill=0) %>% 
      column_to_rownames(var="Var1") %>% as.matrix()
    un1 <- unique(c(colnames(cor.pre), rownames(cor.pre)))
    cor.vars <- matrix(0, length(un1), length(un1), dimnames = list(un1, un1))
    cor.vars[rownames(cor.pre), colnames(cor.pre)] <- cor.pre
    cor.vars = as.data.frame(cor.vars)
    
    #cor.vars = cor(df %>% select(one_of(unique(c(cor.table$Var1,cor.table$Var2)))))
    
    # clustering
    cor.class <- as.dist(1 - cor.vars) %>% hclust(.,method="complete") %>% 
      cutree(.,h=(1-cor.min))
    
    groups.cor = split(names(cor.class), cor.class)
    groups.cor.varnames = groups.cor[lapply(groups.cor, function(x){length(x)>1}) %>% unlist]
    names(groups.cor.varnames)=1:length(groups.cor.varnames)
    
    message(paste0('Identified ', nrow(cor.table), 
                   ' pairs of correlated variables',
                   '. There are in total ', length(unique(unlist(groups.cor.varnames))),
                   ' unique variables, which can be grouped by ', 
                   length(groups.cor.varnames), ' clusters:\n\n',
                   paste(lapply(1:length(groups.cor.varnames), 
                                function(x){paste0("set ", x, ": ", 
                                                   paste0(groups.cor.varnames[[x]], 
                                                          collapse=", "))}), 
                         collapse = "\n")
    ))
    
    # keep only one variable from each cluster
    corr.vars.drop = setdiff(unique(unlist(groups.cor.varnames)), 
                             sapply(groups.cor.varnames,function(x){
                               ifelse(length(intersect(keepVars, x))>=1,
                                      intersect(keepVars, x)[1], x[1])}))
    
    names(groups.cor.varnames)=unname(sapply(groups.cor.varnames,function(x){
      ifelse(length(intersect(keepVars, x))>=1,
             intersect(keepVars, x)[1], x[1])}))
    
    drop.cor=list()
    drop.cor$vars = corr.vars.drop # list of variables to be dropped
    drop.cor$relation = groups.cor.varnames # relationship among variables
    kept.names = names(drop.cor$relation)
    if(grepl("_continuous|_binary|_categorical",kept.names[1])){
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        gsub( "(_continuous|_binary|_categorical)$", 
              paste0(tag,setid,"\\1"), kept.names[setid] )})
    }else{
      drop.cor$setName = sapply( 1:length(kept.names), function(setid){
        paste0( kept.names[setid],tag,setid )   })
    }
    names(drop.cor$setName) = kept.names
    
    return(drop.cor)
  }
  message(paste0('Identified ', 0,
                 ' pairs of correlated variables with r > ',corr.cut))
  cat('\n')
}


# 1.8. replace sets with set variables in a data frame

#' @description 
#' This function replaces the rows of variables with set tags by the variables in the
#' corresponding sets. Extra columns containing information of set
#'
#' @param simDf data frame(with input and output columns) from the simulation network
#' @param corrRes output of corrCluster*
#' @return A data frame with addtional variable rows from the set and extra columns 
#'         containing set IDs of input and output
#' @export
addSetTerms <- function(simDf, corrRes){
  # set variables in both input and output
  input_set_var = grep("_set\\d+_", simDf$input, value=T) %>% unique
  output_set_var = grep("_set\\d+_", simDf$output, value=T) %>% unique
  
  # creat mapping table for set variables in both input and output
  input_setHash <- lapply( input_set_var,
                           function(input_set){
                             setName=input_set
                             setID=gsub(".*_set(\\d+)_.*","\\1",input_set) %>% as.integer()
                             setDf <- data.frame(setName, 
                                                 input_setVars=corrRes$relation[[setID]], 
                                                 input_setID=setID, stringsAsFactors = F)
                             }) %>% do.call(rbind,.)
  
  output_setHash <- lapply( output_set_var,
                           function(output_set){
                             setName=output_set
                             setID=gsub(".*_set(\\d+)_.*","\\1",output_set) %>% as.integer()
                             setDf <- data.frame(setName, 
                                                 output_setVars=corrRes$relation[[setID]], 
                                                 output_setID=setID, stringsAsFactors = F)
                           }) %>% do.call(rbind,.)
  
  # handle the situation where there's on set variable in input or output
  if(is.null(input_setHash)){
    input_setHash = data.frame(setName=NA, input_setVars=NA, input_setID="no_set")
  }
  if(is.null(output_setHash)){
    output_setHash = data.frame(setName=NA, output_setVars=NA, output_setID="no_set")
  }
  
  # where the replacement happens
  simDf_set <- simDf %>%
    left_join(input_setHash, by=c("input"="setName")) %>% 
    left_join(output_setHash, by=c("output"="setName")) %>% 
    mutate(input_vars=ifelse( is.na(input_setVars), input, input_setVars ),
           output_vars=ifelse( is.na(output_setVars), output, output_setVars),
           input_setID=ifelse( is.na(input_setID),"no_set", input_setID),
           output_setID=ifelse( is.na(output_setID),"no_set", output_setID)) %>% 
    select(-input_setVars, -output_setVars)
  
  return(simDf_set)
}

#' @description 
#' This function calculates in degree, out degree, and total degree
#' @param netDf data frame with input and output columns
#' @return A data frame with degree information
#' @export
getDegree <- function(netDf){
  degree <- netDf %>% count(input) %>% 
    full_join( netDf %>% count(output), by=c("input"="output") ) %>% 
    mutate(out_degree=ifelse(is.na(n.x), 0, n.x),
           in_degree=ifelse(is.na(n.y), 0, n.y)) %>% 
    mutate(total_degree=in_degree+out_degree) %>% 
    select(-n.x,-n.y) %>% arrange(desc(total_degree))
  return(degree)
}

#' @description 
#' For genes enriched in the specified process|pathway, identify from the model data
#' frame all corresponding variables. The variable could be the enriched gene or its 
#' clustering set contains the enriched gene.
#' @param geneNamesGO regular expression patterns of genes from enrichment analysis
#'                    e.g  _geneA_|_geneB_|_geneC_
#' @param outcomeDriverList outcome drivers from alltoall simulation 
#' @param corrRes output of corrCluster. corrRes$relation is a hash table containing 
#'                the set information, keys are the variables(with set tags) kept for 
#'                sets and values are the variables highly correlated with the 
#'                corresponding keys
#' @return A list with two objects: terms contain all variables for visulization
#'                                  set_terms contains all variables in sets
#' @export
getGOSetTerms <- function(geneNamesGO=NULL, outcomeDriverList, corrRes ){
  
  matched_terms <- grep(geneNamesGO, outcomeDriverList, value=T)
  
  names(corrRes$relation) = corrRes$setName[names(corrRes$relation)]
  
  in_set <- lapply(names(corrRes$relation),
                   function(x){sum(grepl(geneNamesGO,corrRes$relation[[x]]))>0 &
                                   x%in%outcomeDriverList
                     }) %>% unlist
  set_terms <- corrRes$relation[in_set]
  
  GO_terms = list()
  GO_terms$terms = union(matched_terms, names(set_terms))
  GO_terms$set = set_terms
  
  return(GO_terms)
}



# 1.8 heatmap
# 1.8.1 
# corrplot::corrplot(cor(df),method="color",type="lower", diag=F, tl.col="white")

# 1.8 missing pattern related to the outcome
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

# 1.9 plot treatment trajectory
trtTrajectoryPlot <- function(dfRegimen, dfEvent, keepID=NULL){
  
  # data
  ## regimen info
  regimen <- dfRegimen %>% 
    setNames(c("ID", "LoT", "Start_t", "End_t")) %>% 
    filter(ID%in%keepID) %>% 
    add_count(ID) %>% arrange(desc(n)) %>%
    mutate_at(vars(Start_t, End_t), as.numeric) %>% 
    mutate(LoT = as.factor(LoT),
           ID = factor(ID,levels=unique(ID)))
  ## event info
  event <- dfEvent %>% 
    setNames(c("ID", "eventType", "time")) %>% 
    filter(ID%in%keepID) %>% 
    drop_na(time) %>% 
    mutate_at(vars(time), as.numeric) %>% 
    mutate(ID = factor(ID,levels=unique(ID)))
  
  # plot
  g_desc <- ggplot() +
    ## lines
    geom_segment(data=regimen,aes(x=Start_t, xend=End_t,
                                  y=ID, yend=ID, color=LoT),
                 linetype=1,size=2) +
    ## events
    geom_point(event,
               mapping=aes(x=time,y=ID,
                           shape=eventType),size=2.5)+
    scale_shape_manual(values=c(1:length(unique(event$eventType)))) +
    
    ylab("Patients") +
    labs(color="Lines",shape="Event type") +
    theme_classicM()
  
  return(g_desc)
}








# 1.10 data transformation

library(pracma)
library(car)

#'  Normality transformation of continuous variables
#'
#'  \code{dataTransform} returns data frame with columns data transformed if necessary
#'
#'  @details
#'  Assess per data column for normality based on Shapiro-Wilks normality test.  If the null for raw
#'  data column is accepted, log transform only if its transform p-value is less than the raw p-value, otherwise 
#'  maintain as raw values. If the null is rejected for normality, then the log transform and Box-cox transform
#'  is checked for normality.  If both transforms rejects null for normality and both transforms'
#'  p-values are still smaller (more significant) than the square root of the raw p-value, then 
#'  no data transform.  If log transform accepts null or log transform p-value is greater (less significant) than 
#'  the square root of the raw p-value, then log transform, otherwise 
#'  Box-cox transform.  Specified transform is also allowed for output data transform.
#'
#'  @param df : input data frame for data columns transformation
#'  @param pValThres : p-value threshold for null rejection (default: 1e-21)
#'  @param transform : perform transformation, otherwise output a summary of 
#'                      which data columns are to be transformed by what 
#'                      transforamtion (default: FALSE)
#'  @param chooseTransform : dplyr::select a transformation, "Box-Cox" or "Log" ( default is NULL )
#'  @param BC.lambda : when chooseTransform=="Box-Cox",there is an option to dplyr::select the lambda also.
#'  @param rseed : random seed for sampling 5000 values from data column if 
#'                      >5000 data values are present
#'
#'  @return transformed data frame (transform=TRUE) or a summary data frame
#'              data columns that need to be transformed (transform=FALSE)
#'
#'  @export
#'

dataTransform <- function(df, pValThres=1e-21, transform=FALSE, 
                          chooseTransform=NULL, BC.lambda=NULL, rseed=5067){
  
  transformCheck <- function(varColumn.in,varName){
    varColumn <- as.numeric(as.character(varColumn.in))
    ## Shift the range of data if the min data value is <= 1
    ## by delta so that no value is ever < 1
    if ( min(varColumn,na.rm=T) <= 1){
      delta <- 1.01 - min(varColumn,na.rm=T)
      var.adj <- varColumn + delta
    }else{
      var.adj <- varColumn
    }
    
    ## Box-cox transform
    bc.test <- car::powerTransform(var.adj)
    var.bc <- car::bcPower(var.adj,round(bc.test$lam))
    
    ## if lambda is provided, use that lambda for Box-cox transformation
    if ( !is.null(BC.lambda) ){
      var.bc.chooseLambda <- car::bcPower(var.adj,BC.lambda)
      chooseLambda <- BC.lambda
      
      ## let optimal lambda be estimated
    }else{
      var.bc.chooseLambda <- var.bc
      chooseLambda <- round(bc.test$lam)
    }
    
    ## Log transform
    var.log <- log2(var.adj)
    
    ## P-values for Box-cox transform, log transform, and raw
    ## shapiro-wilkes normality test
    set.seed(rseed)
    sampling <- function(in.vec){
      in.vec1 <- in.vec[!is.na(in.vec)]
      if ( length(in.vec1) > 5000 ){
        return(sample(in.vec1,5000))
      }else{
        return(in.vec1)
      }
    }
    ## p-value significance from Shapiro-Wilks normality test
    bc.pval <- shapiro.test(sampling(var.bc))$p.value
    log.pval <- shapiro.test(sampling(var.log))$p.value
    raw.pval <- shapiro.test(sampling(varColumn))$p.value
    
    ## CHOOSE TRANSFORMATION : User Preference
    if ( !is.null(chooseTransform) ){
      
      if ( chooseTransform=="Box-Cox" ){
        dataColumn <- setNames(data.frame(x=var.bc.chooseLambda),paste0(varName,
                                                                        "_BC_",gsub("-","neg",chooseLambda)))
        return(dataColumn)
        
      }else if ( chooseTransform=="Log" ){
        dataColumn <- setNames(data.frame(x=var.log),paste0(varName,"_LOG"))
        return(dataColumn)
        
      }else{
        print("Do not recognize chooseTransform=",chooseTransform," dplyr::selection, carrying out standard algorithm dplyr::selection")
      }
      
    }
    
    ## accept null  
    if ( raw.pval >= pValThres){
      ## accepted null , but log transform anyways if significance
      ## is less than raw values
      if ( log.pval > raw.pval ){
        decis <- "LOG"
        dataColumn <- setNames(data.frame(x=var.log),paste0(varName,
                                                            "_LOG"))
      }else{
        decis <- "RAW"
        dataColumn <- setNames(data.frame(x=varColumn),paste0(varName,
                                                              ""))
      }
      ## no data transform if both log tranform and Box-cox transform
      ## rejects null and log transform + Box-cox transform are both still more significant than the square root of raw p-value
    }else if ( bc.pval < pValThres & log.pval < pValThres & bc.pval < raw.pval/bc.pval & log.pval < raw.pval/log.pval){
      decis <- "RAW"
      dataColumn <- setNames(data.frame(x=varColumn),paste0(varName,
                                                            ""))
      ## rejected null for raw values but log transform accepts null OR log transform p-value is greater than square root of raw p-value
    }else if ( log.pval >= pValThres | log.pval > raw.pval/log.pval ){
      decis <- "LOG"
      dataColumn <- setNames(data.frame(x=var.log),paste0(varName,
                                                          "_LOG"))
      ## Both raw values and log transform reject null, then Box-cox transform
    }else{
      decis <- "BC"
      dataColumn <- setNames(data.frame(x=var.bc),paste0(varName,
                                                         "_BC_",gsub("-","neg",round(bc.test$lam))))
    }
    
    if ( transform ){ ## transformed data
      return(dataColumn)
      
    }else{ ## transform summary data
      return(data.frame(var=varName,BC=bc.pval,LOG=log.pval,
                        RAW=raw.pval,decis=decis,stringsAsFactors=F))
    }
    
  }
  
  if ( transform ){ ## transformed data frame
    return(bind_cols(lapply(colnames(df),function(x)
      transformCheck(df[,x],x))))
    
  }else{ ## transform summary data frame
    
    return(bind_rows(lapply(colnames(df),function(x)
      transformCheck(df[,x],x))))
    
  }
  
}

logTransform <- function(data, base=2){
  
  data <- as.numeric(as.character(data))
  ## Shift the range of data if the min data value is <= 1
  ## by delta so that no value is ever < 1
  if ( min(data,na.rm=T) <= 1){
    delta <- 1.01 - min(data,na.rm=T)
    var.adj <- data + delta
  }else{
    var.adj <- data
  }
  
  dataLog = log(var.adj, base) 
  
  return(dataLog)
}




# 2. outlier
# 2.1 outlier detector
outlierDetect <- function(varList, nMAD = 6){
  
  varValue <- varList[!is.na(varList)]
  
  lowerBound <- median(varValue) - nMAD*mad(varValue)
  upperBound <- median(varValue) + nMAD*mad(varValue)
  
  sum((varValue < lowerBound | varValue > upperBound) & mad(varValue) != 0) > 0
}

# 2.2 winsorization
#'  Winsorize continuous variables 
#'
#'  \code{winsorize} return winsorized continuous variable.
#'
#'  @details
#'  The values of the continuous variable is checked to see if it is greater than 
#'  some set number of MADs (median absolute deviations) from its median.  If so, 
#'  the value is converted to the set number of MADs from median (winsorization).
#'
#'  @param in.data :  data values of continuous variable
#'  @param nMAD : number of median absolute deviations (default : 5)
#'  @param countWinsorize : instead of peforming the actual winsorization on the data,
#'                      summarize the number of necessary winsorized values (default : FALSE)
#'
#'  @return  winsorized continuous variable data values (countWinsorize=FALSE) or
#'              summary of the number of winsorizable values (countWinsorize=TRUE)
#'
#'  @export
#'

winsorize <- function(in.data1, nMAD = 5,countWinsorize = FALSE){
  
  in.data <- as.numeric(as.character(unlist(in.data1)))
  
  upp.lim <- median(in.data,na.rm=T) + nMAD*mad(in.data,na.rm=T)
  low.lim <- median(in.data,na.rm=T) - nMAD*mad(in.data,na.rm=T)
  
  ## summary of winsorizable values
  if ( countWinsorize ){
    ## MAD==0, no winsorization
    if ( mad(in.data,na.rm=T)==0 ){
      return(0)
    }else{
      ## count number of values above or below upp.lim or low.lim, respectively.
      no.winsor <- length(which(in.data > upp.lim | in.data < low.lim))
      return(no.winsor)
    }
    
  }else{
    
    ## MAD==0, no need for winsorization
    if ( mad(in.data,na.rm=T)==0 ){
      
      return(in.data)
    }else{
      
      ## winsorize data value to upp.lim if above or to low.lim if below  
      winsor.data <- sapply(in.data,function(x)if (is.na(x)){NA}
                            else if ( x > upp.lim){upp.lim}else if ( x < low.lim ){
                              low.lim}else{x})
      
      return(winsor.data)
    }
    
  }
  
}


# Transformation and winsorization

transformWinsorize <- function(df, excludeVars = NULL, con_cut = 15){
  
  df2Imputed <- df %>%
    dplyr::select(-one_of(excludeVars)) %>%
    # identify vars with outliers
    dplyr::select_if(~is.NumCon(., con_cut = con_cut)) %>% 
    dplyr::select_if(outlierDetect)
  
  if (ncol(df2Imputed) == 0){
    # stop when no variable needs to be transformed
    message('No variable needs to be transformed or winsorized')
    return(NULL)
  }else{
    message(paste(ncol(df2Imputed),'variables need to be transformed or winsorized'))
    
    # transformation and then winsorization
    dfImputed <- df2Imputed %>%  
      # transform and winsorize those vars with outliers
      dataTransform(transform = T) %>% 
      # add suffix to winsorized variables
      # rename_if(outlierDetect,~paste0(.,'winsor')) may work using advanced dplyr
      setNames(sapply(names(.),function(x){
        ifelse(x%in%names(.)[sapply(.,outlierDetect)],
               paste0(x,'_winsor'),x)})) %>% 
      mutate_if(outlierDetect, winsorize)
    
    df[,gsub('_LOG.*|_BC_neg.*|_winsor', '', colnames(dfImputed))] <- 
      dfImputed[,colnames(dfImputed)]
    
    varTraned <- data.frame(var2tran = colnames(df2Imputed),
                            varTraned = colnames(dfImputed), stringsAsFactors = F)
    # result
    listTraned <- list()
    listTraned$dfTraned <- df
    listTraned$varTraned <- varTraned
    
    return(listTraned)
  }
}


# Part III. Analysis

# biplot
biplotByGroup <- function(df, groups=NULL, show_outliers=F){
  
  ir.pca <- prcomp(df, center = TRUE, scale. = TRUE)
  
  # 2D-PC plot with package rgl
  if(!is.null(groups)){
    g <- ggbiplot::ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                            groups = groups %>% as.character,
                            ellipse = T, circle = F,
                            var.axes = F)
  }else{
    g <- ggbiplot::ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                            ellipse = F, circle = F,
                            var.axes = F)
  }
  if(show_outliers){
    U <- ir.pca$x[,1:2] %>% data.frame
    outlier_ID <- apply(U, 2, function(x) which( abs(x - mean(x)) > (3 * sd(x)) )) %>% 
      Reduce(union, .)
    print(rownames(U)[outlier_ID])
    
    g <- g + geom_point(data=U[outlier_ID,], aes(x=PC1, y=PC2), colour="red", size=1)
  } 
 
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  g <- g + theme(panel.background = element_blank()) # remove the grey background
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # remove the grid
  g<- g + theme(axis.line.x = element_line(color="black", size = 0.5),
                axis.line.y = element_line(color="black", size = 0.5)) 
  return(g)
}


# 1. Univariate cox model for continuous and discrete variables

# library("survcomp")
# Function IV: univariate analysis
mulUniCox <- function(mydata, timen, statusn, excludeVars=NULL, pvalue_cut=1){
  
  time <- mydata[,timen] %>% unlist()
  status <- mydata[,statusn] %>% unlist() %>% as.numeric()
  varData <- apply( mydata[,!colnames(mydata)%in%c(timen, statusn, excludeVars)],2,scale)
  
  # formulas for each variable
  covariates <- colnames(varData)
  
  mydata[,covariates] <- varData
  
  uniFormulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~scale(', x,')')))
  
  uniModels <- lapply(uniFormulas, function(x){coxph(x, data = mydata)})
  # Extract data
  uniResults <- 
    lapply(uniModels, function(x){
      
      x.summary <- summary(x)
      
      # concordance index computed in a way different from coxph
      # look at Doris Gao's Blog for more details
      if(is.null(x$na.action)){
        cindex <- concordance.index(predict(x), surv.time=time, surv.event=status, 
                                    na.rm=T)
      }else{
        cindex <- concordance.index(predict(x), 
                                    surv.time=time[-as.vector(x$na.action)], 
                                    surv.event=status[-as.vector(x$na.action)])
      }
      
      
      Variablen = gsub("scale\\(|\\)","",rownames(x.summary$coefficients))
      res <- data.frame(
        
        Variable = ifelse(grepl("binary\\w+|categorical", Variablen),
                          gsub('(.*binary|.*categorical)(.*)','\\1 = \\2', Variablen),
                          Variablen),
        
        Pts_n = x.summary$n,
        Event_n = x.summary$nevent,
        Event_prop = x.summary$nevent / x.summary$n,
        
        HR = signif(x.summary$conf.int[,1], digits=3),
        HR_95percent_CI = paste0("(", signif(x.summary$conf.int[,3],digits=3), "-", 
                                 signif(x.summary$conf.int[,4], digits=3), ")"),
        
        Concordance = signif(cindex$c.index, digits=3),
        C_Index_95percent_CI = paste0("(", signif(cindex$lower, digits=3), "-", 
                                      signif(cindex$upper, digits=3), ")"),
        
        p_value = signif(x.summary$coefficients[,5], digits=3),
        
        FDR = signif(p.adjust(x.summary$coefficients[,5], 
                              method="fdr", n=length(covariates)), digits=3),
        
        stringsAsFactors = F
      ) 
      # remove rownames
      rownames(res) <-  c()  
      return(res)
    })
  
  uniResultsDF <- do.call(rbind, uniResults) 
  rownames(uniResultsDF) <- c()
  
  # filtering by p-value
  uniResultsDF %<>% filter(p_value < pvalue_cut) %>% arrange(p_value) %>% 
    dplyr::select(-Concordance, -C_Index_95percent_CI)
  
  return(uniResultsDF)
}
mulUniLogLinear <- function(mydata, target, excludeVars=NULL, pvalue_cut=1){
  
  # formulas for each variable
  covariates <- colnames(mydata)[ !colnames(mydata)%in%c(target, excludeVars) ]
  
  uniResults <- lapply(covariates,
                       
                       function(var) {
                         
                         formula  <- as.formula(paste(target, "~", var))
                         if( length(table(mydata[,target]))==2 ){
                           res.regression <- glm(formula, data = mydata, family = binomial)
                           oddsRatio <- exp(cbind(OR = coef(res.regression), suppressMessages(confint((res.regression)))))
                           
                           x.summary <- summary(res.regression)
                           Variablen <- rownames(x.summary$coefficients)[-1]
                           
                           res <- data.frame(
                             
                             Variable = ifelse(grepl("binary\\w+|categorical", Variablen),
                                               gsub('(.*binary|.*categorical)(.*)','\\1 = \\2', Variablen),
                                               Variablen),
                             odds_ratio = signif(oddsRatio[2,1], digits=3),
                             CI95 = paste0(signif(oddsRatio[2,2],digits=3),",",
                                            signif(oddsRatio[2,3],digits=3)),
                             p_value = signif(x.summary$coefficients[,4][-1], digits=3),
                             FDR = signif(p.adjust(x.summary$coefficients[,4][-1], 
                                          method="fdr", n=length(covariates)), digits=3),
                              stringsAsFactors = F
                             )
                         }else{
                           res.regression <- lm(formula, data = mydata)
                           
                           x.summary <- summary(res.regression)
                           Variablen <- rownames(x.summary$coefficients)[-1]
                           adj.r.squared = signif(x.summary$adj.r.squared, digits=3)
                           
                           res <- data.frame(
                             
                             Variable = ifelse(grepl("binary\\w+|categorical", Variablen),
                                               gsub('(.*binary|.*categorical)(.*)','\\1 = \\2', Variablen),
                                               Variablen),
                             coeff.estimate = signif(x.summary$coefficients[,1][-1], digits=3),
                             coeff.std = signif(x.summary$coefficients[,2][-1], digits=3),
                             adj.r.squared = ifelse( adj.r.squared>0, adj.r.squared,0 ),
                             p_value = signif(x.summary$coefficients[,4][-1], digits=3),
                             FDR = signif(p.adjust(x.summary$coefficients[,4][-1], 
                                                   method="fdr", n=length(covariates)), digits=3),
                             stringsAsFactors = F
                           )
                         }
                       })

  uniResultsDF <- do.call(rbind, uniResults) 
  rownames(uniResultsDF) <- c()

  # filtering by p-value
  uniResultsDF %<>% filter(p_value < pvalue_cut) %>% arrange(p_value)

  return(uniResultsDF)
}

mulUniRepeatMeasure <- function(mydata, Time, Treatment, Subject, 
                                covariates=NULL,
                                response=NULL,excludeVars=NULL){
  
  # formulas for each variable
  targets <- colnames(mydata)[ !colnames(mydata)%in%c(Time, Treatment, Subject, covariates,
                                                      response, excludeVars) ]
  ncovars = length(covariates)

  if(is.null(response)){
    uniResults <- 
      parallel::mclapply(targets,
             function(target) {
               if(is.null(covariates)){
                 formula_fix  <- as.formula(paste0(target, "~",
                                                   Time,"*",Treatment))
               }else{
                 formula_fix  <- as.formula(paste0(target, "~", 
                                                 paste0(covariates,collapse = "+"), 
                                                 "+",Time,"*",Treatment))
               }
               formula_random <- as.formula(paste0("random= ~1|",Subject))
               randomInterModel <- nlme::lme(formula_fix, random=formula_random, 
                                             data=mydata, na.action=na.omit) %>% anova()
               # fm1BW.lmeSphere <- update(fm1BW.lme, correlation = corCompSymm(form = ~ Time|SUBJID))
               randomInter.pval = randomInterModel[(ncovars+2):(ncovars+4),4]
               names(randomInter.pval) = rownames(randomInterModel)[(ncovars+2):(ncovars+4)]
               return(randomInter.pval)
             }, mc.cores = 4 )
  }else{
    uniResults <- 
      parallel::mclapply(targets,
             function(target) {
               if(is.null(covariates)){
                 formula_fix = as.formula(paste0(response, "~",
                                                 target,"+", Time,"*",Treatment))
               }else{
                 formula_fix = as.formula(paste0(response, "~",
                                             paste0(covariates,collapse = "+"), "+",
                                             target,"+", Time,"*",Treatment))
               }
             formula_random = as.formula(paste0("random= ~1|",Subject))
             randomInterModel <- nlme::lme(formula_fix, random=formula_random, 
                                           data=mydata, na.action=na.omit) %>% anova()
             # fm1BW.lmeSphere <- update(fm1BW.lme, correlation = corCompSymm(form = ~ Time|SUBJID))
             randomInter.pval = randomInterModel[(ncovars+2):(ncovars+5),4]
             names(randomInter.pval) = rownames(randomInterModel)[(ncovars+2):(ncovars+5)]
             return(randomInter.pval)
           }, mc.cores=4 )
  }

  uniResultsDF <- do.call(rbind, uniResults) %>% data.frame()
  rownames(uniResultsDF) = targets
  return(uniResultsDF)
} 
            
    
# 2.a extract concordance index from ipredict model (will be deprecated)
cindexExtract <- function(ens, df, target, target_cens, nSamples=10, seed=89){
  # Cindex -- return c-index or predicted values
  
  # simulation
  sim_tt = fsSimulateOverwrite(ens, outputs = target,
                               baselineData=df[, fsFixedVars(ens)],
                               sampleSize = nSamples, reportMeans = F,
                               seed=seed, submitToQueueWhenAvailable = F
                               )
  
  sim_results_temp = data.frame(sim_tt$result)
  sim_results_temp$flag = rep(1:(128*nSamples)) # per sample per model
  N=max(sim_results_temp$fixedDataRow) # number of subjects
  
  # output objects
  Cindex_list= c()
  ignore_count = 0
  
  # per sample per model auc for all subjects
  for(j in 1:(128*nSamples)){
    
    idx <- which(sim_results_temp$flag == j)
    
    if( sum(is.infinite(sim_results_temp[idx,"output"])) > 0 ){
        ignore_count = ignore_count + 1
        next
      }
    # calculate the C-index
    CIndex <- survcomp::concordance.index(x = -sim_results_temp[idx,"output"],
                                          surv.time = df[[target]],
                                          surv.event = df[[target_cens]],
                                          alpha = 0.05,
                                          alternative = "greater")
    Cindex_list = c(Cindex_list, CIndex$c.index)
  }
  
  print(paste(ignore_count, "samples contain infinite values and were thus ignored"))
  
  # 95% credible interval of prauc
  Cindex_pct_25 <- quantile(Cindex_list, 0.025, na.rm = T)
  Cindex_pct_975 <- quantile(Cindex_list, 0.975, na.rm = T)
  
  Cindex_stat <- c(signif(mean(Cindex_list), 3),
                   signif(Cindex_pct_25, 3),
                   signif(Cindex_pct_975, 3))
  
  names(Cindex_stat) <- c("Mean", "Lower95", "Upper95")
  
  return(Cindex_stat)
}

cindexExtractInd <- function(ens, df, targets, targets_cens, nSamples=10, seed=89){
  # Cindex -- return c-index or predicted values
  
  # simulation
  sim_tt = fsSimulateOverwrite(ens, outputs = targets,
                               baselineData=df,
                               sampleSize = nSamples, reportMeans = F,
                               seed=seed, submitToQueueWhenAvailable = F
  )
  
  Cindex_list <- c()
  for( i in 1:length(targets)){
    sim_results_temp = data.frame(sim_tt$result[[i]]) %>%
      mutate(tMean=gamma(1/alpha)/(alpha*beta^(1/alpha))) %>% 
      select(fixedDataRow, network, tMean) %>% 
      group_by(fixedDataRow, network) %>% 
      summarize_all(median) %>% select(-network) %>% 
      summarize_all(median)
    
    # calculate the C-index
    library(survcomp,lib="/home/qdu/R/x86_64-pc-linux-gnu-library/3.6")
    CIndex <- survcomp::concordance.index(x = -sim_results_temp$tMean,
                                          surv.time = df[,targets[i]],
                                          surv.event = df[,targets_cens[i]],
                                          alpha = 0.05,
                                          alternative = "greater")
      
    # store the result for each target
    Cindex_list = c(Cindex_list, signif(CIndex$c.index,3))
  }
  names(Cindex_list)=paste0(targets,"_",c("Cindex"))
  return(Cindex_list)
}

cindexExtractIndN <- function(ens, df, targets, targets_cens, nSamples=10, seed=89){
  # Cindex -- return c-index or predicted values
  
  # simulation
  sim_tt = fsSimulateOverwrite(ens, outputs = targets,
                               baselineData=df,
                               sampleSize = nSamples, reportMeans = F,
                               seed=seed, submitToQueueWhenAvailable = F
  )
  
  Cindex_targets <- c()
  for( i in 1:length(targets)){
    sim_results_temp = data.frame(sim_tt$result[[i]], nSample=c(1:nSamples)) %>%
      mutate(tMean=gamma(1/alpha)/(alpha*beta^(1/alpha))) %>% 
      select(nSample, fixedDataRow, tMean) %>% 
      group_by(nSample, fixedDataRow) %>% 
      summarize_all(median) 
    
    ignore_count=0
    Cindex_list <- c()
    for(j in 1:nSamples){
      idx <- which(sim_results_temp$nSample == j)
      pred_j <- sim_results_temp$tMean[idx]
      if( sum(is.infinite(pred_j)) > 0 ){
       ignore_count = ignore_count + 1
       next
      }
      
      # calculate the C-index
      library(survcomp,lib="/home/qdu/R/x86_64-pc-linux-gnu-library/3.6")
      CIndex <- survcomp::concordance.index(x = -pred_j,
                                           surv.time = df[,targets[i]],
                                           surv.event = df[,targets_cens[i]],
                                           alpha = 0.05,
                                           alternative = "greater")
    
      # store the result for each target
      Cindex_list = c(Cindex_list, signif(CIndex$c.index,3))
    }
    print(paste("For target",targets[i],ignore_count, 
                "nSample contain infinite values and were thus ignored"))
    
    # 95% credible interval
    Cindex_pct_25 <- quantile(Cindex_list, 0.025)
    Cindex_pct_975 <- quantile(Cindex_list, 0.975)
    
    Cindex_stat <- c(signif(mean(Cindex_list), 3),
                     signif(Cindex_pct_25, 3),
                     signif(Cindex_pct_975, 3))
    
    Cindex_targets <- c(Cindex_targets, Cindex_stat)
  }
  names(Cindex_targets)=paste0(targets,"_",c("Cindex"), c("Mean", "Lower95", "Upper95"))
  return(Cindex_targets)
}

# 2.c extract concordance index by concatenating the predictions from all folds (will be deprecated)
cvConExtract <- function(cvDir, nfold, target){
  
  cvSummary <- list()
  
  for(foldID in 1:nfold){
    # load the model and data in the same fold folder
    ens <- fsReadModel(paste0(cvDir,'/cv_fold_', foldID, '/run1/ensemble.txt.proto'))
    
    # train and test dataset
    load(paste0(cvDir,'/cv_fold_', foldID, '/testing.RData'))
    load(paste0(cvDir,'/cv_fold_', foldID, '/training.RData'))
    
    cvSummary$train[[foldID]] <- cindexExtract(ens, train, target, Cindex=F)
    cvSummary$test[[foldID]] <- cindexExtract(ens, test, target, Cindex=F)
  }
  
  concatTrain <- do.call(rbind, cvSummary$train)
  concatTest <- do.call(rbind, cvSummary$test)
  
  # generate C-index
  ## train
  CIndexTrain <- concordance.index(x = concatTrain$negPredT,
                                   surv.time = concatTrain$realT,
                                   surv.event = concatTrain$censor,
                                   alpha = 0.05,
                                   alternative = "greater")
  ## test
  CIndexTest <- concordance.index(x = concatTest$negPredT,
                                  surv.time = concatTest$realT,
                                  surv.event = concatTest$censor,
                                  alpha = 0.05,
                                  alternative = "greater")
  
  # C-index estimate with 95% CI
  cindexDF <- data.frame(ConcatTrain = 
                           c(round(CIndexTrain$c.index, 3),
                             round(CIndexTrain$lower, 3),
                             round(CIndexTrain$upper, 3)),
                         ConcatTest = 
                           c(round(CIndexTest$c.index, 3),
                             round(CIndexTest$lower, 3),
                             round(CIndexTest$upper, 3)))
  
  rownames(cindexDF) <- c("C-Index", "Lower_95%_CI", "Upper_95%_CI")
  
  
  return(cindexDF)
  
}

# 3.a extract auc and prauc from ipredict model
## per sample per model (will be deprecated)
aucExtract <- function(ens, df, target, nSamples=10, seed=89){
  
  # Binary target
  observ.values <- df[,target]

  if( !identical( names(table(observ.values)), c("0","1") ) ){
    stop("It was required to use 0 as control and 1 as positive")
  }
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens,outputs = target,
                               baselineData=df[, fsFixedVars(ens), drop=F],
                               sampleSize = nSamples, reportMeans = F,
                               seed=seed,submitToQueueWhenAvailable = F
  )

  sim_results_temp = data.frame(sim_tt$result)
  sim_results_temp$flag = rep(1:(128*nSamples)) # per sample per model
  N=max(sim_results_temp$fixedDataRow) # number of subjects

  # output objects
  pmf_auc = c()
  pmf_pr = c()
  auc_list= c()
  
  # per sample per model auc for all subjects
  for(j in 1:(128*nSamples)){
    
    idx <- which(sim_results_temp$flag == j)
  
    # calculate the fpauc
    ROC <- PRROC::roc.curve(scores.class0 = sim_results_temp$pmf[idx,2], 
                     weights.class0 = observ.values)
    pmf_auc = c(pmf_auc, ROC$auc)
    
    # calculate the prauc
    PR <- PRROC::pr.curve(scores.class0 = sim_results_temp$pmf[idx,2], 
                            weights.class0 = observ.values)
    pmf_pr = c(pmf_pr, PR$auc.integral)
    
  }
  
  # 95% credible interval of fpauc
  pmf_auc_pct_5 <- quantile(pmf_auc, 0.025, na.rm = T)
  pmf_auc_pct_95 <- quantile(pmf_auc, 0.975, na.rm = T)
  
  auc_stat <- c(signif(median(pmf_auc), 3),
                signif(pmf_auc_pct_5, 3),
                signif(pmf_auc_pct_95, 3))
  
  names(auc_stat) <- c("fpauc", "Lower95", "Upper95")
  
  # 95% credible interval of prauc
  pmf_pr_pct_5 <- quantile(pmf_pr, 0.025, na.rm = T)
  pmf_pr_pct_95 <- quantile(pmf_pr, 0.975, na.rm = T)
  
  pr_stat <- c(signif(median(pmf_pr), 3),
               signif(pmf_pr_pct_5, 3),
               signif(pmf_pr_pct_95, 3))
  
  names(pr_stat) <- c("prauc", "Lower95", "Upper95")

  # return
  auc_list = c( auc_stat,pr_stat )
  
  return(auc_list)
}
## per individual
aucExtractInd <- function(ens, df, targets, nSamples=10, seed=89){
  
  # Binary targets
  observ.df <- df[, targets, drop=F] %>% 
    mutate_all(as.factor) %>% 
    data.matrix() %>% data.frame() 
  observ.df <- observ.df - 1 
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens, outputs = targets,
                                baselineData= df,
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed, submitToQueueWhenAvailable = F
  )
  
  auc_list <- c()
  
  for( i in 1:length(targets)){
    sim_results_temp = sim_tt$result[[i]]
    sim_results_form = data.frame(fixedDataRow=sim_results_temp$fixedDataRow,
                                  network=sim_results_temp$network,
                                  output=as.numeric(sim_results_temp$output),
                                  pmf.0=sim_results_temp$pmf$`0`,
                                  pmf.1=sim_results_temp$pmf$`1`)
    sim_median_mean=sim_results_form %>% group_by(fixedDataRow, network) %>% 
      summarize_all(median) %>% select(-network,-output) %>% 
      summarize_all(mean)
  
    # calculate the fpauc
    ROC <- PRROC::roc.curve(scores.class0 = sim_median_mean$pmf.1, 
                           weights.class0 = observ.df[,i])$auc
  
    # calculate the prauc
    PR <- PRROC::pr.curve(scores.class0 = sim_median_mean$pmf.1, 
                         weights.class0 = observ.df[,i])$auc.integral

    auc_list =c( auc_list, signif(ROC,3), signif(PR,3) )
  }
  
  names(auc_list)=paste0(rep(targets,each=2),"_",c("fpAUC","prAUC"))
  return(auc_list)
}

aucExtractIndN <- function(ens, df, targets, nSamples=10, seed=89){
  
  # Binary targets
  observ.df <- df[, targets, drop=F] %>% 
    mutate_all(as.factor) %>% 
    data.matrix() %>% data.frame() 
  observ.df <- observ.df - 1 
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens, outputs = targets,
                                baselineData= df,
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed, submitToQueueWhenAvailable = F
  )
  
  AUC_targets = c()
  
  for( i in 1:length(targets)){
    sim_results_temp = sim_tt$result[[i]]
    sim_results_form = data.frame(fixedDataRow=sim_results_temp$fixedDataRow,
                                  nSample=c(1:nSamples),
                                  pmf.0=sim_results_temp$pmf$`0`,
                                  pmf.1=sim_results_temp$pmf$`1`) %>% 
      group_by(nSample, fixedDataRow) %>% 
      summarize_all(mean)
    
    ROC_list = c()
    PR_list = c()
    for(j in 1:nSamples){
      idx <- which(sim_results_form$nSample == j)
      pred_j <- sim_results_form$pmf.1[idx]
      
      # calculate the fpauc
      ROC <- PRROC::roc.curve(scores.class0 = pred_j, 
                              weights.class0 = observ.df[,i])$auc
    
      # calculate the prauc
      PR <- PRROC::pr.curve(scores.class0 = pred_j, 
                           weights.class0 = observ.df[,i])$auc.integral
      
      # store the result for each target
      ROC_list = c(ROC_list, signif(ROC,3))
      PR_list = c(PR_list,signif(PR,3))
    }
    # 95% credible interval
    ## ROC
    ROC_pct_25 <- quantile(ROC_list, 0.025)
    ROC_pct_975 <- quantile(ROC_list, 0.975)
    ROC_stat <- c(signif(mean(ROC_list), 3),
                  signif(ROC_pct_25, 3),
                  signif(ROC_pct_975, 3))
    ## PR
    PR_pct_25 <- quantile(PR_list, 0.025)
    PR_pct_975 <- quantile(PR_list, 0.975)
    PR_stat <- c(signif(mean(PR_list), 3),
                  signif(PR_pct_25, 3),
                  signif(PR_pct_975, 3))
    
    AUC_targets <- c(AUC_targets, ROC_stat, PR_stat)
  }
  names(AUC_targets)=paste0(rep(targets,each=6),"_",
                            rep(c("fpAUC","prAUC"),each=3),"_",
                            c("Mean", "Lower95", "Upper95"))
  return(AUC_targets)
}

# 3.b extract the R2 (will be deprecated)
r2Extract <- function(ens, df, target, nSamples, method="mss", seed=89){
  sim_perf <- fsPerformance(df, ens,
                            outcome = target,
                            nSamples=nSamples,
                            seed=seed)

  r2Vec <- sim_perf$getR2(method=method)[[target]]$R2_netSample %>% 
    quantile(c(0.5,0.025,0.975), na.rm=T) %>% signif(3)
  names(r2Vec) = c("R2","Lower95","Upper95") 
  
  return(r2Vec)
}

# 3.c extract the adjusted R2 (will be deprecated)
adjr2Extract <- function(ens, df, target, nSamples=10, seed=89){
  
  # continuous target
  observ.values <- df[,target]
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens,outputs = target,
                                baselineData=df[, fsFixedVars(ens), drop=F],
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed,submitToQueueWhenAvailable = F
  )
  
  sim_results_temp = data.frame(sim_tt$result)
  sim_results_temp$flag = rep(1:(128*nSamples)) # per sample per model
  N=max(sim_results_temp$fixedDataRow) # number of subjects
  
  # output objects
  adjr2_list= c()
  
  # per sample per model auc for all subjects
  for(j in 1:(128*nSamples)){
    
    idx <- which(sim_results_temp$flag == j)
    
    # calculate the adjusted R2
    adjr2 <- summary(lm(observ.values~sim_results_temp$mu[idx]))$adj.r.squared 
    adjr2_list = c(adjr2_list, adjr2)
  }
  
  # 95% credible interval of adjusted R squared
  adjr2_pct_25 <- quantile(adjr2_list, 0.025, na.rm = T)
  adjr2_pct_975 <- quantile(adjr2_list, 0.975, na.rm = T)
  
  adjr2_stat <- c(signif(median(adjr2_list), 3),
                   signif(adjr2_pct_25, 3),
                   signif(adjr2_pct_975, 3))
  
  names(adjr2_stat) <- c("Median", "Lower95", "Upper95")
  
  return(adjr2_stat)
}

adjr2ExtractInd <- function(ens, df, targets, nSamples=10, seed=89){
  
  # continuous target
  observ.df <- df[,targets,drop=F]
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens,outputs = targets,
                                baselineData=df,
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed, submitToQueueWhenAvailable = F
  )
  
  adjr2_list <- c()
  for( i in 1:length(targets)){
    sim_results_temp = data.frame(sim_tt$result[[i]]) %>% 
      select(fixedDataRow, network, mu) %>% 
      group_by(fixedDataRow, network) %>% 
      summarize_all(median) %>% select(-network) %>% 
      summarize_all(mean)
    # calculate the adjusted R2
    adjr2 <- summary(lm(observ.df[,i]~sim_results_temp$mu))$adj.r.squared %>% 
      signif(.,3)
    # store the result for each target
    adjr2_list=c(adjr2_list, adjr2)
  }
  names(adjr2_list)=targets
  
  return(adjr2_list)
}

adjr2ExtractInd1 <- function(ens, df, targets, nSamples=10, seed=89){
  
  # continuous target
  observ.df <- df[,targets,drop=F]
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens,outputs = targets,
                                baselineData=df,
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed, submitToQueueWhenAvailable = F
                                )
  
  adjr2_list <- c()
  for( i in 1:length(targets)){
    sim_results_temp = data.frame(sim_tt$result[[i]]) %>% 
      select(fixedDataRow, network, mu) %>% 
      group_by(fixedDataRow, network) %>% 
      summarize_all(median) %>% select(-network) %>% 
      summarize_all(mean)
    # calculate the adjusted R2
    rr = sum( (observ.df[,i]-sim_results_temp$mu)^2 )
    tt = sum( (observ.df[,i]-mean(observ.df[,i]))^2 )
    adjr2 <-  signif((1-rr/tt),3)
    # store the result for each target
    adjr2_list=c(adjr2_list, adjr2)
  }
  names(adjr2_list)=targets
  
  return(adjr2_list)
}

adjr2ExtractIndMedian <- function(ens, df, targets, nSamples=10, seed=89){
  
  # continuous target
  observ.df <- df[,targets,drop=F]
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens,outputs = targets,
                                baselineData=df,
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed, submitToQueueWhenAvailable = F
  )
  
  adjr2_list <- c()
  for( i in 1:length(targets)){
    sim_results_temp = data.frame(sim_tt$result[[i]]) %>% 
      select(fixedDataRow, network, mu) %>% 
      group_by(fixedDataRow, network) %>% 
      summarize_all(median) %>% select(-network) %>% 
      summarize_all(median)
    # calculate the adjusted R2
    adjr2 <- summary(lm(observ.df[,i]~sim_results_temp$mu))$adj.r.squared %>% 
      signif(.,3)
    # store the result for each target
    adjr2_list=c(adjr2_list, adjr2)
  }
  names(adjr2_list)=targets
  
  return(adjr2_list)
}

adjr2ExtractInd1Median <- function(ens, df, targets, nSamples=10, seed=89){
  
  # continuous target
  observ.df <- df[,targets,drop=F]
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens,outputs = targets,
                                baselineData=df,
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed, submitToQueueWhenAvailable = F
  )
  
  adjr2_list <- c()
  for( i in 1:length(targets)){
    sim_results_temp = data.frame(sim_tt$result[[i]]) %>% 
      select(fixedDataRow, network, mu) %>% 
      group_by(fixedDataRow, network) %>% 
      summarize_all(median) %>% select(-network) %>% 
      summarize_all(median)
    # calculate the adjusted R2
    rr = sum( (observ.df[,i]-sim_results_temp$mu)^2 )
    tt = sum( (observ.df[,i]-mean(observ.df[,i]))^2 )
    adjr2 <-  signif((1-rr/tt),3)
    # store the result for each target
    adjr2_list=c(adjr2_list, adjr2)
  }
  names(adjr2_list)=targets
  
  return(adjr2_list)
}


adjr2ExtractIndN <- function(ens, df, targets, nSamples=10, seed=89){
  
  # continuous target
  observ.df <- df[,targets,drop=F]
  
  # simulation
  sim_tt <- fsSimulateOverwrite(ens,outputs = targets,
                                baselineData=df,
                                sampleSize = nSamples, reportMeans = F,
                                seed=seed, submitToQueueWhenAvailable = F
  )
  
  adjr2_targets <- c()
  for( i in 1:length(targets)){
    sim_results_temp = data.frame(sim_tt$result[[i]], nSample=c(1:nSamples)) %>% 
      select(nSample, fixedDataRow, mu) %>% 
      group_by(nSample, fixedDataRow) %>% 
      summarize_all(mean)
    
    adjr2_list <- c()
    for(j in 1:nSamples){
      idx <- which(sim_results_temp$nSample == j)
      pred_j <- sim_results_temp$mu[idx]
      
      # calculate the adjusted R2
      adjr2 <- summary(lm(observ.df[,i]~pred_j))$adj.r.squared %>% 
      signif(.,3)
      # store the result for each target
      adjr2_list=c(adjr2_list, adjr2)
    }
    # 95% credible interval
    adjr2_pct_25 <- quantile(adjr2_list, 0.025)
    adjr2_pct_975 <- quantile(adjr2_list, 0.975)
    
    adjr2_stat <- c(signif(mean(adjr2_list), 3),
                     signif(adjr2_pct_25, 3),
                     signif(adjr2_pct_975, 3))
    
    adjr2_targets <- c(adjr2_targets, adjr2_stat)
  }
  names(adjr2_targets)=paste0(rep(targets,each=3),"_adjr2_",
                              c("Mean", "Lower95", "Upper95"))
  return(adjr2_targets)
}

# 4. extract the performances from the ipredict model(cross-validation) (will be deprecated)
cvIpredictExtract <- function(cvDir, target, nSamples, targetype="normalMSS", target_cens=NULL){
  ## reading in crossvalidaiton file
  dirfold <- list.files(cvDir, pattern = "fold", full.names = TRUE)
  pb <- progress_estimated(length(dirfold))
  
  allres <- purrr::map_df( dirfold,
                           function(dirf) {
                             if(grepl("cv_Repeat", dirf)){
                               repeatn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\1", dirf)
                               foldn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\2", dirf)
                             }else{
                               repeatn = 1
                               foldn = sub(".*cv_fold_(.*)", "\\1", dirf)
                             }
                             ensloc <- file.path(dirf, "ensemble.txt.proto")
                             if (file.exists(ensloc)) {
                               # load in the model
                               ens <- fsReadModel(ensloc)
                               # performance on training folds
                               traindf <- fread(file.path(dirf, "training.csv"), data.table = F)
                               if(targetype=="normalMSS"){
                                 trainrs <- r2Extract(ens, df=traindf, target, nSamples, seed=89)
                               }else if(targetype=="normalRSS"){
                                 trainrs <- r2Extract(ens, df=traindf, target, nSamples, method="rss",seed=89)
                               }else if(targetype=="adjustR2"){
                                 trainrs <- adjr2Extract(ens, df=traindf, target, nSamples)
                               }else if(targetype=="binary"){
                                 trainrs <- aucExtract(ens, df=traindf, target, nSamples)
                               }else if(targetype=="time"){
                                 trainrs <- cindexExtract(ens, df=traindf, target, target_cens)
                               }
                               # performance on testing fold
                               testdf <- fread(file.path(dirf, "testing.csv"), data.table = F)
                               if(targetype=="normalMSS"){
                                 testrs <- r2Extract(ens, df=testdf, target, nSamples, seed=89)
                               }else if(targetype=="normalRSS"){
                                 testrs <- r2Extract(ens, df=testdf, target, nSamples, method="rss",seed=89)
                               }else if(targetype=="adjustR2"){
                                 testrs <- adjr2Extract(ens, df=testdf, target, nSamples)
                               }else if(targetype=="binary"){
                                 testrs <- aucExtract(ens, df=testdf, target, nSamples)
                               }else if(targetype=="time"){
                                 testrs <- cindexExtract(ens, df=testdf, target, target_cens)
                               }
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               target, repeatn, foldn, trainrs, testrs))
                             }else{
                               message("ensemble.txt.proto missing in ", dirf)
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               target, repeatn, foldn, list(), list()))
                             }
                           }
  )
  
  cvSummary = list()
  # convert into dataframe
  cvSummary$train <- do.call(rbind, allres$TrainPerf) 
  storage.mode(cvSummary$train) = "numeric"
  cvSummary$train <- cvSummary$train %>% rbind(.,colMeans(.))
  
  cvSummary$test <- do.call(rbind, allres$TestPerf) 
  storage.mode(cvSummary$test) = "numeric"
  cvSummary$test <- cvSummary$test %>% rbind(.,colMeans(.))

  # rownames
  #rownames(cvSummary$train) <- 
   # c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "trainMean")
  #rownames(cvSummary$test) <- 
   # c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "testMean")
  
  return(cvSummary)
}

cvIpredictExtractInd <- function(cvDir, targets, nSamples, 
                                 targetype="adjustR2", targets_cens=NULL){
  ## reading in crossvalidaiton file
  dirfold <- list.files(cvDir, pattern = "fold", full.names = TRUE)
  pb <- progress_estimated(length(dirfold))
  
  allres <- purrr::map_df( dirfold,
                           function(dirf) {
                             if(grepl("cv_Repeat", dirf)){
                               repeatn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\1", dirf)
                               foldn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\2", dirf)
                             }else{
                               repeatn = 1
                               foldn = sub(".*cv_fold_(.*)", "\\1", dirf)
                             }
                             ensloc <- file.path(dirf, "ensemble.txt.proto")
                             if (file.exists(ensloc)) {
                               # load in the model
                               ens <- fsReadModel(ensloc)
                               # performance on training folds
                               traindf <- fread(file.path(dirf, "training.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 trainrs <- adjr2ExtractInd(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 trainrs <- aucExtractInd(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="time"){
                                 trainrs <- cindexExtractInd(ens, df=traindf, targets, targets_cens, nSamples)
                               }
                               # performance on testing fold
                               testdf <- fread(file.path(dirf, "testing.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 testrs <- adjr2ExtractInd(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 testrs <- aucExtractInd(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="time"){
                                 testrs <- cindexExtractInd(ens, df=testdf, targets, targets_cens, nSamples)
                               }
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, trainrs, testrs))
                             }else{
                               message("ensemble.txt.proto missing in ", dirf)
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, list(), list()))
                             }
                           }
  )
  
  cvSummary = list()
  # convert into dataframe
  if(is.list(allres$TrainPerf)){
    cvSummary$train <- do.call(rbind, allres$TrainPerf)
    cvSummary$test <- do.call(rbind, allres$TestPerf) 
  }else{
    cvSummary$train <- data.frame(Train=allres$TrainPerf)
    cvSummary$test <- data.frame(Test=allres$TestPerf)
  }
  #storage.mode(cvSummary$train) = "numeric"
  cvSummary$train <- cvSummary$train %>% rbind(.,colMeans(.))
  #storage.mode(cvSummary$test) = "numeric"
  cvSummary$test <- cvSummary$test %>% rbind(.,colMeans(.))
  
  # rownames
  rownames(cvSummary$train) <- 
   c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "trainMean")
  rownames(cvSummary$test) <- 
   c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "testMean")
  
  return(cvSummary)
}

cvIpredictExtractInd1 <- function(cvDir, targets, nSamples, 
                                 targetype="adjustR2", targets_cens=NULL){
  ## reading in crossvalidaiton file
  dirfold <- list.files(cvDir, pattern = "fold", full.names = TRUE)
  pb <- progress_estimated(length(dirfold))
  
  allres <- purrr::map_df( dirfold,
                           function(dirf) {
                             if(grepl("cv_Repeat", dirf)){
                               repeatn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\1", dirf)
                               foldn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\2", dirf)
                             }else{
                               repeatn = 1
                               foldn = sub(".*cv_fold_(.*)", "\\1", dirf)
                             }
                             ensloc <- file.path(dirf, "ensemble.txt.proto")
                             if (file.exists(ensloc)) {
                               # load in the model
                               ens <- fsReadModel(ensloc)
                               # performance on training folds
                               traindf <- fread(file.path(dirf, "training.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 trainrs <- adjr2ExtractInd1(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 trainrs <- aucExtractInd(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="time"){
                                 trainrs <- cindexExtractInd(ens, df=traindf, targets, targets_cens, nSamples)
                               }
                               # performance on testing fold
                               testdf <- fread(file.path(dirf, "testing.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 testrs <- adjr2ExtractInd1(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 testrs <- aucExtractInd(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="time"){
                                 testrs <- cindexExtractInd(ens, df=testdf, targets, target_cens, nSamples)
                               }
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, trainrs, testrs))
                             }else{
                               message("ensemble.txt.proto missing in ", dirf)
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, list(), list()))
                             }
                           }
  )
  
  cvSummary = list()
  # convert into dataframe
  if(is.list(allres$TrainPerf)){
    cvSummary$train <- do.call(rbind, allres$TrainPerf)
    cvSummary$test <- do.call(rbind, allres$TestPerf) 
  }else{
    cvSummary$train <- data.frame(Train=allres$TrainPerf)
    cvSummary$test <- data.frame(Test=allres$TestPerf)
  }
  #storage.mode(cvSummary$train) = "numeric"
  cvSummary$train <- cvSummary$train %>% rbind(.,colMeans(.))
  #storage.mode(cvSummary$test) = "numeric"
  cvSummary$test <- cvSummary$test %>% rbind(.,colMeans(.))
  
  # rownames
  rownames(cvSummary$train) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "trainMean")
  rownames(cvSummary$test) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "testMean")
  
  return(cvSummary)
}

cvIpredictExtractIndMedian <- function(cvDir, targets, nSamples, 
                                 targetype="adjustR2", targets_cens=NULL){
  ## reading in crossvalidaiton file
  dirfold <- list.files(cvDir, pattern = "fold", full.names = TRUE)
  pb <- progress_estimated(length(dirfold))
  
  allres <- purrr::map_df( dirfold,
                           function(dirf) {
                             if(grepl("cv_Repeat", dirf)){
                               repeatn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\1", dirf)
                               foldn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\2", dirf)
                             }else{
                               repeatn = 1
                               foldn = sub(".*cv_fold_(.*)", "\\1", dirf)
                             }
                             ensloc <- file.path(dirf, "ensemble.txt.proto")
                             if (file.exists(ensloc)) {
                               # load in the model
                               ens <- fsReadModel(ensloc)
                               # performance on training folds
                               traindf <- fread(file.path(dirf, "training.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 trainrs <- adjr2ExtractIndMedian(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 trainrs <- aucExtractInd(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="time"){
                                 trainrs <- cindexExtractInd(ens, df=traindf, targets, targets_cens, nSamples)
                               }
                               # performance on testing fold
                               testdf <- fread(file.path(dirf, "testing.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 testrs <- adjr2ExtractIndMedian(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 testrs <- aucExtractInd(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="time"){
                                 testrs <- cindexExtractInd(ens, df=testdf, targets, target_cens, nSamples)
                               }
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, trainrs, testrs))
                             }else{
                               message("ensemble.txt.proto missing in ", dirf)
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, list(), list()))
                             }
                           }
  )
  
  cvSummary = list()
  # convert into dataframe
  if(is.list(allres$TrainPerf)){
    cvSummary$train <- do.call(rbind, allres$TrainPerf)
    cvSummary$test <- do.call(rbind, allres$TestPerf) 
  }else{
    cvSummary$train <- data.frame(Train=allres$TrainPerf)
    cvSummary$test <- data.frame(Test=allres$TestPerf)
  }
  #storage.mode(cvSummary$train) = "numeric"
  cvSummary$train <- cvSummary$train %>% rbind(.,colMeans(.))
  #storage.mode(cvSummary$test) = "numeric"
  cvSummary$test <- cvSummary$test %>% rbind(.,colMeans(.))
  
  # rownames
  rownames(cvSummary$train) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "trainMean")
  rownames(cvSummary$test) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "testMean")
  
  return(cvSummary)
}

cvIpredictExtractInd1Median <- function(cvDir, targets, nSamples, 
                                  targetype="adjustR2", targets_cens=NULL){
  ## reading in crossvalidaiton file
  dirfold <- list.files(cvDir, pattern = "fold", full.names = TRUE)
  pb <- progress_estimated(length(dirfold))
  
  allres <- purrr::map_df( dirfold,
                           function(dirf) {
                             if(grepl("cv_Repeat", dirf)){
                               repeatn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\1", dirf)
                               foldn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\2", dirf)
                             }else{
                               repeatn = 1
                               foldn = sub(".*cv_fold_(.*)", "\\1", dirf)
                             }
                             ensloc <- file.path(dirf, "ensemble.txt.proto")
                             if (file.exists(ensloc)) {
                               # load in the model
                               ens <- fsReadModel(ensloc)
                               # performance on training folds
                               traindf <- fread(file.path(dirf, "training.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 trainrs <- adjr2ExtractInd1Median(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 trainrs <- aucExtractInd(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="time"){
                                 trainrs <- cindexExtractInd(ens, df=traindf, targets, targets_cens, nSamples)
                               }
                               # performance on testing fold
                               testdf <- fread(file.path(dirf, "testing.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 testrs <- adjr2ExtractInd1Median(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 testrs <- aucExtractInd(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="time"){
                                 testrs <- cindexExtractInd(ens, df=testdf, targets, target_cens, nSamples)
                               }
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, trainrs, testrs))
                             }else{
                               message("ensemble.txt.proto missing in ", dirf)
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, list(), list()))
                             }
                           }
  )
  
  cvSummary = list()
  # convert into dataframe
  if(is.list(allres$TrainPerf)){
    cvSummary$train <- do.call(rbind, allres$TrainPerf)
    cvSummary$test <- do.call(rbind, allres$TestPerf) 
  }else{
    cvSummary$train <- data.frame(Train=allres$TrainPerf)
    cvSummary$test <- data.frame(Test=allres$TestPerf)
  }
  #storage.mode(cvSummary$train) = "numeric"
  cvSummary$train <- cvSummary$train %>% rbind(.,colMeans(.))
  #storage.mode(cvSummary$test) = "numeric"
  cvSummary$test <- cvSummary$test %>% rbind(.,colMeans(.))
  
  # rownames
  rownames(cvSummary$train) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "trainMean")
  rownames(cvSummary$test) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "testMean")
  
  return(cvSummary)
}

cvIpredictExtractIndN <- function(cvDir, targets, nSamples, 
                                 targetype="adjustR2", targets_cens=NULL){
  ## reading in crossvalidaiton file
  dirfold <- list.files(cvDir, pattern = "fold", full.names = TRUE)
  pb <- progress_estimated(length(dirfold))
  
  allres <- purrr::map_df( dirfold,
                           function(dirf) {
                             if(grepl("cv_Repeat", dirf)){
                               repeatn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\1", dirf)
                               foldn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\2", dirf)
                             }else{
                               repeatn = 1
                               foldn = sub(".*cv_fold_(.*)", "\\1", dirf)
                             }
                             ensloc <- file.path(dirf, "ensemble.txt.proto")
                             if (file.exists(ensloc)) {
                               # load in the model
                               ens <- fsReadModel(ensloc)
                               # performance on training folds
                               traindf <- fread(file.path(dirf, "training.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 trainrs <- adjr2ExtractIndN(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 trainrs <- aucExtractIndN(ens, df=traindf, targets, nSamples)
                               }else if(targetype=="time"){
                                 trainrs <- cindexExtractIndN(ens, df=traindf, targets, targets_cens, nSamples)
                               }
                               # performance on testing fold
                               testdf <- fread(file.path(dirf, "testing.csv"), data.table = F)
                               if(targetype=="adjustR2"){
                                 testrs <- adjr2ExtractIndN(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="binary"){
                                 testrs <- aucExtractIndN(ens, df=testdf, targets, nSamples)
                               }else if(targetype=="time"){
                                 testrs <- cindexExtractIndN(ens, df=testdf, targets, target_cens, nSamples)
                               }
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, trainrs, testrs))
                             }else{
                               message("ensemble.txt.proto missing in ", dirf)
                               
                               pb$tick()$print()
                               
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               targets, repeatn, foldn, list(), list()))
                             }
                           }
  )
  
  cvSummary = list()
  # convert into dataframe
  if(is.list(allres$TrainPerf)){
    cvSummary$train <- do.call(rbind, allres$TrainPerf)
    cvSummary$test <- do.call(rbind, allres$TestPerf) 
  }else{
    cvSummary$train <- data.frame(Train=allres$TrainPerf)
    cvSummary$test <- data.frame(Test=allres$TestPerf)
  }
  #storage.mode(cvSummary$train) = "numeric"
  cvSummary$train <- cvSummary$train %>% rbind(.,colMeans(.))
  #storage.mode(cvSummary$test) = "numeric"
  cvSummary$test <- cvSummary$test %>% rbind(.,colMeans(.))
  
  # rownames
  rownames(cvSummary$train) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "trainMean")
  rownames(cvSummary$test) <- 
    c(paste0("Repeat",allres$Repeat, "Fold", allres$Fold), "testMean")
  
  return(cvSummary)
}


# Part IV: presentation using knitr
# tables
# 1.
# knitr::kable(osCindex$train, "html") %>% 
#  column_spec(1, bold=T) %>% 
#  row_spec(6, bold=T, color="#3333FF") %>% 
#  kable_styling(full_width=F, position="float_left",
#                fixed_thead = list(enabled=T, background="#99CCFF"),
#                bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# 3. extract distributions of HR for predictors with freq > cutoff

causalEffExtract <- function(modelPath, modelData, target, 
                             cutoff=0.05, vio.plot=F, keptID=1:2,
                             HR.plot=F, showpmin=0, showpmax=0.998){
  # 1. warning: for the moment, this function can't deal with interaction terms and n-th power terms
  # 2. for continuous variables, the experiment conditions were set up using 0.05 quantiles
  # and 0.95 quantiles of the tested variables
  # 3. since the distribution of HR often has long tail, we only (0-0.998) percent
  #    of the variable's distribution
  
  # trained model
  ens <- fsReadModel(paste0(modelPath,"run1/ensemble.txt.proto"))
  
  # Terms with frequencies largher than the cutoff
  # drop any interaction term or terms with power > 1
  terms <- fsTermFrequencies(ens, incParamStats = TRUE) %>% 
    subset(output == target & freq > cutoff & !grepl("\\^|\\:", input)) 
  
  varsHR <- lapply(terms$input %>% as.character, function(x){
    
    # experimental conditions
    preTerm <- gsub("\\(?(.*categorical|.*binary|.*continuous|.*Imputed)\\^?.*", "\\1", x)
    
    if(grepl("continuous$|Imputed", x)){ # continuous variable
      
      expCondition <- quantile(modelData[,preTerm],
                               probs=c(0.05,0.95)) %>% as.vector()
      
    }else if(grepl("continuous\\^", x)){
      # power of the continuous variable
      varPower <- gsub(".*\\^(.*)", "\\1", x) %>% as.numeric()
      
      expCondition <- quantile(modelData[,preTerm]^varPower,
                               probs=c(0.05,0.95)) %>% as.vector()
      
    }else if(grepl("categorical|binary", preTerm)){
      # reference level
      refLev <- names(table(modelData[, preTerm]))[1]
      # experimental level
      expLev <- gsub(".*= (.*)\\)", "\\1", x)
      
      expCondition <- c(refLev, expLev)
      
    }
    
    # causal effect
    causalEff <- fsCausalEffectInference(
      modelData, ens, treatment=preTerm,
      treatmentValues=expCondition,outcome=target)
    # hazard ratio
    causalEff.DF <- causalEff$getAvgHazardRatio() %>% unlist()
    
    return(causalEff.DF)
  })
  
  plotData <- data.frame(do.call(cbind, varsHR))
  colnames(plotData) <- gsub("\\(|\\)|Lab_|Muta_|Diag_|Vital_|Pts_","",terms$input)
  
  # violin plot of HRs
  if(vio.plot){
    # variables to be plotted and set the HR limit to be 3
    vioData = gather(plotData[,keptID], "variables", "values") %>% 
      mutate(values = ifelse(values > 3, 3, values),
             variables = factor(variables, levels=unique(variables)))
    
    # get means of hazard ratios
    # differe colors for variables with HR means > 1, = 1, and < 1
    vioData_summary = vioData %>% group_by(variables) %>% 
      mutate(mean = format(round(mean(values, na.rm=T),2),nsmall=2)) %>% 
      filter(!duplicated(variables)) %>% 
      mutate(Hgroup = ifelse(as.numeric(mean) > 1, "#D73027", 
                             ifelse(as.numeric(mean) == 1, "#999999", "#313695"))) %>% 
      as.data.frame 
    
    p <- ggplot(vioData, aes(x=variables, y = values, 
                             fill=variables, color=variables)) + 
      geom_violin(scale="width") +
      scale_fill_manual(values=vioData_summary$Hgroup) +
      scale_color_manual(values=vioData_summary$Hgroup) +
      geom_boxplot(outlier.shape=NA, coef=0, width=0.08, fill="white", color = "black") +
      geom_label(data=vioData_summary, aes(x=variables, y=-0.5, label=mean), 
                 label.padding=unit(0.2,"lines"), size=3.5, 
                 fill="white", color=vioData_summary$Hgroup) +
      ylim(-0.75,3) + ylab("posterior distribution of hazard ratio") +
      theme_classicM(legend.position = "none") +
      theme(axis.text.y = element_text(color=vioData_summary$Hgroup),
            axis.title.y = element_text(color=vioData_summary$Hgroup)) +
      coord_flip()
    print(p)
    
  }
  
  
  # Density plot of hazard ratios
  if(HR.plot){
    
    for(chunk in split(1:ncol(plotData),ceiling(seq_along(1:ncol(plotData))/6))){
      chunkData <- plotData[, chunk]
      
      my_plots <- lapply(names(chunkData), function(var){
        
        p <- ggplot(data.frame(HR=chunkData[, var]),aes(x=HR)) + theme_classic() +
          geom_density(alpha=0.2,fill="#FF6655") + 
          geom_vline(xintercept=1,colour="red",linetype="longdash",size=0.5) +
          labs(x = "Hazard Ratio (Average Causal Effect)", y="Density", 
               title = var) +
          xlim(quantile(chunkData[,var], 
                        probs=c(showpmin, showpmax), na.rm=T) %>% as.vector()) +
          theme(axis.text.x=element_text(face="bold",color="black",size=7,
                                         angle=45,hjust=1),axis.text.y= element_text(color="black",size=7),
                axis.title= element_text(face="bold",size=8),
                plot.title = element_text(face="bold", size=9))
      })
      print(plot_grid(plotlist = my_plots,nrow=3,ncol=2,labels="auto"))
    }
  }
  
  resultHR <- cbind(terms[,c("input", "freq")],  
                    lapply(varsHR, function(x){c(mean(x, na.rm=T),
                                                 quantile(x, probs=c(0.5, 0.05, 0.95), na.rm=T))}) %>% 
                      do.call(rbind, .)) %>% 
    mutate(CI_95perc = paste0("(",round(`5%`,2),", ",round(`95%`,2),")")) %>% 
    dplyr::select(-`5%`,-`95%`) %>% 
    mutate(input = gsub("\\(|\\)|Lab_|Muta_|Diag_|Vital_|Pts_","",input))
  
  colnames(resultHR) <- c("Vars", "Freq", "HR.mean", "HR.median", "HR.95%CI")
  return(resultHR)
}


library("visNetwork")
fsPlotNetwork_pretty <- function(ens, output, node_name_mapping, freq = 0.2, 
                                 cutoff = 0.2, maxpath = 2, fontsize = 25, 
                                 hierarchical = FALSE){
  
  #R.devices::nulldev()
  NELgraph = fsPlotNetwork(ens, output = output, freq = freq, cutoff = cutoff, 
                           maxpath = maxpath, fontsize=fontsize)
  #dev.off()
  
  visgraph = toVisNetworkData(igraph.from.graphNEL(NELgraph))
  #visgraph$nodes = node_level_mappings(visgraph$nodes)
  if(!is.null(node_name_mapping)) {
    visgraph$nodes$label = sapply(visgraph$nodes$label, function(x) 
      ifelse(x %in% rownames(node_name_mapping),as.character(node_name_mapping[x,])))
  }
  
  visgraph$edges$value = visgraph$edges$freq
  visgraph$edges$arrows = "to"
  visgraph$edges$title = visgraph$edges$freq
  viz = visNetwork(nodes = visgraph$nodes, edges = visgraph$edges,
                   main = paste("Consensus Network around ",output,sep="")) %>%  
    visPhysics(enabled = F) %>% 
    visOptions(highlightNearest = list(enabled = T, degree = 1, hover = TRUE, 
                                       algorithm = "hierarchical"), manipulation = FALSE) %>% 
    visEdges(smooth = F, scaling = list(min=1,max=8)) %>% 
    visNodes(shape = "box")  # %>% visHierarchicalLayout(levelSeparation = 50, nodeSpacing = 200)
  if(hierarchical){
    viz = viz %>% visHierarchicalLayout(levelSeparation = 30, nodeSpacing = 200)
  }
  return(viz)
}



