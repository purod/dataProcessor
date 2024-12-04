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
