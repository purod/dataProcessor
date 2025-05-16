library(plyr)
library(tidyverse)
library(data.table)
library(readxl)
source("/home/qdu/git/dataProcessor/table2df.R")
source("/home/qdu/git/dataProcessor/split_replace_strings.R")

# batch effect
source("/home/qdu/git/dataProcessor/PCAbiplot.R")


source("/home/qdu/git/dataProcessor/graphs.R")
source("/home/qdu/git/dataProcessor/plotVenn.R")
source("/home/qdu/git/dataProcessor/pheatmap_anno.R")
source("/home/qdu/git/dataProcessor/plotTheme.R")
source("/home/qdu/git/dataProcessor/reportDTtable.R")
source("/home/qdu/git/dataProcessor/reportVarDistTable.R")
select=dplyr::select
