## load libraries
rm(list = ls())


#path2library=.libPaths() ##default directory for library

source("load_project_packages.R")

## Data directory
directory="../data/"
#path2library


source("functions_normalization.R")
source('functions_counts_manipulations.R')
source('functions_plots_figures.R')
source('functions_check_uploadFile_format.R')

###GLOBAL VARIABLE LOAD
##load dataBase table
DataBase<-paste(directory,"htseq_database_design.tsv",sep = "") %>%
  read_tsv(. ,col_names  =TRUE) %>% 
  remove_empty(which=c("rows","cols"))%>%
  mutate(path= paste(directory,"htseq_database/",Run,".txt",sep=""))




##list all library available
liste_run<-DataBase$Run

##list gene 
liste_gene<- paste(directory,"taille_feature/gene_informations.tab",sep="")%>%
  read_tsv(.,col_names = T)



### Row from Databse with field dev_stage not empty
DB_for_times_series <-DataBase %>%
 filter(dev_stage != is.na(.))

### Run from Databse with field dev_stage not empty
run_with_tps_label_fromDB <- DB_for_times_series$Run
#run_with_tps_label_fromDB
## Dev stage label available
#devStage_label<- unique(data_for_times_series$dev_stage)


gene_go<-paste(directory,"annotation/gene_go.json",sep="") %>% 
                    read_json(., simplifyVector = TRUE)
gene_interpro<-paste(directory,"annotation/gene_interpro.json",sep="") %>% 
  read_json(., simplifyVector = TRUE)
