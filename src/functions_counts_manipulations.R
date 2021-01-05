### FUNCTION COUNTS FILES MANIPULATIONS
##load files from DB in 1 dtf
counts2dtf<-function(sampleInfo){
  dtf <-  sampleInfo$path %>%
    set_names(sampleInfo$Run) %>%
    map_dfr(~ read_tsv(. , col_names = c("gene","counts")), .id = "Run", path=sampleInfo$Run)
  return(dtf)
}

##convert dtf 2 matrix
dtf2matrix<- function(dtf){ ##convert dataframe 2 matrix
  matrice<-reshape2::dcast(dtf, gene ~ Run, value.var= "counts")
  rownames(matrice) <- matrice$gene
  matrice <- matrice[, -1] ##suppr colonne GeneID
  return(matrice)
}



matrix2deseq <- function(matrice, sampleInfo){
  DESeqDataSetFromMatrix (countData = matrice,
                          colData = sampleInfo ,
                          design = ~BioSample)
}