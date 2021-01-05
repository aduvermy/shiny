
is_same_list = function(listA, listB){
  if (( length(listA) == length(listB)) && (length(setdiff(listA, listB))== 0 )){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


check_upld_countFile_format <- function(countFile_path, gene){
  count_file <- countFile_path %>% 
              read_tsv(. ,col_names=c("gene","counts"))
  #print(head(gene))
  gene_from_CountFile <- count_file$gene
  gene_from_DB <- gene$id
  # if Run field in design file MATCH with your input counts files names
  print(is_same_list(gene_from_CountFile, gene_from_DB))
        
  if (is_same_list(gene_from_CountFile, gene_from_DB) == TRUE) {
    print("Format Count File: GOOD")
    return(countFile_path)
  }
  else{
    print("Format Count File: WRONG")
    return(NA)
  }
}





check_upld_designFile_format = function(design_upload, countFile_upload){
  design <- design_upload$datapath %>% 
                        read_tsv(. ,col_names  =TRUE)
  file_name_run <- countFile_upload$name %>% str_remove(paste(".",file_extension(.),sep=""))
  # if Run field in design file MATCH with your input counts files names
  if (is_same_list(design$Run, file_name_run) == TRUE) {
    print("Design file match with input counts files")
    return(design)
  }
  else{
    print("Design file doesn't match with input counts files")
    return(NULL)
  }
}