# Biocmanager to install required packages ---
if ("BiocManager" %in% rownames(installed.packages()) == FALSE) {
install.packages("BiocManager")
}


ProjectLibraries <- function(pkgs) {
  
  library(BiocManager)
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    BiocManager::install(pkgs_miss,update = TRUE, ask = FALSE)
  }
  
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    BiocManager::install(pkgs_miss,update = TRUE, ask = FALSE)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <-
    pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach))
      require(need_to_attach[i], character.only = TRUE)
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages are loaded!\n")
  }
}


# Packages used
pkgs <-
  c(
    "jsonlite",
    "reshape2",
    "ggplot2",
    "tidyverse",
    "shinydashboard",
    "shinyWidgets",
    "shiny",
    "DESeq2",
    "ggplot2",
    "RColorBrewer",
    "DT",
    "janitor",
    "shinyBS",
    "data.table",
    "pheatmap",
    "NMF",
    "plotly",
    "shinythemes",
    "sortable",
    "grid",
    "shinycssloaders",
    "ggdendro", 
    "heatmaply"
  )

# Install if needed
ProjectLibraries(pkgs)

# to Load packages
packlist <- sapply(pkgs, require, character.only = T)
names(packlist) <- pkgs
packlist
