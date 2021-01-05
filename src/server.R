############################################################################  
## Server side
############################################################################

source("global.R")

shinyServer(function(input, output,session) {

############################################################################
##OBSERVE USER CHOICE AND BUILD DATA 2 PLOT
############################################################################

  ## PLOT DB TABLE -> allows selection Table
  output$DB_table <- DT::renderDataTable(
          datatable( data =  DataBase %>% select(-path),
              selection = 'multiple')
    ) # End of renderDataTable
  
  ### MY SAMPLE INPUT uploaded
  getData <- reactive({
    print("getData")
    if (is.null(input$input_countFile)) return(NULL)
    else {
      # Read file design  
      inCountFiles<- input$input_countFile
      # Check if upload count file matches with design file ##returns NULL or design_uploaded
      test <- sapply(inCountFiles$datapath, function(file) check_upld_countFile_format(file, liste_gene))
      print(test)
      if(any(is.na(test))) return(NULL)  ## Format: wrong
      else return(inCountFiles) ## Format: ok
    }
  })
  
  # Check if file uploaded to modify ui with conditionnal panel
  ## BOOLEAN FILE UPLOADED -> ui print uploaded your design files 
  output$fileUploaded <- reactive({
    print('Count file Uploaded correctly')
    if (!is.null(input$input_countFile)) {
      if (is.null(getData())) return("countFile_unload")
      else return(TRUE)
    }
    return(FALSE)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  ### MY SAMPLE INPUT DESIGN uploaded
  getData_design <- reactive({
    print("getData_design")
    if (is.null(input$input_countFile_design)) return(NULL)
    else {
      ## Read file design
      design_uploaded <- input$input_countFile_design
      inCountFiles <- input$input_countFile
      # Check if upload count file match with design file ##return NULL or design_uploaded
      return(check_upld_designFile_format(design_uploaded, inCountFiles))
      } 
  })
  
  ##check if file design match with run uploaded to modify ui with conditionnal panel
  ##BOOLEAN DESIGN UPLOADED -> if NULL and design uploaded  -> ui print WARNING
  output$warnmsg <- reactive({
    print('Check if WARNING msg needed')
    bool <- FALSE ##default
    if (!is.null(input$input_countFile_design)) {
        if (is.null(getData_design())) {
        bool <- TRUE ##if design upload and doens't match
        }
    }
    print(bool)
    return(bool)
  })
  outputOptions(output, 'warnmsg', suspendWhenHidden=FALSE)
  
  output$success <- reactive({
    print('Check if success msg needed')
    bool <- FALSE ##default
    if (!is.null(input$input_countFile_design)) {
      if (!is.null(getData_design())) {
        bool <- TRUE ##if design upload and doens't match
      }
    }
    print(bool)
    return(bool)
  })
  outputOptions(output, 'success', suspendWhenHidden=FALSE)

  ## OBSERVED TABLE's ROWS SELECTED BY USER
  input_selected_from_table <- reactive({
    print("In: Main Table List Observer.")
    selected_library <- DataBase[input$DB_table_rows_selected, ]
    return(selected_library)
    }) ## End of observeEvent
  
  
  ## OBSERVED INPUT FILE UPLOADED
  input_from_upload <- reactive({
    print("BUILT info for uploaded count files")
    ## If counts file upladed
    if (!is.null(getData())) {
      inFile <- input$input_countFile
      ## If design file not uploaded
      if (is.null(getData_design())){
            print("Built file uploaded info WITHOUT design file")
            inFile_info <- setnames(data.frame(matrix(ncol=length(colnames(DataBase)), nrow = length(inFile$name))), colnames(DataBase))
            inFile_info$Run <- inFile$name %>% str_remove(paste(".",file_extension(.), sep = ""))
            inFile_info$path <- inFile$datapath
            inFile_info$BioSample <- inFile$name
      }
      else {
            print("Built file uploaded info WITH design file")
            design <- getData_design()
            inFile_info <- design
            file_path <- inFile$datapath %>% set_names(inFile$name %>% str_remove(paste(".", file_extension(.), sep = "")))
            inFile_info$path <-  file_path[inFile_info$Run]
      }
      return(inFile_info) # Returns data.frame with info
    }
    ## Else returns empty dataframe
    else return(data.frame())
    
  })
  
  ## Input data available
  input_data_available <- reactive({
    input_dta <- DataBase
    if (!is.null(getData_design())) {
      print("Binding DataBase with input upload")
      input_dta <- input_dta  %>% bind_rows(.,input_from_upload())
      return(input_dta)
    }
    if (!is.null(getData())) {
      print("Binding DataBase with input upload")
      input_dta <- input_dta  %>% bind_rows(.,input_from_upload())
      return(input_dta)
    }
    else {
      return(input_dta)
    }
  })
  
  ### MERGE DATA WITH DEV STAGE FROM DB WITH DATA UPLOADED (if dev_stage precised in design file)
  data_for_timeSeries_available <- reactive({
    if (!is.null(getData_design())){
      print('Update RUN TPS: with new run uploaded')
      new_dta <- getData_design() %>%
        filter(dev_stage != is.na(.))
      dta_available <- DB_for_times_series  %>% bind_rows(., new_dta)
      #run_tps_available <- c(run_with_tps_label_fromDB, new_run_with_tps_label$Run)
    }
    else{
      dta_available <- DB_for_times_series
      #run_tps_available <- run_with_tps_label_fromDB
    }
    #print(run_tps_available)
    return(dta_available)
  })
  
  ## UPDATE INPUT RUN BOUTONS ACCORDING TO USER'S INPUT
  ## N.B: large panel of choice needs to be precised on server side !
  observe({
    print("Observe: Update input data available and choice")
    input_data_avail <- input_data_available()
    run_choices <-  input_data_avail$Run 
  
    input_from_table <- input_selected_from_table()
    input_uploaded <- input_from_upload()
    ## Combine run from DB and run uploaded
    run_selected <- c(input_from_table$Run, input_uploaded$Run)
  
    updateSelectizeInput(session, "Run", choices = run_choices, selected = run_selected, server = TRUE)
    run_with_tps_label_available <- data_for_timeSeries_available()
    updateSelectizeInput(session, "Run_tps", choices = run_choices[run_choices  %in% run_with_tps_label_available$Run], selected = run_selected[run_selected %in% run_with_tps_label_available$Run], server = TRUE)
  })
  
  observe({  
    print("Observe: upadate gene annotation choices from server (cause lot of choice)")
    updateSelectizeInput(session, "gene_go", choices = names(gene_go), server = TRUE)
    updateSelectizeInput(session, "gene_interpro", choices = names(gene_interpro), server = TRUE)
    
  })
  
  ## UPDATE CHOICE OF GENE
  ## N.B: large panel of choice needs to be precised on server side !
  observe({  
    print("Observe: upadate gene choices from server (because lot of choice)")
    updateSelectizeInput(session, "gene", choices = liste_gene$id, server = TRUE)
  })
  
  data2plot <- reactive({
    print('Filter interest Run in all data available (DB + input upload)')
    dtf <- input_data_available() %>% filter(Run %in% input$Run)
    print(dtf)
  })
  
  getList_gene <- reactive({
    print("Get List gene")
    gene_from_GO <- gene_go[input$gene_go]  %>% flatten() %>% unlist()
    gene_from_INTERPRO <- gene_interpro[input$gene_interpro]  %>% flatten() %>% unlist()
    print(gene_from_INTERPRO)
    if(input$operator == "Union"){
      return(unique(c(input$gene, gene_from_GO, gene_from_INTERPRO)))
    }
    if(input$operator == "Intersection"){
      ##deal with every cases of submission
      ##case 1: gene from INTERSECT and from GO
      if (!is.null(gene_from_GO) && !is.null(gene_from_INTERPRO)){
        intersection <- intersect(gene_from_INTERPRO, gene_from_GO)
      }
      ##case 2: only from gene FROM GO
      if (!is.null(gene_from_GO) && is.null(gene_from_INTERPRO)){
        intersection <-  gene_from_GO
      }
      ##case 3: only from gene FROM INTERSECT
      if (is.null(gene_from_GO) && !is.null(gene_from_INTERPRO)){
        intersection <-  gene_from_INTERPRO
      }
      ##case 4: no submission from GO or INTERSECT
      if (is.null(gene_from_GO) && is.null(gene_from_INTERPRO)){
        intersection <-  c()
      }
      return(unique(c(input$gene,intersection)))
    }
  })
  
  getChoiceClustering <- reactive({
    if (input$clustering == "Both") {
      choice = "both"
    } 
    if (input$clustering == "None"){
      choice = "none"
    }
    if (input$clustering == "Gene"){
      choice = "row"
    }
    if (input$clustering == "Sample"){
      choice = "column"
    }
    return (choice) })
  
  ##############################################################
  ### RAW COUNTS 
  ##############################################################
  
  getCounts <- reactive({
    print("Convert file counts to dtf")
    counts2dtf(data2plot())
  })
  
  getMatrice <- reactive({
    print("Convert dtf 2 matrice")
    getCounts() %>% dtf2matrix()
  })
  
  getDESeq <- reactive({
    print('Convert matrice 2 Deseq object')
    fun <- function(x) {
      return(x+1)
    }
    mat <- getMatrice()
    matrice <- mat %>% mutate_all(fun) 
    rownames(matrice) <- rownames(mat)
    ds <- matrix2deseq(matrice,data2plot())
    ds <- estimateSizeFactors(ds)
    return(ds)
  })
  


  ######################################################################
  ## NORMALIZATION : RPKM
  ######################################################################
  
  getCounts_RPKM <- reactive({
    print("Get dtf counts normalized by RPKM")
    getCounts()%>%
      group_by(Run) %>%
      mutate(RPKM = rpkm(counts, liste_gene$length)) %>%
      mutate(counts = RPKM ) %>%
      select(-RPKM)
  })
  
  getMatrice_RPKM <- reactive({
    print("Get matrix counts normalized by RPKM")
    getCounts_RPKM() %>% dtf2matrix()
  })
  
  getDESeq_RPKM <- reactive({
    print("Get Dseq obj counts normalized by RPKM")
    
    ## Convert count to integer -> needed for DESEQ transform
    matrice <- round(getMatrice_RPKM())
    ds <-  matrix2deseq(matrice,data2plot())
    return(ds)
  })
  
  ######################################################################
  ##NORMALIZATION : TPM
  ######################################################################
  
  getCounts_TPM <- reactive({
    print("Get dtf counts normalized by TPM")
    getCounts() %>% 
    group_by(Run) %>%
      mutate(TPM = tpm(counts, liste_gene$length)) %>%
      mutate(counts = TPM) %>%
      select(-TPM)
  })
  
  getMatrice_TPM<- reactive({
    print("Get matrix counts normalized by TPM")
    getCounts_TPM() %>% dtf2matrix()
  })
  
  getDESeq_TPM <- reactive({
    print("Get Dseq obj counts normalized by TPM")
    ## Convert count to integer -> needed for DESEQ transform
    matrice <- round(getMatrice_TPM())
    ds <- matrix2deseq(matrice, data2plot())
    #ds <- estimateSizeFactors(ds)
    return(ds)
  })
  
  ######################################################################
  ## NORMALIZATION : DESEQ
  ######################################################################
  
  getCounts_DESEQ <- reactive({
    print("Get dtf counts normalized by DESEQ method")
    c <- getMatrice_DESEQ() %>% rownames_to_column() %>%
      reshape2::melt( ., id=c("rowname"), variable.name = "Run") %>%
      mutate(counts= value) %>%
      mutate(gene = rowname) %>%
      select(c(-rowname,-value))
  })
  
  getMatrice_DESEQ<- reactive({
    print("Get matrix counts normalized by DESEQ method")
    as.data.frame(counts(getDESeq(), normalized = TRUE))
  })
  
  getDESeq_DESEQ <- reactive({
    print("Get Deseq obj counts normalized by DESEQ method")
    
    ## Convert count to integer -> needed for DESEQ transform
    matrice <- round(getMatrice_DESEQ())
    ds <- matrix2deseq(matrice, data2plot()) 
  })
  
  ########################################################################
  ##INPUT FOR VISUALISATION
  ########################################################################
  
  get_input_MATRICE <- reactive({
    print("Get matrice input for COUNTS MATRIX PANEL & HEATMAP PANEL")
    
    if(input$norm == "None"){
      matrice <- getMatrice() %>% filter(rownames(.) %in% getList_gene())
    }
    if(input$norm == "TPM"){
      matrice <- getMatrice_TPM() %>% filter(rownames(.) %in% getList_gene())
    }
    if(input$norm == "RPKM"){
      matrice <- getMatrice_RPKM() %>% filter(rownames(.) %in% getList_gene())
    }
    if(input$norm == "DESEQ"){
      matrice <- getMatrice_DESEQ() %>% filter(rownames(.) %in% getList_gene())
    }
    return (matrice)
  })
  
  get_input_DTF <- reactive({
    print("Get dataframe input for BARPLOT PANEL")
    
    if(input$norm == "None"){
      dtf <- getCounts() %>% filter(gene %in% input$gene)
    }
    if(input$norm == "TPM"){
      dtf <- getCounts_TPM() %>% filter(gene %in% input$gene)
    }
    if(input$norm == "RPKM"){
      dtf <- getCounts_RPKM() %>% filter(gene %in% input$gene)
    }
    if(input$norm == "DESEQ"){
      dtf <- getCounts_DESEQ() %>% filter(gene %in% input$gene)
    }
    return(dtf)
  })
  
  
  get_input_DESEQ <- reactive({
    print("Get Deseq obj input for ACP PANEL & TREE PANEL")
    
    if(input$norm == "None"){
      ds <- getDESeq()
    }
    if(input$norm == "TPM"){
      ds <- getDESeq_TPM()
    }
    if(input$norm == "RPKM"){
      ds <- getDESeq_RPKM()
    }
    if(input$norm == "DESEQ"){
      ds <- getDESeq_DESEQ()
    }
    return(ds)
  })
  
  
  ########################################################################
  ## COUNTS TABLE
  ########################################################################
  
  ## PLOT COUNTS TABLE 
  output$counts_table <- DT::renderDataTable({
    list_gene <- getList_gene()
    shiny::validate(
     need(input$Run, 'Choose at least one run from DataBase!'),
     need(length(list_gene) > 0, 'Choose at least one annotation or 2 genes!'))
    
    matrice = get_input_MATRICE()
    datatable(data = matrice)
  }) # End of renderDataTable
  
  ########################################################################
  ## BUILD FIGURES
  ########################################################################
  
  getThemeForBackground <- reactive({
    if (input$themeToggle == FALSE) {
      backgroundTheme <- "white"
      labelsTheme <- "#222222"
    } else {
      backgroundTheme <- "#222222"
      labelsTheme <- "white"
    }
    return (list(backgroundTheme, labelsTheme))
  })
  
  ## Generate ACP
  output$acp <- renderPlotly({
    print("Built ACP")
    print(input$condition)
    data2plot <- data2plot()
    list_biosample <- data2plot$BioSample
    ## Check input value
    shiny::validate(need(input$Run >= 2, 'Choose at least 2 runs from DataBase!'),
                    need(length(list_biosample) != length(unique(list_biosample)), "Choose at least 2 replicates (twice the same BioSample)!"),
                    need(length(unique(list_biosample)) > 1, "Choose at least 2 different Biosamples!"))
    ds <- get_input_DESEQ()
    print(is.na(unique(ds[[input$condition]])))
    shiny::validate(need(!is.na(unique(ds[[input$condition]])), 'You cannot choose this condition : it is not filled in the table.'))
    plotColors <- getThemeForBackground()
    plot_acp(ds, input$condition, plotColors)
  })
    
    
  ## Generate tree 
  output$tree <- renderPlot({
    print("Built Tree")
    data2plot <- data2plot()
    list_biosample <- data2plot$BioSample
    ## Check input value
    shiny::validate(need(input$Run >= 2, 'Choose at least 2 runs from DataBase!'),
                    need(length(unique(list_biosample)) > 1, "Choose at least 2 different Biosamples!"))
    
    ds <- get_input_DESEQ()
    plotColors <- getThemeForBackground()
    plot_tree(ds, plotColors)
  })
    
  ## Generate heatmap
  output$heatmap <- renderPlotly({
    print("Built Heatmap")
    
    ## Check input value
    list_gene <- getList_gene()
    print(list_gene)
    shiny::validate(
      need(input$Run > 2, 'Choose at least two runs from DataBase!'),
      need(length(list_gene)>=2, 'Choose at least one annotation or 2 genes!'))
    
    ##non interactive
    #mat <- get_input_MATRICE() %>% filter_all(., any_vars(. != 0))
    #gplots::heatmap.2(
    #  as.matrix(mat),
    #  trace = "none",
    #  col = viridis(100),
    #  key = FALSE,
    #  scale="row"
    #)
    ##interactive
    mat <- get_input_MATRICE() %>% filter_all(., any_vars(. != 0))
    plotColors <- getThemeForBackground()
     heatmaply(
      as.matrix(mat),
      colors = cool_warm,
      seriate = "mean",
      dendrogram = c(getChoiceClustering()),
      row_dend_left = TRUE,
      plot_method = "plotly"
    ) %>% layout(plot_bgcolor=plotColors[[1]], paper_bgcolor = plotColors[[1]], font = list(color = plotColors[[2]]))

    ## Old method
    #plot_heatmap(mat, plotColors)
  })
    
  ## BARPLOT OUTPUT
  output$barplot <- renderPlotly({
    print("Built barplot")
    
    ## Check input value
    shiny::validate(
      need(input$Run, 'Choose at least one run from DataBase!'),
      need(input$gene, 'Choose at least one gene!'))    
   
    dtf <- get_input_DTF()
    plotColors <- getThemeForBackground()
    plot_barplot(dtf, plotColors)
  })
    
  ####################################################################################
  ## TIME SERIES TRACK
  ###################################################################################
    
  getCounts_with_tps_label<- reactive({ 
    print("Get dtf raw counts for TIMES SERIES PANEL")
    
    dtf <- data.frame()
    ## If input run have been used
    if (!is.null(input$Run)) {
      dtf <- getCounts() %>% 
        filter(Run %in% input$Run_tps) %>%
        filter(gene %in% input$gene) %>%
        inner_join(data2plot(), by="Run")
    }
    ### If run selection have been realized directly from TEMPORAL TRACKING PANEL
    if (nrow(dtf) == 0 && !is.null(input$Run_tps)) {
      dtf <- input_data_available() %>% 
        filter(Run %in% input$Run_tps) %>% 
        counts2dtf()  %>%  
        filter(gene %in% input$gene) %>%
        inner_join(input_data_available(), by = "Run")
    }
    return(dtf)
  })
  
  getCounts_with_tps_label_TPM <- reactive({
    print("Get dtf counts normalized by TPM for TIMES SERIES PANEL ")
    
    dtf<-data.frame()
    
    ## If input run have been used
    if (!is.null(input$Run)){
      dtf <- getCounts_TPM() %>% 
        filter(Run %in% input$Run_tps) %>%
        filter(gene %in% input$gene) %>%
        inner_join(data2plot(), by = "Run")
    }
    ### If run selection have been realized directly from TEMPORAL TRACKING PANEL
    if (nrow(dtf) == 0 && !is.null(input$Run_tps)) {
      dtf <- input_data_available() %>% 
        filter(Run %in% input$Run_tps) %>% 
        counts2dtf()  %>%
        group_by(Run) %>%
        mutate(TPM = tpm(counts, liste_gene$length)) %>%
        mutate(counts = TPM) %>%
        select(-TPM) %>%
        filter(gene %in% input$gene) %>%
        inner_join(input_data_available(), by="Run")
    }
    return(dtf)
  })
  
  getCounts_with_tps_label_RPKM <- reactive({ 
    print("Get dtf counts normalized by RPKM for TIMES SERIES PANEL ")
    
    dtf<-data.frame()
    
    ## If input run have been used
    if (!is.null(input$Run)) {
      dtf <- getCounts_RPKM() %>% 
        filter(Run %in% input$Run_tps) %>%
        filter(gene %in% input$gene) %>%
        inner_join(data2plot(), by="Run")
    }
    
    ### If run selection have been realized directly from TEMPORAL TRACKING PANEL
    if (nrow(dtf) == 0 && !is.null(input$Run_tps)) {
      dtf <- input_data_available() %>% 
        filter(Run %in% input$Run_tps) %>% 
        counts2dtf() %>%
        group_by(Run) %>%
        mutate(RPKM = rpkm(counts, liste_gene$length)) %>%
        mutate(counts = RPKM) %>%
        select(-RPKM) %>%
        filter(gene %in% input$gene) %>%
        inner_join(input_data_available(), by="Run")
    }
    return(dtf)
  })
  
  getCounts_with_tps_label_DESEQ <- reactive({ 
    print("Get dtf counts normalized by DESEQ method for TIMES SERIES PANEL ")
    
    dtf<-data.frame()
    
    ## If input run have been used
    if (!is.null(input$Run)) {
      dtf <- getCounts_DESEQ() %>% 
        filter(Run %in% input$Run_tps) %>%
        filter(gene %in% input$gene) %>%
        inner_join(data2plot(), by="Run")
    }
    ### If run selection have been realized directly from TEMPORAL TRACKING PANEL
    if (nrow(dtf) == 0 && !is.null(input$Run_tps)) {
      matrice <- input_data_available() %>% 
        filter(Run %in% input$Run_tps) %>% 
        counts2dtf() %>%
        dtf2matrix()
      
      sampleInfo <- input_data_available() %>% filter(Run %in% input$Run_tps)
      ds <- matrix2deseq(matrice, sampleInfo)
      ds <- estimateSizeFactors(ds)
      
      dtf < -as.data.frame(counts(ds,normalized=TRUE)) %>% 
        rownames_to_column() %>%
        reshape2::melt(., id = c("rowname"), variable.name = "Run") %>%
        mutate(counts = value) %>%
        mutate(gene = rowname) %>%
        select(c(-rowname,-value)) %>% 
        filter(gene %in% input$gene) %>%
        inner_join(input_data_available(), by="Run")
    }
    return(dtf)
  })
  
  get_input_TIMESERIES <- reactive({
    print("Get dtf input for TIMES SERIES PANEL ")
    
    if (input$norm == 'None') {
      dtf <- getCounts_with_tps_label()
    }
    if(input$norm == 'RPKM'){
      dtf <- getCounts_with_tps_label_RPKM()
    }
    if(input$norm == 'TPM'){
      dtf <- getCounts_with_tps_label_TPM()
    }
    if(input$norm == 'DESEQ'){
      dtf <- getCounts_with_tps_label_DESEQ()
    }
    return(dtf)
  })
  
  ## Time series PLOT
  ## Order tmp label
  # Render bucket list
  output$dev_stagelist <- renderUI({
    print("RenderUI: Bucketlist dev stage")
    bucket_list(   
      header = "Drag and drop development stage order",
      #group_name = "devStage_list_group",
      orientation = "horizontal",
      add_rank_list(text = "Development stage",
                    labels = dev_stagelistlabels(), # labels from row selection
                    input_id = "dev_stage_list",
                    options = sortable_options(multiDrag = TRUE))
    )
    
  })
  
# Reactive expression to create labels from rows selected
  dev_stagelistlabels <- reactive({
    print("Update: dev_stage list available according to data available")
    interest_data <- data_for_timeSeries_available() %>% 
      filter(Run %in% input$Run_tps)
    dev_label <- unique(interest_data$dev_stage)
    return(dev_label)
  })
  
  output$list_devStage <- renderPrint({
    print(input$dev_stage_list)
    # This matches the input_id of the rank list
  })
  
  output$tmp_series <- renderPlotly({
    print('Built Time series plot')
    shiny::validate(
      need(input$Run_tps, 'Choose at least one run from DataBase!'),
      need(input$gene >= 1, 'Choose at least 1 gene!')
    )
    
    dtf <- get_input_TIMESERIES()
    plotColors <- getThemeForBackground()
    plot_timeSeries(dtf, input$dev_stage_list, plotColors)
  })
  
  ####################################################################################
  ## DOWNLOAD COUNTS
  ###################################################################################
 
  output$download_counts <- downloadHandler(
    filename = function() { 
      paste("counts", '.tsv', sep='') 
    },
    content = function(file) {
      write.table(get_input_MATRICE(), file, sep = "\t", row.names = TRUE)
    })
}) ## End shinyServer