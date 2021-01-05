############################################################################  
## UI side
############################################################################

source("global.R")

# Collects all of the tab UIs
ui <- fluidPage(
  theme = shinytheme("flatly"),
  checkboxInput(
    inputId = "themeToggle",
    label = icon("moon")
  ),
  tagList(
    tags$head(
      tags$script(type="text/javascript", src = "logo.js")
    ),
    tags$script(type = "text/javascript", src = "themeSwitch.js"),
    tags$head(
      tags$style(HTML(".rank-list-item {color: black;}"))
    ),
    tags$head(includeScript("google-analytics.js")),
    navbarPage(
      title = "Acyrthosiphon Pisum Transcriptome Analysis",
      tabPanel("Introduction",
        includeMarkdown("../README.md")
      ),
      
    ## =========================================================================== ##
    ## DOWNLOAD DATA TABS
    ## =========================================================================== ##
      tabPanel("Input Data", 
             ## ==================================================================================== ##
             ## Left hand column has the input data settings and options
             ## ==================================================================================== ##
                fluidPage(
                  sidebarLayout(
                    sidebarPanel(fileInput("input_countFile", "Upload your own samples",
                                              multiple = TRUE,
                                              accept = c("text/tsv",
                                                          "text/tab-separated-values,text/plain",
                                                           ".tsv")),
                                  conditionalPanel("(output.fileUploaded == 'countFile_unload')",
                                                    alert(status = "danger",tags$b("Error!"), 
                                                          "Incorrect count file format"),
                                                   img(src="examplecountfiledata.png",width="100%"),
                                                   tags$ul(
                                                     tags$li("File counts without header row."), 
                                                     tags$li("2 columns: Gene, Counts"),
                                                     tags$li("18284 rows/genes per count file"),
                                                     tags$li("Template: my_data_ex/countFiles/")
                                                   )
                                                   ),
                                  conditionalPanel("output.fileUploaded == true", 
                                                    fileInput("input_countFile_design", "Upload your design file",
                                                                multiple = FALSE,
                                                                accept = c("text/tsv",
                                                                            "text/tab-separated-values,text/plain",
                                                                            ".tsv"))),
                                  conditionalPanel("output.warnmsg == true", 
                                                    alert(status = "warning", 
                                                          tags$b("Warning!"), 
                                                          "Incorrect design file format"),
                                                   img(src="exampleDesignFiledata.png",width="100%"),
                                                   tags$ul(
                                                     tags$li("File with 18 columns header"), 
                                                     tags$li("Columns format names follow rigth table colnames"),
                                                     tags$li("As many rows as counts files uploads"),
                                                     tags$li("Column Run: sample count file name without extension"),
                                                     tags$li("You can leave cells empty, except required"),
                                                     tags$li("Required columns : Run, BioSample and dev_stage for time series"),
                                                      tags$li("Template: my_data_ex/design_my_data.tsv")
                                                   )
                                                   ),
                                 conditionalPanel("output.success == true", 
                                                  alert(status = "success", 
                                                        tags$b("Success!"), 
                                                        "Count & Design files Uploaded"))
                                  
        
                    ),
                    mainPanel(DT::dataTableOutput("DB_table"), style = "height:800px; overflow-y: scroll;overflow-x: scroll;")
                  )
                )
    ), # End tabPanel,
    tabPanel("Visualisation", 
             ## ==================================================================================== ##
             ## Left hand column has the input data settings and options
             ## ==================================================================================== ##
             fluidPage(
               sidebarLayout( 
                 sidebarPanel (
                   # Ajout de condition d'affichage selon le type de graphique voulu
                   conditionalPanel("input.id_tabset=='ACP' || input.id_tabset=='Tree' || 
                                    input.id_tabset=='Heatmap'|| input.id_tabset=='Barplot' || input.id_tabset=='Counts matrix'" ,
                                    selectizeInput(inputId  = "Run",
                                                    label ="Choose run(s)",
                                                    choices = character(0),
                                                    multiple = TRUE,
                                                    size = 6,
                                                    selected = character(0))),
                    conditionalPanel("input.id_tabset=='Time series'", # Other input for run with dev_stage field 
                                      selectizeInput(inputId  = "Run_tps",
                                                      label = "Choose run(s)",
                                                      choices = character(0),
                                                      multiple = TRUE,
                                                      size = 6,
                                                      selected = character(0))),
                   conditionalPanel("input.id_tabset=='Heatmap'",
                                    radioButtons(inputId = "clustering",
                                                 label = "Choose the way to apply clustering",
                                                 choices = c("None",
                                                             "Gene",
                                                             "Sample",
                                                             "Both"),
                                                 selected = "None")
                                    ),
                    conditionalPanel("input.id_tabset=='Heatmap' || input.id_tabset=='Counts matrix'",
                                     
                                     selectizeInput(inputId ='gene_go',
                                                     label = "Choose gene(s) to visualize from GO",
                                                     choices = character(0),
                                                     multiple = TRUE,
                                                     size = 6),
                                     radioButtons(inputId = "operator",
                                                  label = "Operator between gene(s) selected from GO/INTERSECT",
                                                  choices = c("Union",
                                                              "Intersection"),
                                                  selected = "Union"),
                                   selectizeInput(inputId ='gene_interpro',
                                                  label = "Choose gene(s) to visualize from INTERPRO",
                                                  choices = character(0),
                                                  multiple = TRUE,
                                                  size = 6)),
                    conditionalPanel("input.id_tabset=='Barplot' || input.id_tabset=='Time series'|| input.id_tabset=='Heatmap' || input.id_tabset=='Counts matrix'",
                                     selectizeInput(inputId = 'gene',
                                                    label = "Choose gene(s) to visualize",
                                                    choices = character(0),
                                                    multiple = TRUE,
                                                    size = 6)),
                    conditionalPanel("input.id_tabset=='ACP'",
                                      selectizeInput(inputId = 'condition',
                                                     label = "Choose condition",
                                                     choices = colnames(DataBase) ,
                                                     multiple = FALSE,
                                                     size = 6,
                                                     selected ="Run" )),
                    radioButtons(inputId = "norm",
                                 label = "Counts normalization",
                                 choices = c("None",
                                           "TPM",
                                           "RPKM",
                                           "DESEQ"),
                                  selected = "None"),
                    conditionalPanel("(input.id_tabset=='Time series' && input.Run_tps !== 'undefined' && input.Run_tps.length > 0)",
                                     htmlOutput("dev_stagelist"))#,
                                     #verbatimTextOutput("list_devStage"))
                 ),
                 #mainPanel(DT::dataTableOutput("library_table"))))
                 mainPanel(
                   column(12,
                          tabsetPanel(id = "id_tabset",
                            tabPanel(title = "ACP",
                                     shinycssloaders::withSpinner(plotlyOutput("acp",  width = "100%"))
                            ),
                            tabPanel(title = "Tree",
                                     shinycssloaders::withSpinner(plotOutput("tree", width = "100%"))
                                     
                            ),
                            tabPanel(title = 'Counts matrix',
                                     DT::dataTableOutput("counts_table"),
                                     br(),
                                     br(),
                                     downloadButton('download_counts', 'Download Data'),
                                     style = "height:610px; overflow-y: scroll;overflow-x: scroll;",
                            ),
                            tabPanel(title = "Heatmap",
                                     shinycssloaders::withSpinner(plotlyOutput("heatmap", width = "100%"))
                            ),

                            tabPanel(title = "Barplot",
                                     shinycssloaders::withSpinner(plotlyOutput("barplot", width="100%"))
                            ),
                            tabPanel(title = "Time series",
                                     shinycssloaders::withSpinner(plotlyOutput("tmp_series", width = "100%"))
                            )
                          )
                   )
                 )
               )
             )
    )
    # End tabPanel
    
    # Source("ui_visualisation.R",local=TRUE)$value,
    # End definitions of tabs, now footer
    ## ============================================================================ ##
    ## INFO TAB
    ## ============================================================================ ##   
    
    ## ==================================================================================== ##
    ## FOOTER
    ## ==================================================================================== ##              
    
    ## ==================================================================================== ##
    ## END
    ## ==================================================================================== ##
  ) #End navbarPage
 ) #End tagList
) #End fluidPage