tabItem(tabName = "findMarkersTab",

        fluidRow(

          column(12,
                 h3(strong("Finding differentially expressed genes (cluster biomarkers)")),
                 hr(),

                 column(12,
                        shiny::tags$div(class = "BoxArea2",
                                 p("Seurat can help you find markers that define clusters via differential expression. By default, it identifes positive and negative markers of a single cluster (specified in ident.1), compared to all other cells. FindAllMarkers automates this process for all clusters, but you can also test groups of clusters vs. each other, or against all cells."),
                                 p("The min.pct argument requires a gene to be detected at a minimum percentage in either of the two groups of cells, and the thresh.test argument requires a gene to be differentially expressed (on average) by some amount between the two groups. You can set both of these to 0, but with a dramatic increase in time - since this will test a large number of genes that are unlikely to be highly discriminatory. As another option to speed up these computations, max.cells.per.ident can be set. This will downsample each identity class to have no more cells than whatever this is set to. While there is generally going to be a loss in power, the speed increases can be significiant and the most highly differentially expressed genes will likely still rise to the top."),

                                 tabsetPanel(id = "fm", type = "tabs",
                                             tabPanel("Find markers by cluster",

                                                      column(12,
                                                             wellPanel(
                                                               h4("Select clusters to find markers:"),
                                                               column(4,selectInput("clusterNum", "Cluster Num (ident.1)",
                                                                                    choices = c(1,2,3,4), selected = 1)),
                                                               column(4,numericInput("minPct", "Min % (min.pct)", value = 0.25)),
                                                               column(4,selectInput("testuse", "Test to use",
                                                                                    choices = c("wilcox","bimod","roc","t","tobit","poisson","negbinom","MAST","DESeq2")
                                                                                    , selected = "wilcox")),
                                                               column(4, checkboxInput("onlypos","Show Only Positive Markers"), value = TRUE),
                                                               div(style = "clear:both;"),
                                                               actionButton("findClusterMarkers","Find Cluster Markers",class = "button button-3d button-block button-pill button-primary button-large", style = "width: 100%")
                                                             ),
                                                             conditionalPanel("output.clusterMarkersAvailable",
                                                                              downloadButton('downloadClusterMarkersCSV','Save Results as CSV File', class = "btn btn-primary")
                                                             ),
                                                             br(),
                                                             withSpinner(dataTableOutput('clusterMarkers'))
                                                      ),
                                                      shiny::tags$div(class = "clearBoth")
                                             ),
                                             tabPanel("Find markers by cluster vs other clusters",
                                                      column(12,
                                                             wellPanel(
                                                               h4("Select clusters to find markers:"),
                                                               column(3,selectInput("clusterNumVS1", "Cluster Num (ident.1)",
                                                                                    choices = c(1,2,3,4), selected = 1)),
                                                               column(3,selectInput("clusterNumVS2", "Cluster Num (ident.2)",
                                                                                    choices = c(1,2,3,4), selected = NULL, multiple = TRUE)),
                                                               column(3,numericInput("minPctvs", "Min % (min.pct)", value = 0.25)),
                                                               column(3,selectInput("testuseVS", "Test to use",
                                                                                    choices = c("wilcox","bimod","roc","t","tobit","poisson","negbinom","MAST","DESeq2")
                                                                                    , selected = "wilcox")),
                                                               column(4, checkboxInput("onlyposVS","Show Only Positive Markers"), value = TRUE),
                                                               div(style = "clear:both;"),
                                                               actionButton("findClusterMarkersVS","Find Cluster Markers",class = "button button-3d button-block button-pill button-primary button-large", style = "width: 100%")
                                                             ),
                                                             conditionalPanel("output.clusterMarkersVSAvailable",
                                                                              downloadButton('downloadClusterMarkersVSCSV','Save Results as CSV File', class = "btn btn-primary")
                                                             ),
                                                             br(),
                                                             withSpinner(dataTableOutput('clusterMarkersVS'))),
                                                      shiny::tags$div(class = "clearBoth")
                                             ),
                                             tabPanel("Find all markers",
                                                      column(12,
                                                             wellPanel(
                                                               h4("Find All Markers:"),
                                                               column(4,numericInput("minPctAll", "Min % (min.pct)", value = 0.25)),
                                                               column(4,selectInput("testuseAll", "Test to use",
                                                                                    choices = c("wilcox","bimod","roc","t","tobit","poisson","negbinom","MAST","DESeq2")
                                                                                    , selected = "wilcox")),
                                                               column(4,numericInput("threshAll", "Logfc Thresh", value = 0.25)),
                                                               
                                                               column(4, checkboxInput("onlyposAll","Show Only Positive Markers"), value = TRUE),
                                                               div(style = "clear:both;"),
                                                               actionButton("findClusterMarkersAll","Find Cluster Markers",class = "button button-3d button-block button-pill button-primary button-large", style = "width: 100%")
                                                             ),
                                                             conditionalPanel("output.clusterMarkersAllAvailable",
                                                                              column(4, downloadButton('downloadClusterMarkersAllCSV','Save Results as CSV File', class = "btn btn-primary")),
                                                                              column(8, numericInput("numGenesPerCluster", "# top genes to show per cluster (0 to show all)", value = 0))
                                                             ),
                                                             br(),
                                                             withSpinner(dataTableOutput('clusterMarkersAll'))),
                                                      shiny::tags$div(class = "clearBoth")
                                             )
                                 )
                        )

                 ),
                 shiny::tags$div(class = "clearBoth")



          )
        )
)
