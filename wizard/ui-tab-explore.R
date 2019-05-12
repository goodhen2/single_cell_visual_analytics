tabItem(tabName = "exploreTab",
        
        fluidRow(
          
          column(12,
                 # h3(strong("Run Non-linear dimensional reduction (tSNE)")),
                 # hr(),
                 
                 column(12,
                        actionButton("runExplore","Run",class = "button button-3d button-block button-pill button-primary button-large", style = "width: 100%"))
                 ,
                 
                 hr(),
                 hr(),
                 hr(),
                 
                 shiny::tags$div(class = "BoxArea2",
         
                                 
                                 # column(12,
                                 #        conditionalPanel("output.exploreAvailable",
                                 #                         
                                 #                
                                 #                         # fluidRow(
                                 #                         #   column(12, verbatimTextOutput("test"))
                                 #                         # ),
                                 # 
                                 #                         fluidRow(
                                 #                           column(3, NULL),
                                 #                           column(6, numericInput("threshold", "pic.1 threshold", 0.7)),
                                 #                           column(3, numericInput("threshold2", "avg_logFC threshold", 2.0))
                                 #                         ),
                                 #                         
                                 #                         fluidRow(
                                 #                           column(3, plotlyOutput("plotly"))
                                 #                           ,
                                 #                           column(6, DT::dataTableOutput("genelist")),
                                 #                           column(3, plotlyOutput("circles"))
                                 #                         )
                                 # 
                                 #                         #hr()
                                 #                         
                                 #        )
                                 #        
                                 # )
                                 
                                 column(12,
                                        conditionalPanel("output.exploreAvailable",
                                                         # column(12, 
                                                         #        fluidRow(
                                                         #          column(3, NULL),
                                                         #          column(6, numericInput("threshold", "pic.1 threshold", 0.7)),
                                                         #          column(3, numericInput("threshold2", "avg_logFC threshold", 2.0))
                                                         #        ))
                                                         #        
                                                         # ,
                                                         # 
                                                         # column(3,
                                                         #        plotlyOutput("plotly"),
                                                         #        plotlyOutput("bar_chart")),
                                                         # 
                                                         # column(6, DT::dataTableOutput("genelist")),
                                                         # 
                                                         # column(3, plotlyOutput("circles"))
                                                         
                                                         
                                                         
                                                         column(12, 
                                                               fluidRow(
                                                                 column(3, numericInput("threshold2", "avg_logFC threshold", 2.0)),
                                                                 column(3, NULL),
                                                                 column(6, numericInput("threshold", "Ratio threshold", 0.7))
                                                                 
                                                               ))

                                                        ,
                                                         column(6,
                                                                fluidRow(
                                                                  column(6, plotlyOutput("circles")),
                                                                  column(6, plotlyOutput("plotly"))
                                                                ),
                                                                
                                                                hr(),

                                                                fluidRow(
                                                                  column(12, plotlyOutput("bar_chart"))
                                                                )),
                                                         column(6, 
                                                                fluidRow(
                                                                  column(12, DT::dataTableOutput("genelist"))
                                                                ),
                                                                hr(),
                                                                fluidRow(
                                                                  column(12, verbatimTextOutput("genelist2"))
                                                                )
                                                         
                                                         
                                                         )
                                        )
                                 ),
                                 shiny::tags$div(class = "clearBoth")
                                 
                                 
                                 
                 )
          )
        )
)