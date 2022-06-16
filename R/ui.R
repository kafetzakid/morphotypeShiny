## UI ##

ui = tags$div(fluidPage(theme = shinytheme("journal"),
                        headerPanel("Map & Cluster SRSW based on morphological features"),
                        tabsetPanel(
                          tabPanel("Network",
                                   fluidRow(
                                     column(width = 8, align = "center",
                                            selectInput("nodecolor", h5("Select node color"), choices = listcolors))
                                   ),
                                   fluidRow(
                                     column(width = 8,
                                            visNetworkOutput("network_graph", height = "1000px")
                                     ),
                                     column(width = 4,
                                            plotOutput("plotSherdHeightWidthRatio", height = "100px"),
                                            br(),
                                            plotOutput("plotSherdIncl", height = "100px"),
                                            br(),
                                            plotOutput("plotSherdRimDiam", height = "100px"),
                                            br(),
                                            plotOutput("plotSherdWTat2.3", height = "100px"),
                                            br(),
                                            plotOutput("plotSherdHeight", height = "100px"),
                                            br(),
                                            imageOutput("selectedSherdImage"),
                                            br(),
                                            column(width = 2,
                                                   tableOutput("sherdInfo")
                                            ),br(),
                                            column(width = 2,
                                                   # tableOutput("sherdClusterInfo")
                                                   plotOutput("plotClusterInfo", width = "400px", height = "200px"))

                                     )
                                   ),
                                   fluidRow(
                                     column(width = 12,
                                            actionButton("cluster_1", "4 Clusters", icon("glasses"), style = "color: #000000; background-color: #ffffff; border-color: #000000"),
                                            actionButton("cluster_2", "6 Clusters", icon("glasses"), style = "color: #000000; background-color: #ffffff; border-color: #000000")
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 12,
                                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("FISplot_by_cluster_1", height = "200px"), plotOutput("FISplot_by_cluster_2", height = "200px")),
                                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("FISplot_by_cluster_3", height = "200px"), plotOutput("FISplot_by_cluster_4.ext", height = "200px")),
                                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("FISplot_by_cluster_4.int", height = "200px"), plotOutput("FISplot_by_cluster_5", height = "200px")),
                                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("FISplot_by_cluster_6", height = "200px"), plotOutput("FISplot_by_cluster_7", height = "200px"))
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 8,
                                            plotlyOutput("beeplot", height = "1200px"),
                                            verbatimTextOutput("beeselected")
                                     ),
                                     column(width = 4,
                                            tableOutput('tableContexts'))
                                   ),
                                   br(),br(),
                                   fluidRow(
                                     column(12, align = "center",
                                            # br(),
                                            DTOutput('FIStable')
                                     ), br(),
                                     column(3, align = "left",
                                            imageOutput("imageColorLegend") # , width = "100%"
                                     )
                                   ),
                                   br(),br()
                          ),
                          tabPanel("Memo",
                                   fluidRow(
                                     h2("Fuzzy type definition and description", align = "center"),
                                     br(),br(),br(),br(),br(),
                                     h4("This works examines whether there are morphologically different groups within the SRSW type 1B150.", align = "center"),
                                     br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                     h5("This work is part of the FWO G088319N Back To Basics, With A Twist.",
                                        align = "center", style = "font-family: 'Times'; color: #808080; font-style: italic;"),
                                     h5("Implementation: Danai Kafetzaki. Date: 27 May 2020 - 19 October 2021",
                                        align = "center", style = "font-family: 'Times'; color: #808080; font-style: italic;"),
                                     br(),br(),
                                     fluidRow(
                                       column(12,
                                              column(6, align = "right",
                                                     imageOutput("imageLogo1" , width = "100%", height = "100%")),
                                              column(6,  align = "left",
                                                     imageOutput("imageLogo2" , width = "100%", height = "100%"))
                                       ),
                                       br(),br()
                                     )
                                   )
                          ),
                          tabPanel("Distance Matrix",
                                   fluidRow(
                                     column(width = 8, align = "center",
                                            highchartOutput('distM_plot', height = "1200px", width = "1200px")
                                     ),
                                     column(width = 2, align = "center",
                                            plotOutput("plotBoth", height = "200px")
                                     )
                                   )
                          )
                        )
)
)

