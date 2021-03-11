
tabsetPanel(type = "tabs",
            tabPanel(
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Metier and stock",
                  sliderInput("rangeMby",        label=h4("Years"), min(as.numeric(mtStk$year)), max(as.numeric(mtStk$year)), value=range(as.numeric(mtStk$year)),step = 1),
                  selectizeInput("fleetMby",      label=h4("Fleet"),     unique(mtStk$fleet),     selected=unique(mtStk$fleet)[1],    multiple=F, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("metierMby",    label=h4("Metier"),    unique(mtStk$metier),     selected=unique(mtStk$metier),      multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("stockMby",      label=h4("Stock"),     unique(mtStk$stock),     selected=unique(mtStk$stock),       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorMby", label=h4("Indicator"), unique(mtStk$indicator), selected="catch",                   multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioMby",  label=h4("Scenario"),  unique(mtStk$scenario),  selected=unique(mtStk$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                 ),
                 shinyBS::bsCollapsePanel("Graphs",
                  checkboxInput("fitCIMby", h5("Confident interval"), FALSE),
                  checkboxInput("fitMby", h5("Free scales"), FALSE),
                  numericInput('nColMby', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100)
                  ),
                 shinyBS::bsCollapsePanel("Download",
                 # Options for file downloading
                 textInput('filenmMby', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                 numericInput('fileWMby', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                 numericInput('fileHMby', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                 numericInput('fileScMby', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                 selectInput(inputId = "fileTypeMby", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                 downloadButton(outputId = "downMby", label = "Download the plot")
                 ))),
                column(9,
                  plotOutput("plotMby", height = "600px", width = "900px")
                ))),
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #  Area plot
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel(
              title = "Area plot",
              fluidRow(
                br(),
                column(3,
                       shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                                shinyBS::bsCollapsePanel("Select variables",
                                sliderInput("rangeMbyA",        label=h4("Years"),      min(mtStk$year), max(mtStk$year), value=range(mtStk$year),step = 1),
                                selectizeInput("fleetMbyA",     label=h4("Fleet"),      unique(mtStk$fleet), selected=unique(mtStk$fleet)[1],multiple=F, options=list(plugins=list("remove_button", "drag_drop"))),
                                selectizeInput("metierMbyA",    label=h4("Metiers"),    choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                                selectizeInput("stockMbyA",     label=h4("Stocks"),     choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                                selectizeInput("indicatorMbyA", label=h4("Indicators"), choices =  unique(mtStk$indicator), selected = 'catch',multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                                selectizeInput("scenarioMbyA",  label=h4("Scenarios"),  unique(mtStk$scenario), selected=unique(mtStk$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                                checkboxInput("percMbyA", h5("Percentage"), FALSE),
                                checkboxInput("fitMbyA", "Free scales", FALSE),
                                numericInput('nColMbyA', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100)
                                                                    #hr(),
                                           ),
                                           shinyBS::bsCollapsePanel("Download",  # Options for file downloading
                                                                    textInput('filenmMbyA', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                                                                    numericInput('fileWMbyA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                                                                    numericInput('fileHMbyA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                                                                    numericInput('fileScMbyA', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                                                                    selectInput(inputId = "fileTypeMbyA", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                                                                    downloadButton(outputId = "downMbyA", label = "Download the plot")
                                           ))),
                column(9, plotOutput("plotFSMbyA", height = "600px", width = "900px")
                )))
)# end of the tabsetPanel