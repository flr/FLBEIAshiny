
tabsetPanel(type = "tabs",
            
            #--------------------------------
            # TIME SERIES
            #--------------------------------
            tabPanel(
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Fleet and Indicator",
                  sliderInput("rangeF",        label=h4("Years"),      min(as.numeric(flt$year)), max(as.numeric(flt$year)), value=range(as.numeric(flt$year)),step = 1),
                  selectizeInput("fleetF",     label=h4("Fleet"),      unique(flt$fleet),         selected= unique((flt$fleet))[1],       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioF",  label=h4("Scenarios"),  unique(flt$scenario),      selected= unique((flt$scenario))[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorF", label=h4("Indicators"), unique(flt$indicator),     selected= "effort",                  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("iterF", label=h4("Iterations"), levels(as.factor(fltIt$iter)), selected=NULL, multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                       ),
                  shinyBS::bsCollapsePanel("Graphs",
                  checkboxInput("fitCIF", h5("Confident intervals"), FALSE),
                  checkboxInput("fitF", h5("Free scales"), FALSE),
                  checkboxInput("dotLineF", "Dot & Lines", FALSE),
                  numericInput('lwdF', h5("Line width"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
                  numericInput('dszF', h5("Dot size"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
                  numericInput('nColF', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100)),
                  shinyBS::bsCollapsePanel("Download",
                     #  Options for file downloading
                     fluidRow(column(8,textInput('filenmF', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                              column(4,selectInput(inputId = "fileTypeF", label = "File type", selected= "png", 
                                              choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
                     fluidRow(column(3,numericInput('fileWF', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                              column(3,numericInput('fileHF', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                              column(3,numericInput('fileScF', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
                              downloadButton(outputId = "downF", label = "Download the plot")))),
            
                column(9, uiOutput("plotF", inline =TRUE) 
                ))),
            
            #--------------------------------
            # Net present value
            #--------------------------------
            tabPanel(
              title = "Net present value",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Fleet and Scenario",
                  selectizeInput("fleetN",    label=h4("Fleet"),    unique(npv$fleet),    selected = unique(npv$fleet)[1],       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioN", label=h4("Scenario"), unique(npv$scenario), selected = unique(npv$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  numericInput('nColNPV', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100) ),
                  shinyBS::bsCollapsePanel("Download",
                     #  Options for file downloading
                     fluidRow(column(8,textInput('filenmFN', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                              column(4,selectInput(inputId = "fileTypeFN", label = "File type", selected= "png", 
                                        choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
                     fluidRow(column(3,numericInput('fileWFN', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                              column(3,numericInput('fileHFN', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                              column(3,numericInput('fileScFN', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
                        downloadButton(outputId = "downFN", label = "Download the plot")))),
                column(9,
                  plotOutput("plotFN", height = "600px", width = "900px")
                ))),
      
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # SPIDER PLOTS
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel( 
              title = "Spider",
              fluidRow(
                br(),
                column(12, 
                       includeHTML("data/SpiderPlot.txt")), 
                column(3,
                       shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                       shinyBS::bsCollapsePanel("Select variables",
                       radioButtons("baseFSP", label=h4("Base"),  c("Year" = "radio1F","Scenario" = "radio2F")),
                                                                    
                       #  Only show this panel if the radio1 is selected
                       conditionalPanel(
                       condition = "input.baseFSP == 'radio1F'",
                       div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSP1", "Base Year", unique(flt$year), selected=min(flt$year), multiple = FALSE)),
                       div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSP2", "Year",     unique(flt$year), selected=max(flt$year), multiple = FALSE))
                       ),
                                                                    
                                                                    # Only show this panel if radio2 is selected
                      conditionalPanel(
                        condition = "input.baseFSP == 'radio2F'",
                        div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSP3", "Base Scenario", unique(flt$scenario), selected= unique(flt$scenario)[1], multiple = FALSE)),
                        div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSP4", "Year",          unique(flt$year),     selected= max(flt$year), multiple = FALSE))
                                                                    ),
                        radioButtons("GrpPanFSP", label=h4("Group"),  c("Fleet" = "fleet","Indicator" = "indicator")),
                        selectizeInput("fleetFSP", label=h4("Fleets"),          unique(flt$fleet),    selected=unique(flt$fleet),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                        selectizeInput("indicatorFSP", label=h4("Indicators"), unique(flt$indicator),selected=unique(flt$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                        selectizeInput("scenarioFSP", label=h4("Scenarios"),   unique(flt$scenario), selected=unique(flt$scenario), multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                        numericInput('nColFSP', h5("N.Col in facets"), value = 3, min = 1, max = 3, step = 1, width = 100)#hr(),
                                           ),
                      shinyBS::bsCollapsePanel("Download",
                          #  Options for file downloading
                          fluidRow(column(8,textInput('filenmFSP', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                                   column(4,selectInput(inputId = "fileTypeFSP", label = "File type", selected= "png", 
                                                choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
                          # fluidRow(column(3,numericInput('fileWFSP', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                          #  #        column(3,numericInput('fileHFSP', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                          #          column(3,numericInput('fileScFSP', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
                          downloadButton(outputId = "downFSP", label = "Download the plot"))
                      )),
                column(9, uiOutput("plotFSP", inline =TRUE) 
                )))
)#end of the tabsetPanel

