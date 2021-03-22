

tabsetPanel(type = "tabs",
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #  Time series plot
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel(
              title = "Time series",
              br(),
              column(12, includeHTML("data/TimeSeriesPlot.txt")),
              fluidRow(
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Select variables",
                  sliderInput("rangeM", label=h4("Years"), min(as.numeric(mt$year)), max(as.numeric(mt$year)), value=range(as.numeric(mt$year)),step = 1),
                  selectizeInput("fleetM", label=h4("Fleet"), unique(mt$fleet), selected=unique(mt$fleet)[1],multiple=F, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("metierM", label=h4("Metier"), unique(mt$metier), selected=unique(mt$metier),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioM", label=h4("Scenarios"), unique(mt$scenario), selected=unique(mt$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorM", label=h4("Indicators"), unique(mt$indicator),selected="effort",multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                  ),
                  shinyBS::bsCollapsePanel("Graphical options",
                                           checkboxInput("fitCIM", "Confident interval", FALSE),
                                           fluidRow(column(3,checkboxInput("fitM", "Free scales", FALSE)),
                                                    column(3,numericInput('nColM', h5("Number of columns in facets"), value = 2, min = 1, max = 10, step = 1, width = 200))),
                                           fluidRow(column(3,checkboxInput("dotLineM", "Dot & Lines", FALSE)),
                                                    column(3,numericInput('lwdM', h5("Line width"), value = 1, min = 0, max = 5, step = 0.1, width = 100)),
                                                    column(3,numericInput('dszM', h5("Dot size"), value = 3, min = 0, max = 5, step = 0.1, width = 100)))
                  ), 
                  shinyBS::bsCollapsePanel("Download",
                  # Options for file downloading
                  fluidRow(column(8,textInput('filenmM', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                           column(4,selectInput(inputId = "fileTypeM", label = "File type", selected= "png", 
                                                choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
                  fluidRow(column(3,numericInput('fileWM', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                           column(3,numericInput('fileHM', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                           column(3,numericInput('fileScM', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
                  downloadButton(outputId = "downM", label = "Download plot")))),
                  column(9, uiOutput("plotMM", inline =TRUE))
                )),
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #  Area plot
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel(
              title = "Area plot",
              br(),
              column(12, includeHTML("data/AreaPlot.txt")),
              fluidRow(
                br(),
                column(3,
                shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                shinyBS::bsCollapsePanel("Select variables",
                sliderInput("rangeMA",        label=h4("Years"),      min(as.numeric(mt$year)), max(as.numeric(mt$year)), value=range(as.numeric(mt$year)),step = 1),
                selectizeInput("fleetMA",     label=h4("Fleet"),      unique(mt$fleet), selected=unique(mt$fleet)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("metierMA",     label=h4("Metier"),    choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("indicatorMA", label=h4("Indicators"), choices = unique(mt$indicator), selected = unique(mt$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("scenarioMA",  label=h4("Scenarios"),  unique(mt$scenario), selected=unique(mt$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                checkboxInput("percMA", h5("Percentage"), FALSE),
                fluidRow(column(3,checkboxInput("fitMA", "Free scales", FALSE)),
                         column(3,numericInput('nColMA', h5("Number of columns in facets"), value = 2, min = 1, max = 10, step = 1, width = 200)))
                                                                    #hr(),
                ),
                shinyBS::bsCollapsePanel("Download",
                  #  Options for file downloading
                    fluidRow(column(8,textInput('filenmMA', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                    column(4,selectInput(inputId = "fileTypeMA", label = "File type", selected= "png", 
                    choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
                    fluidRow(column(3,numericInput('fileWMA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                    column(3,numericInput('fileHMA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                    column(3,numericInput('fileScMA', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
                    downloadButton(outputId = "downMA", label = "Download  plot")))),
                column(9, uiOutput("plotFSMA", inline =TRUE))
                ))
)