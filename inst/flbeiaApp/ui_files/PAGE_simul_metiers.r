

tabsetPanel(type = "tabs",
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #  Time series plot
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel(
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Metier and Indicator",
                  sliderInput("rangeM", label=h4("Years"), min(as.numeric(mt$year)), max(as.numeric(mt$year)), value=range(as.numeric(mt$year)),step = 1),
                  selectizeInput("fleetM", label=h4("Fleet"), unique(mt$fleet), selected=unique(mt$fleet)[1],multiple=F, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("metierM", label=h4("Metier"), unique(mt$metier), selected=unique(mt$metier),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioM", label=h4("Scenarios"), unique(mt$scenario), selected=unique(mt$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorM", label=h4("Indicators"), unique(mt$indicator),selected="effort",multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                  ),
                  shinyBS::bsCollapsePanel("Graphs",
                  checkboxInput("fitCIM", h5("Confident intervals"), FALSE),
                  checkboxInput("fitM", h5("Free scalse"), FALSE),
                  numericInput('nColM', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100)
                  ),
                  shinyBS::bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmM', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWM', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHM', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScM', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeM", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downM", label = "Download the plot")
                  ))),
                column(9,
                  plotOutput("plotMM", height = "600px", width = "900px")
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
                sliderInput("rangeMA",        label=h4("Years"),      min(as.numeric(mt$year)), max(as.numeric(mt$year)), value=range(as.numeric(mt$year)),step = 1),
                selectizeInput("fleetMA",     label=h4("Fleet"),      unique(mt$fleet), selected=unique(mt$fleet)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("metierMA",     label=h4("Metier"),    choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("indicatorMA", label=h4("Indicators"), choices = unique(mt$indicator), selected = unique(mt$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("scenarioMA",  label=h4("Scenarios"),  unique(mt$scenario), selected=unique(mt$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                checkboxInput("percMA", h5("Percentage"), FALSE),
                checkboxInput("fitMA", "Free scales", FALSE),
                numericInput('nColMA', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100)
                                                                    #hr(),
                ),
                shinyBS::bsCollapsePanel("Download",  # Options for file downloading
                textInput('filenmMA', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                numericInput('fileWMA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                numericInput('fileHMA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                numericInput('fileScMA', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                selectInput(inputId = "fileTypeMA", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                downloadButton(outputId = "downMA", label = "Download the plot")
               ))),
                column(9, plotOutput("plotFSMA", height = "600px", width = "900px")
                )))
)