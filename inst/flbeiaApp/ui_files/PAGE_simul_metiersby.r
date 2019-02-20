
tabsetPanel(type = "tabs",
            tabPanel(
              title = "Time series",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("rangeMby",        label=h4("Years"), min(as.numeric(mtStk$year)), max(as.numeric(mtStk$year)), value=range(as.numeric(mtStk$year)),step = 1),
                  selectizeInput("fleetMby",      label=h4("Fleet"),     unique(mtStk$fleet),     selected=unique(mtStk$fleet)[1],    multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("metierMby",    label=h4("Metier"),    unique(mtStk$metier),     selected=unique(mtStk$metier),      multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("stockMby",      label=h4("Stock"),     unique(mtStk$stock),     selected=unique(mtStk$stock),       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorMby", label=h4("Indicator"), unique(mtStk$indicator), selected="catch",                   multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioMby",  label=h4("Scenario"),  unique(mtStk$scenario),  selected=unique(mtStk$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                 hr(),
                  checkboxInput("fitCIMby", h5("Confident interval"), FALSE),
                  checkboxInput("fitMby", h5("Free scales"), FALSE),
                 
                 # Options for file downloading
                 textInput('filenmMby', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                 numericInput('fileWMby', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                 numericInput('fileHMby', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                 numericInput('fileScMby', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                 selectInput(inputId = "fileTypeMby", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                 downloadButton(outputId = "downMby", label = "Download the plot")
                 
                ),
                mainPanel(
                  plotOutput("plotMby")
                )
              ))
)# end of the tabsetPanel