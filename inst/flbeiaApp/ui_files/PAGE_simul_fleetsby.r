


tabsetPanel(type = "tabs",
            tabPanel(
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Select Variables",
                  sliderInput("rangeFby",        label=h4("Years"),     min(as.numeric(fltStk$year)), max(as.numeric(fltStk$year)), value=range(as.numeric(fltStk$year)),step = 1),
                  selectizeInput("fleetFby",     label=h4("Fleet"),     unique(fltStk$fleet),     selected=unique(fltStk$fleet)[1],    multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("stockFby",     label=h4("Stock"),     choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorFby", label=h4("Indicator"), unique(fltStk$indicator), selected= unique(fltStk$indicator)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioFby",  label=h4("Scenarios"), unique(fltStk$scenario),  selected=unique(fltStk$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
    			        ),
    			        shinyBS::bsCollapsePanel("Graphical options",
    			                                 checkboxInput("dotLineFby", "Dot & Lines", FALSE),
    			                                 numericInput('lwdFby', h5("Line width"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
    			                                 numericInput('dszFby', h5("Dot size"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
    			                                 checkboxInput("fitCIFby", "Confident interval", FALSE),
    			                                 checkboxInput("fitFby", "Free scales", FALSE)
    			        ),
    			        shinyBS::bsCollapsePanel("Download",
    			        # Options for file downloading
    			        textInput('filenmFby', h5("File Name"), value = "", width = NULL, placeholder = NULL),
    			        numericInput('fileWFby', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
    			        numericInput('fileHFby', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
    			        numericInput('fileScFby', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
    			        selectInput(inputId = "fileTypeFby", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
    			        downloadButton(outputId = "downFby", label = "Download the plot")
    			        ))),
                column(9,
                  plotOutput("plotFby", height = "600px", width = "900px")
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
                sliderInput("rangebyA",        label=h4("Years"),      min(fltStk$year), max(fltStk$year), value=range(fltStk$year),step = 1),
                selectizeInput("fleetbyA",     label=h4("Fleet"),      unique(fltStk$fleet), selected=unique(fltStk$fleet)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("stockbyA",     label=h4("Stock"),    choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("indicatorbyA", label=h4("Indicators"), choices =  intersect(unique(fltStk$indicator), c('catch', 'landings', 'discards', 'quota')),
                                                                       selected = intersect(unique(fltStk$indicator), c('catch', 'landings', 'discards', 'quota'))[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                selectizeInput("scenariobyA",  label=h4("Scenarios"),  unique(fltStk$scenario), selected=unique(fltStk$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                checkboxInput("percbyA", h5("Percentage"), FALSE),
                checkboxInput("fitbyA", "Free scales", FALSE)                                          #hr(),
                ),
                shinyBS::bsCollapsePanel("Download",  # Options for file downloading
                textInput('filenmbyA', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                numericInput('fileWbyA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                numericInput('fileHbyA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                numericInput('fileScbyA', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                selectInput(inputId = "fileTypebyA", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                downloadButton(outputId = "downbyA", label = "Download the plot")
    ))),
    column(9, plotOutput("plotFSbyA", height = "600px", width = "900px")
          )))
)
# end of the tabsetPanel