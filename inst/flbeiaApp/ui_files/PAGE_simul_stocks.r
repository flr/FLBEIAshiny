

tabsetPanel(type = "tabs",
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # TIME SERIES window
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel( 
              title = "Time series",
              fluidRow(
                br(),
                  # column(12, 
                  #    includeHTML("data/TimeSeriesPlot.txt")), 
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Select variables",
                  sliderInput("rangeS", label=h4("Years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
                  selectizeInput("stockS", label=h4("Stock"), levels(as.factor(bio$stock)), selected=unique(bio$stock)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorS", label=h4("Indicators"), levels(as.factor(bio$indicator)),selected=unique(bio$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioS", label=h4("Scenarios"), levels(as.factor(bio$scenario)), selected=unique(bio$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("iterS", label=h4("Iterations"), levels(as.factor(bioIt$iter)), selected=NULL, multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  checkboxInput("refpointS", h5("Reference points"), FALSE)
                  #hr(),
                      ),
                  shinyBS::bsCollapsePanel("Graphical options",
                  checkboxInput("dotLineS", "Dot & Lines", FALSE),
                  numericInput('lwdS', h5("Line width"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
                  numericInput('dszS', h5("Dot size"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
                  checkboxInput("fitCIS", "Confident interval", FALSE),
                  checkboxInput("fitS", "Free scales", FALSE)
                  ),
                  shinyBS::bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmS', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWS', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHS', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScS', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeS", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "down", label = "Download the plot")
                  ))),
                
                column(9,
                  uiOutput("plotS", inline =TRUE) #height = "600px", width = "900px"
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
                                                        sliderInput("rangeSA", label=h4("Years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
                                                        selectizeInput("stockSA", label=h4("Stock"), levels(as.factor(bio$stock)), selected=levels(as.factor(bio$stock)),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                                                        selectizeInput("indicatorSA", label=h4("Indicators"), c('ssb', 'biomass', 'catch', 'landings', 'discards'),selected='catch',multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                                                        selectizeInput("scenarioSA", label=h4("Scenarios"), levels(as.factor(bio$scenario)), selected=unique(bio$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                                                        checkboxInput("percSA", h5("Percentage"), FALSE),
                                                        checkboxInput("fitSA", "Free scales", FALSE)
                                                                              #hr(),
                                           ),
                       shinyBS::bsCollapsePanel("Download",  # Options for file downloading
                                                      textInput('filenmSA', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                                                      numericInput('fileWSA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                                                      numericInput('fileHSA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                                                      numericInput('fileScSA', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                                                      selectInput(inputId = "fileTypeSA", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                                                      downloadButton(outputId = "downSA", label = "Download the plot")
                                           ))),

                column(9, uiOutput("plotSA", inline =TRUE) 
                ))),
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #  KOBE PLOT
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel(
              title = "Kobe plot",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Select variables",
                  sliderInput("rangeK", label=h4("Years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
                  selectizeInput("stockK", label=h4("Stock"), unique(RefPts$stock),  selected=unique(RefPts$stock)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioK", label=h4("Scenarios"), unique(as.factor(RefPts$scenario)), selected=unique(RefPts$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                       ),
                  shinyBS::bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmSK', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWSK', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHSK', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScSK', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeSK", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downSK", label = "Download the plot")
                  ))),
                column(9, uiOutput("plotK", inline =TRUE) 
              ))),
            

            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # SPIDER PLOTS
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            tabPanel( 
              title = "Spider",
              fluidRow(
                br(),
                column(3,
                   shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                   shinyBS::bsCollapsePanel("Select variables",
                     radioButtons("baseSP", label=h4("Base"),  c("Year" = "radio1","Scenario" = "radio2")),
                     
                   #  Only show this panel if the radio1 is selected
                   conditionalPanel(
                     condition = "input.baseSP == 'radio1'",
                     div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseSP1", "Base Year", unique(bio$year), selected=min(bio$year), multiple = FALSE)),
                     div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseSP2", "Year",     unique(bio$year), selected=max(bio$year), multiple = FALSE))
                   ),

                  # Only show this panel if radio2 is selected
                  conditionalPanel(
                     condition = "input.baseSP == 'radio2'",
                     div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseSP3", "Base Scenario", unique(bio$scenario), selected= unique(bio$scenario)[1], multiple = FALSE)),
                     div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseSP4", "Year",          unique(bio$year),     selected= max(bio$year), multiple = FALSE))
                   ),
                     radioButtons("GrpPanSP", label=h4("Group"),  c("Stock" = "stock","Scenario" = "scenario")),
                     selectizeInput("stockSP", label=h4("Stock"),          unique(bio$stock),    selected=unique(bio$stock),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                     selectizeInput("indicatorSP", label=h4("Indicators"), unique(bio$indicator),selected=unique(bio$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                     selectizeInput("scenarioSP", label=h4("Scenarios"),   unique(bio$scenario), selected=unique(bio$scenario), multiple=T, options=list(plugins=list("remove_button", "drag_drop")))              #hr(),
                     ),
                   shinyBS::bsCollapsePanel("Download",
                     # Options for file downloading
                      textInput('filenmSP', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                      numericInput('fileWSP', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                      numericInput('fileHSP', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                      numericInput('fileScSP', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                      selectInput(inputId = "fileTypeSP", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                      downloadButton(outputId = "downSP", label = "Download the plot")
                    ))),
                
                       column(9, uiOutput("plotSP", inline =TRUE) 
                )))
            
)#end of tabsetPanel

