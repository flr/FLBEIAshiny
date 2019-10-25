
tabsetPanel(type = "tabs",
            
            #--------------------------------
            # TIME SERIES
            #--------------------------------
            tabPanel(
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                       bsCollapse(id = "collapse", #open = "Stock and Indicator",
                       bsCollapsePanel("Fleet and Indicator",
                  sliderInput("rangeF",        label=h4("Years"),      min(as.numeric(flt$year)), max(as.numeric(flt$year)), value=range(as.numeric(flt$year)),step = 1),
                  selectizeInput("fleetF",     label=h4("Fleet"),      unique(flt$fleet),         selected= unique((flt$fleet))[1],       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioF",  label=h4("Scenarios"),  unique(flt$scenario),      selected= unique((flt$scenario))[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorF", label=h4("Indicators"), unique(flt$indicator),     selected= "effort",                  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("iterF", label=h4("Iteractions"), levels(flt.iter$iter), selected=NULL, multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                       ),
                  bsCollapsePanel("Graphs",
                  checkboxInput("fitCIF", h5("Confident intervals"), FALSE),
                  checkboxInput("fitF", h5("Free scales"), FALSE)
                  ),
                  bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmF', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWF', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHF', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScF', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeF", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downF", label = "Download the plot")
                  ))),
            
                column(9,
                plotOutput("plotF", height = "600px", width = "900px")
                ))),
            
            #--------------------------------
            # Net present value
            #--------------------------------
            tabPanel(
              title = "Net present value",
              fluidRow(
                br(),
                column(3,
                       bsCollapse(id = "collapse", #open = "Stock and Indicator",
                       bsCollapsePanel("Fleet and Scenario",
                  selectizeInput("fleetN",    label=h4("Fleet"),    unique(npv$fleet),    selected = unique(npv$fleet)[1],       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioN", label=h4("Scenario"), unique(npv$scenario), selected = unique(npv$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                  ),
                  bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmFN', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWFN', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHFN', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScFN', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeFN", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downFN", label = "Download the plot")
                  ))),
           
                column(9,
                  plotOutput("plotFN", height = "600px", width = "900px")
                ))),
            
            #--------------------------------
            # RISK
            #--------------------------------
            tabPanel(
              title = "Economic risk", #change dataframe
              fluidRow(
                br(),
                column(3,
                       bsCollapse(id = "collapse", #open = "Stock and Indicator",
                       bsCollapsePanel("Fleet and Economic indicator",
                  sliderInput("rangeE",       label = h4("Years"),    min(as.numeric(risk$year)), max(as.numeric(risk$year)), value=range(as.numeric(risk$year)),step = 1),
                  selectizeInput("fleetE",    label = h4("Fleet"),    unique(risk[risk$indicator=="pPrflim",]$unit), selected=unique(flt$fleet)[1],    multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioE", label = h4("Scenario"), unique(flt$scenario), selected=unique(flt$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                  ),
                  bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmFR', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWFR', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHFR', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScFR', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeFR", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downFR", label = "Download the plot")
                  ))),
  
                column(9,
                  plotOutput("plotFR", height = "600px", width = "900px")
                ))), 
            #------------------------------------
            # SPIDER PLOTS
            #------------------------------------
            tabPanel( 
              title = "Spider",
              fluidRow(
                br(),
                column(3,
                  bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  bsCollapsePanel("Fleet and Indicator",
                    radioButtons("yearFP", label=h4("Year"),  c("Year" = "radioF1","Years ratio" = "radioF2")),
                       
                     # Only show this panel if the radio1 is selected
                     conditionalPanel(
                        condition = "input.yearFP == 'radioF1'",
                           div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("yearFP0", "Year",levels(as.factor(flt$year)), selected = 2015, multiple = FALSE))
                            ),
                                                  
                     # Only show this panel if radio2 is selected
                      conditionalPanel(
                        condition = "input.yearFP == 'radioF2'",
                            div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("yearFP1", "Year 1",levels(as.factor(flt$year)), selected=2015, multiple = FALSE)),
                            div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("yearFP2", "Year 2", levels(as.factor(flt$year)), selected=2013, multiple = FALSE))
                             ),
                                                  
                    selectizeInput("fleetFP",   label=h4("Fleet"),      unique(flt$fleet),         selected= unique((flt$fleet))[1],       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                    selectizeInput("scenarioFP", label=h4("Scenarios"),  unique(flt$scenario),      selected= unique((flt$scenario))[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                    selectizeInput("indicatorFP", label=h4("Indicators"), unique(flt$indicator),     selected= "effort",                  multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                               ),
                    bsCollapsePanel("Download",
                             # Options for file downloading
                     textInput('filenmFP', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                     numericInput('fileWFP', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                     numericInput('fileHFP', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                     numericInput('fileScFP', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                     selectInput(inputId = "fileTypeFP", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                     downloadButton(outputId = "downFP", label = "Download the plot")
                     ))),
                
                column(9,
                       plotOutput("plotFP", height = "600px", width = "900px")
                )))
)#end of the tabsetPanel
