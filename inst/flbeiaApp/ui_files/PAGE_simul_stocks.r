

tabsetPanel(type = "tabs",
            
            #------------------------------------
            # TIME SERIES window
            #------------------------------------
            tabPanel( 
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                      bsCollapse(id = "collapse", #open = "Stock and Indicator",
                      bsCollapsePanel("Stock and Indicator",
                  sliderInput("rangeS", label=h4("Years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
                  selectizeInput("stockS", label=h4("Stock"), levels(as.factor(bio$stock)), selected=unique(bio$stock)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorS", label=h4("Indicators"), levels(as.factor(bio$indicator)),selected=unique(bio$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioS", label=h4("Scenarios"), levels(as.factor(bio$scenario)), selected=unique(bio$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("iterS", label=h4("Iteractions"), levels(as.factor(bio.iter$iter)), selected=NULL, multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  checkboxInput("refpointS", h5("Reference points"), FALSE)
                  #hr(),
                      ),
                  bsCollapsePanel("Graphs",
                  checkboxInput("fitCIS", "Confident interval", FALSE),
                  checkboxInput("fitS", "Free scales", FALSE)
                  ),
                  bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmS', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWS', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHS', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScS', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeS", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "down", label = "Download the plot")
                  ))),
                
                column(9,
                  plotOutput("plotS", height = "600px", width = "900px")
                ))),
            
            
            #------------------------------------
            #  KOBE PLOT
            #------------------------------------
            tabPanel(
              title = "Kobe plot",
              fluidRow(
                br(),
                column(3,
                       bsCollapse(id = "collapse", #open = "Stock and Indicator",
                       bsCollapsePanel("Stock and Scenarios",
                  sliderInput("rangeK", label=h4("Years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
                  selectizeInput("stockK", label=h4("Stock"), unique(RefPts$stock),  selected=unique(RefPts$stock)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioK", label=h4("Scenarios"), unique(as.factor(bio$scenario)), selected=unique(RefPts$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                       ),
                  bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmSK', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWSK', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHSK', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScSK', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeSK", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downSK", label = "Download the plot")
                  ))),
                
              column(9,
                  plotOutput("plotK", height = "600px", width = "900px")
              
              ))),
            
            
            #------------------------------------
            #  BIOLOGICAL RISK
            #------------------------------------
            tabPanel(
              title = "Biological risk", 
              fluidRow(
                br(),
                column(3,
                       bsCollapse(id = "collapse", #open = "Stock and Indicator",
                       bsCollapsePanel("Stock and Reference points",
                  sliderInput("rangeR", label=h4("Years"), min(as.numeric(risk$year)), max(as.numeric(risk$year)), value=range(as.numeric(risk$year)),step = 1),
                  selectizeInput("stockR", label=h4("Stock"), choices= unique((RefPts$stock)), selected=unique((RefPts$stock))[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioR", label=h4("Scenarios"), levels(as.factor(risk$scenario)), selected= unique((risk$scenario))[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("brpR", label=h4("Biological reference point"), choices=c("pBlim", "pBpa"),selected="pBlim", multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                       ),
                  bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmSR', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWSR', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileHSR', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                  numericInput('fileScSR', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                  selectInput(inputId = "fileTypeSR", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downSR", label = "Download the plot")
                  ))),
                
                column(9,
                  plotOutput("plotR", height = "600px", width = "900px")
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
                   bsCollapsePanel("Stock and Indicator",
                     radioButtons("yearSP", label=h4("Year"),  c("Year" = "radio1","Years ratio" = "radio2")),
                     
                     # Only show this panel if the radio1 is selected
                     conditionalPanel(
                       condition = "input.yearSP == 'radio1'",
                       div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("yearSP0", "Year",levels(as.factor(bio$year)), selected = 2015, multiple = FALSE))
                       ),
                       
                       # Only show this panel if radio2 is selected
                       conditionalPanel(
                         condition = "input.yearSP == 'radio2'",
                         div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("yearSP1", "Year 1",levels(as.factor(bio$year)), selected=2015, multiple = FALSE)),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("yearSP2", "Year 2", levels(as.factor(bio$year)), selected=2013, multiple = FALSE))
                         ),
                     
                     selectizeInput("stockSP", label=h4("Stock"), levels(as.factor(bio$stock)), selected=unique(bio$stock)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                     selectizeInput("indicatorSP", label=h4("Indicators"), levels(as.factor(bio$indicator)),selected=unique(bio$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                     selectizeInput("scenarioSP", label=h4("Scenarios"), levels(as.factor(bio$scenario)), selected=unique(bio$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                     #hr(),
                     ),
                    bsCollapsePanel("Download",
                     # Options for file downloading
                      textInput('filenmSP', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                      numericInput('fileWSP', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
                      numericInput('fileHSP', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
                      numericInput('fileScSP', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
                      selectInput(inputId = "fileTypeSP", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                      downloadButton(outputId = "downSP", label = "Download the plot")
                    ))),
                
                column(9,
                    plotOutput("plotSP", height = "600px", width = "900px")
                )))
            
)#end of tabsetPanel

