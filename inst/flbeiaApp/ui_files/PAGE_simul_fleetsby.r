


tabsetPanel(type = "tabs",
            tabPanel(
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Select Variables",
                    sliderInput("rangeFby",        label=h4("Years"),     min(as.numeric(fltStk$year)), max(as.numeric(fltStk$year)), value=range(as.numeric(fltStk$year)),step = 1),
                    selectizeInput("fleetFby",     label=h4("Fleets"),     unique(fltStk$fleet),     selected=unique(fltStk$fleet)[1],    multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                    selectizeInput("stockFby",     label=h4("Stocks"),     choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                    selectizeInput("indicatorFby", label=h4("Indicators"), unique(fltStk$indicator), selected= unique(fltStk$indicator)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                    selectizeInput("scenarioFby",  label=h4("Scenarios"), unique(fltStk$scenario),  selected=unique(fltStk$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
    			        ),
    			  shinyBS::bsCollapsePanel("Graphical options",
    			    checkboxInput("dotLineFby", "Dot & Lines", FALSE),
    			    numericInput('lwdFby', h5("Line width"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
    			    numericInput('dszFby', h5("Dot size"), value = 1, min = 0, max = 5, step = 0.1, width = 100),
    			    selectizeInput("rowFby",     label=h4("Rows"),     c('Fleet-Stock', 'Fleet-Indicator', 'Stock-Indicator'),   'Fleet-Stock',    multiple=F, options=list(plugins=list("remove_button", "drag_drop"))),
    			    checkboxInput("fitCIFby", "Confident interval", FALSE),
    			    checkboxInput("fitFby", "Free scales", FALSE),
    			    numericInput('nColFby', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100)),
    			  shinyBS::bsCollapsePanel("Download",
    			    #  Options for file downloading
    			    fluidRow(column(8,textInput('filenmFby', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
    			             column(4,selectInput(inputId = "fileTypeFby", label = "File type", selected= "png", 
    			                               choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
    			    fluidRow(column(3,numericInput('fileWFby', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
    			             column(3,numericInput('fileHFby', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
    			             column(3,numericInput('fileScFby', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
    			     downloadButton(outputId = "downFby", label = "Download the plot")))),
                column(9, uiOutput("plotFby", inline =TRUE))
                )),
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
                checkboxInput("fitbyA", "Free scales", FALSE),
                numericInput('nColbyA', h5("N.Col in facets"), value = 2, min = 1, max = 10, step = 1, width = 100)
                #hr(),
                ),
                shinyBS::bsCollapsePanel("Download",
                    #  Options for file downloading
                    fluidRow(column(8,textInput('filenmbyA', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                             column(4,selectInput(inputId = "fileTypebyA", label = "File type", selected= "png", 
                                         choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
                    fluidRow(column(3,numericInput('fileWbyA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                             column(3,numericInput('fileHbyA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                             column(3,numericInput('fileScbyA', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
                    downloadButton(outputId = "downbyA", label = "Download the plot")))),
                column(9, uiOutput("plotFSbyA", inline =TRUE))
          )),
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # SPIDER PLOTS
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabPanel( 
        title = "Spider",
        fluidRow(
            br(),
            column(12, includeHTML("data/SpiderPlot.txt")), 
            column(3,
                  shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
                  shinyBS::bsCollapsePanel("Select variables",
                  radioButtons("baseFSPby", label=h4("Base"),  c("Year" = "radio1Fby","Indicator" = "radio2Fby")),
                  #  Only show this panel if the radio1 is selected
                  conditionalPanel(
                      condition = "input.baseFSPby == 'radio1Fby'",
                        div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSPby1", "Base Year", unique(fltStk$year), selected=min(fltStk$year), multiple = FALSE)),
                        div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSPby2", "Year",     unique(fltStk$year), selected=max(fltStk$year), multiple = FALSE))
                      ),
                  # Only show this panel if radio2 is selected
                   conditionalPanel(
                      condition = "input.baseFSPby == 'radio2Fby'",
                          div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSPby3", "Base Scenario", unique(fltStk$scenario), selected= unique(fltStk$scenario)[1], multiple = FALSE)),
                          div(style="display: inline-block;vertical-align:top; width: 100px;",selectInput("baseFSPby4", "Year",          unique(fltStk$year),     selected= max(fltStk$year), multiple = FALSE))
                         ),
                  radioButtons("GrpPanFSPby", label=h4("Group"),  c("Fleet" = "fleet","Stock" = "stock","Indicator" = "indicator")),
                  selectizeInput("fleetFSPby", label=h4("Fleets"),          unique(fltStk$fleet),    selected=unique(fltStk$fleet),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("stockFSPby",     label=h4("Stocks"),     choices = '',  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorFSPby", label=h4("Indicators"), unique(fltStk$indicator),selected=unique(fltStk$indicator)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioFSPby", label=h4("Scenarios"),   unique(fltStk$scenario), selected=unique(fltStk$scenario), multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  numericInput('nColFSPby', h5("N.Col in facets"), value = 3, min = 1, max = 3, step = 1, width = 100)#hr(),
                  ),
            shinyBS::bsCollapsePanel("Download",
               #  Options for file downloading
               fluidRow(column(8,textInput('filenmFSPby', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                        column(4,selectInput(inputId = "fileTypeFSPby", label = "File type", selected= "png", 
                                       choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
               # fluidRow(column(3,numericInput('fileWFSPby', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
               #          column(3,numericInput('fileHFSPby', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
               #          column(3,numericInput('fileScFSPby', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
               downloadButton(outputId = "downFSPby", label = "Download the plot")))),
            column(9, uiOutput("plotFSPby", inline =TRUE) 
            )))
    
)
# end of the tabsetPanel