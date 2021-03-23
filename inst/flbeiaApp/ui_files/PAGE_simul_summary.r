

  fluidRow(
    br(),
    column(12, includeHTML("data/SummaryPlot.txt")),
    column(3,
      shinyBS::bsCollapse(id = "collapse", #open = "Stock and Indicator",
      shinyBS::bsCollapsePanel("Select variables",
      selectInput("yearP", label=h4("Reference year"), unique(as.numeric(bio$year)), selected=min(as.numeric(bio$year))),
      sliderInput("rangeP", label=h4("Projection years"), min(bio$year), max(bio$year), value=range(unique(bio$year)[(length(unique(bio$year))-2):length(unique(bio$year))]),step = 1),
   #   sliderInput("rangeP", label=h4("Projection years"), min(as.numeric(bio$year)), max(as.numeric(bio$year)), value=(max(as.numeric(bio$year))-2):max(as.numeric(bio$year)),step = 1),
      selectizeInput("fleetP", label=h4("Fleets"),       unique(flt$fleet), selected = unique(flt$fleet),multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
      selectizeInput("stockP",     label=h4("Stocks"),   unique(bio$stock), selected = unique(bio$stock), multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
      selectizeInput("scenarioP", label=h4("Scenarios"), unique(bio$scenario), selected=unique(bio$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
        numericInput('nColP', h5("Number of columns in facets"), value = 1, min = 1, max = 3, step = 1, width = 200)
      ),
      shinyBS::bsCollapsePanel("Download",
          #  Options for file downloading
          fluidRow(column(8,textInput('filenmP', h5("File Name"), value = "", width = NULL, placeholder = NULL)),
                   column(4,selectInput(inputId = "fileTypeP", label = "File type", selected= "png", 
                                   choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE))),
          fluidRow(column(3,numericInput('fileWP', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100)),
                   column(3,numericInput('fileHP', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100)),
                   column(3,numericInput('fileScP', h5("Scale"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100))),
          downloadButton(outputId = "downP", label = "Download  plot")))),
    column(9, uiOutput("plotP", inline =TRUE) 
      )
   )

