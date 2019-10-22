

  fluidRow(
    br(),
    column(3,
           bsCollapse(id = "collapse", #open = "Stock and Indicator",
           bsCollapsePanel("Projection",
      selectInput("yearP", label=h4("Year of proyection"), levels(as.factor(bio$year)), selected=max(bio$year)),
      sliderInput("rangeP", label=h4("Reference years"), min(bio$year), max(bio$year), value=range(bio$year),step = 1),
      selectizeInput("scenarioP", label=h4("Scenarios"), levels(as.factor(bio$scenario)), selected=unique(bio$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
      ),
      bsCollapsePanel("Download",
      # Options for file downloading
      textInput('filenmP', h5("File Name"), value = "", width = NULL, placeholder = NULL),
      numericInput('fileWP', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 100),
      numericInput('fileHP', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 100),
      numericInput('fileScP', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 100),
      selectInput(inputId = "fileTypeP", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
      downloadButton(outputId = "downP", label = "Download the plot")
      ))),
      column(9,
      plotOutput("plotP", height = "600px", width = "900px")
      )
   )

