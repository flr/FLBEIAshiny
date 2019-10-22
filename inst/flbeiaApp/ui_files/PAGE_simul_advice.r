

tabsetPanel(type = "tabs",
            tabPanel( 
              title = "Time series",
              fluidRow(
                br(),
                column(3,
                       bsCollapse(id = "collapse", #open = "Stock and Indicator",
                       bsCollapsePanel("Stock and Indicator",
                  sliderInput("rangeA", label=h4("Years"), min(as.numeric(adv$year)), max(as.numeric(adv$year)), value=range(as.numeric(adv$year)),step = 1),
                  selectizeInput("stockA", label=h4("Stock"), levels(as.factor(adv$stock)), selected=unique(adv$stock)[1],multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorA", label=h4("Indicators"), levels(as.factor(adv$indicator)),selected="tac",multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioA", label=h4("Scenarios"), levels(as.factor(adv$scenario)), selected=unique(adv$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop")))
                  ),
                  bsCollapsePanel("Graphs",
                  checkboxInput("fitCIA", h5("Confident intervals"), FALSE),
                  checkboxInput("fitA", h5("Free scales"), FALSE)
                  ),
                  bsCollapsePanel("Download",
                  # Options for file downloading
                  textInput('filenmA', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWA', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHA', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScA', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeA", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downA", label = "Download the plot")
                  ))),
                column(9,
                  plotOutput("plotA", height = "600px", width = "900px")
                )))
)# end of the tabsetPanel