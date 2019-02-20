
tabsetPanel(type = "tabs",
            
            #--------------------------------
            # TIME SERIES
            #--------------------------------
            tabPanel(
              title = "Time series",
              sidebarLayout(
                sidebarPanel(
                  sliderInput("rangeF",        label=h4("Years"),      min(as.numeric(flt$year)), max(as.numeric(flt$year)), value=range(as.numeric(flt$year)),step = 1),
                  selectizeInput("fleetF",     label=h4("Fleet"),      unique(flt$fleet),         selected= unique((flt$fleet))[1],       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioF",  label=h4("Scenarios"),  unique(flt$scenario),      selected= unique((flt$scenario))[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("indicatorF", label=h4("Indicators"), unique(flt$indicator),     selected= "effort",                  multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  hr(),
                  checkboxInput("fitCIF", h5("Confident intervals"), FALSE),
                  checkboxInput("fitF", h5("Free scales"), FALSE),
                  
                  # Options for file downloading
                  textInput('filenmF', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWF', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHF', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScF', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeF", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downF", label = "Download the plot")
                  
              ),
            mainPanel(
              plotOutput("plotF")
            )
            )),
            
            #--------------------------------
            # Net present value
            #--------------------------------
            tabPanel(
              title = "Net present value",
              sidebarLayout(
                sidebarPanel(
                  selectizeInput("fleetN",    label=h4("Fleet"),    unique(npv2$fleet),    selected = unique(npv2$fleet),       multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioN", label=h4("Scenario"), unique(npv2$scenario), selected = unique(npv2$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  
                  # Options for file downloading
                  textInput('filenmFN', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWFN', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHFN', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScFN', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeFN", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downFN", label = "Download the plot"),
                  hr()
                ),
                mainPanel(
                  plotOutput("plotFN")
                )
              )),
            
            #--------------------------------
            # RISK
            #--------------------------------
            tabPanel(
              title = "Economic risk", #change dataframe
              sidebarLayout(
                sidebarPanel(
                  sliderInput("rangeE",       label = h4("Years"),    min(as.numeric(risk$year)), max(as.numeric(risk$year)), value=range(as.numeric(risk$year)),step = 1),
                  selectizeInput("fleetE",    label = h4("Fleet"),    unique(risk[risk$indicator=="pPrflim",]$unit), selected=unique(flt$fleet)[1],    multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  selectizeInput("scenarioE", label = h4("Scenario"), unique(flt$scenario),                          selected=unique(flt$scenario)[1], multiple=T, options=list(plugins=list("remove_button", "drag_drop"))),
                  
                  # Options for file downloading
                  textInput('filenmFR', h5("File Name"), value = "", width = NULL, placeholder = NULL),
                  numericInput('fileWFR', h5("Width (cm)"), value = 14, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileHFR', h5("Height (cm)"), value = 10, min = 0, max = 25, step = 1, width = 75),
                  numericInput('fileScFR', h5("Scale in ggsave"), value = 1.5, min = 0, max = 3, step = 0.1, width = 75),
                  selectInput(inputId = "fileTypeFR", label = "Select the file type", selected= "png", choices = c("eps", "ps", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"), multiple = FALSE),
                  downloadButton(outputId = "downFR", label = "Download the plot"),
                  hr()
                ),
                mainPanel(
                  plotOutput("plotFR")

                )
              )
            )
)
