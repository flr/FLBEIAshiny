tabPanel("FLBEIA",
           br(),
           includeHTML ("data/DescriptionFlbeia.txt"), 
           textAreaInput("caption", "", rows = 5, "Describe your case study here...", width = "1000px"),
           textOutput("value"),
           br(),
           br(),
           actionButton("submit", "Add")
           )

