tabPanel("FLBEIA",
           br(),
           includeHTML ("data/DescriptionFlbeia.txt"), 
           #includeHTML ("data/DescriptionCS.txt")#,  
           #textAreaInput("caption", "", rows = 5, "Describe your case study here...", width = "1000px"),
           br(),
           tableOutput("cs")#,
           # br(),
           # br(),
           # actionButton("submit", "Add")
           )

