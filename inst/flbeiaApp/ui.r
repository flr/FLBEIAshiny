

 data<-reshape(as.data.frame(bio), direction = "wide",
               timevar = "indicator", v.names = c("q05","q50","q95"),
               idvar = c("year","stock","scenario"))

 data<-data[, c("stock", "year","scenario", "q50.f", "q50.ssb")]


 data$Bmsy<-NA
 data$Fmsy<-NA
 
 bmsy  <- fmsy <-  numeric(length(unique(reference_points$stock)))
 names(bmsy)  <- names(bmsy) <- unique(reference_points$stock)
 
 for(st in unique(reference_points$stock)){
   bmsy[st] <- reference_points$value[reference_points$stock== st & reference_points$indicator=="Bmsy"] 
   data$Bmsy[data$stock==st] <- bmsy[st] 
   
   fmsy[st] <- reference_points$value[reference_points$stock== st & reference_points$indicator=="Fmsy"] 
   data$Fmsy[data$stock==st] <- fmsy[st] 
  }

 names(data)<- c("unit","year","scenario", "q50.f","q50.ssb","Bmsy","Fmsy")

 data$stock<-data$q50.ssb/data$Bmsy
 data$harvest<-data$q50.f/data$Fmsy

 # save(data, file="data/data.RData")
 #  load("data/data.RData")

 
 head(data)
# Begin shinyUI

ui <- tagList(
  shinyjs::useShinyjs(),
  includeCSS("css/lumen.css"),

  navbarPage(
    title="FLBEIA SHINY",
    fluid=FALSE, # TRUE Layout izateko fluid baina FALSE ikonoa jarri ahal izateko
    inverse=TRUE,

    #### 1.HOME ####
    tabPanel(
      title = "Home",
      value = "home",
      source("ui_files/PAGE_home.r")$value # eliminates printed "TRUE" word
    ),
 
    #### 2.ABOUT ####
    tabPanel(
      title = "About",
      value = "about",
      source("ui_files/PAGE_about.r",local =TRUE)$value
    ),

    #### 4.SIMULATIONS ####
    navbarMenu("Simulations",
               tabPanel("Stocks", 
                        source("ui_files/PAGE_simul_stocks.r",local =TRUE)$value),
               tabPanel("Fleets",
                        source("ui_files/PAGE_simul_fleets.r",local =TRUE)$value),
               tabPanel("Fleets by stock",
                        source("ui_files/PAGE_simul_fleetsby.r",local =TRUE)$value),
               tabPanel("Metiers",
                        source("ui_files/PAGE_simul_metiers.r",local =TRUE)$value),
               tabPanel("Metiers by stock",
                        source("ui_files/PAGE_simul_metiersby.r",local =TRUE)$value),
               tabPanel("Summary",
                        source("ui_files/PAGE_simul_summary.r",local =TRUE)$value),
               tabPanel("Advice",
                        source("ui_files/PAGE_simul_advice.r",local =TRUE)$value)
    ),
    
    # #FLBEIA logo (with link) in the right of the navigation bar. Note that it should be: fluid=FALSE
    # tags$script(HTML("var header=$('.navbar > .container');
    #                   header.append('<div style=\"float:right\"><a href=\"http://flbeia.azti.es\"><img src=\"FLBEIA.png\" height=\"50\"></a></div>');
    #                console.log(header)"))

    tags$style('.navbar-default .navbar-brand {
                color: #000000;
                #font-family: Arial;
                font-size: 30px;
                }'
                
    )
    ) # end navbarPage
) # end tagList
