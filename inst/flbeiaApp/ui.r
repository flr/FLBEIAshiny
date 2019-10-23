
## Reference points for kobe plots::
names(RefPts)<- c("stock", "scenario", "refpoint","value")
RefPts$indicator <- NA
RefPts$indicator[RefPts$refpoint =="Bmsy"] <-"ssb"
RefPts$indicator[RefPts$refpoint =="Fmsy"] <-"f"


## radar coordinate system for spider plots::
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

###  rescale all the coordinates within 0 and 1 and 
###   melt the dataset in order to plot it easily with ggplot.

library(dplyr)
library(scales)

bio.scaled <- bio %>% group_by(stock, scenario) %>% 
  mutate(value2 = rescale(q50))
bio.scaled <- as.data.frame(bio.scaled)

 
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
