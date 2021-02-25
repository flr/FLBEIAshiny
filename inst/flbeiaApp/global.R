# # ## *********************************


#load(file.path(getwd(), 'inst/flbeiaApp/deploy.RData'))
load('deploy.RData')
if(deploy == TRUE){
  load("App.RData")
    library(ggplot2)
  library(dplyr)
    library(schoolmath)
   library(kobe)
 }
# radar coordinate system for spider plots::

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}