# # ## *********************************


#load(file.path(getwd(), 'inst/flbeiaApp/deploy.RData'))
# global application

# deploy <- FALSE
# 
# dep <- 'deploy.RData' %in%  dir(file.path(getwd(), 'inst/flbeiaApp/'))
# data/
# if(dep){ load(file.path(getwd(), "inst/flbeiaApp/deploy.RData"))}
#if(dep2){ load(file.path(.libPaths(), "FLBEIAshiny/flbeiaApp/deploy.RData"))}

# Here, the application takes the files 
load("data/deploy.RData")


if(deploy == TRUE){
    load("data/App.RData")
    library(ggplot2)
    library(dplyr)
    library(schoolmath)
    library(kobe)
    library(pals)
    library(ggnewscale)
 }
# radar coordinate system for spider plots::

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}