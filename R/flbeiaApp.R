#-------------------------------------------------------------------------------
#
#' Launch FLBEIA-Shiny application
#' 
#' FLBEIA Shiny application is an interactive interface to analyze the biological, economic and social indicators obtained through FLBEIA simulation model. It provides lots of graphics at scenario, stock, fleet and metier level to facilitate the analysis of the results and the comparison among scenarios.
#'
#' @param flbeiaObjs A named list with a set of FLBEIA outputs, each element of the list corresponding with one scenario. The names of the list will be used to name the scenarios.
#' @param RefPts A data frame with columns, 'stock', 'scenario', 'indicator', and 'value', with the values of 'Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa' and 'Flim' per stock and scenario. If the value for certain stock and/or scenario is not available, them NA should be used. If the data.frame is not available in the function call, then it is created internally with NA values for all the cases.   
#' @param bio The output of bioSumQ function (in long format).
#' @param bioIt The output of bioSum function (in long format). This argument should be defined only if aiming to include the results of individual iterations in the figures.
#' @param flt The output of fltSumQ function (in long format).
#' @param fltIt The output of bioSum function (in long format). This argument should be defined only if aiming to include the results of individual iterations in the figures.
#' @param fltStk The output of fltStkSumQ function (in long format).
#' @param mt The output of mtSumQ function (in long format).
#' @param mtStk The output of mtStkSumQ function (in long format).
#' @param adv The output of advSumQ function (in long format).
#' @param years The years we want to be shown in the app.
#' @param proj.yr The year in which the projection starts.
#' @param calculate_npv logical (default = FALSE). Should the npv be calculated?
#' @param npv The output of npvQ function.
#' @param npv.y0 The first year in the calculation of net present value (npv).
#' @param npv.yrs The range of years to be considered in the npv calculation.
#' @param desc The description of the case study.
#' @param version c('all', 'stock', 'fleet'), the default is 'all'. Allows using a reduced version of the FLBEIA Shiny app 
#'                (if 'all' , all the pages are shown, if 'stock' only the 'Stocks' and 'Advice' pages are shown and if 'fleet' 
#'                 'Stock', 'Advice', 'Fleets', 'Fleets and Stock' and 'Summary' pages are shown.). 
#' @param deploy logical (default = FALSE). The application is deployed in the external shiny server (shinyapps.io) configured in the computer.
#' 
#' @return The function launches a Shiny app to analyse the results of FLBEIA in an interactive way.
#' 
#' @details If flbeiaObjs is provided some of the other arguments (bio, bioIt, flt, fltIt, fltStk, mt, mtStk, adv and npv) 
#' are not needed, as they are internally calculated. 
#' If not, then the follwing arguments are compulsory: bio, flt, fltStk, mt, mtStk, adv and npv.
#' 
#' The call to the flbeiaApp creates the folders 'inst/flbeiaApp' in the working directory. This is necessary to save the necessary 
#' objects to deploy the application to an external server. Even if deploy = FALSE the folders need to be created because the 'deploy.RData'
#' file needs to be created.
#' 
#' When deploy = TRUE the shiny application is published in the external server linked to the local computer. Shiny makes a copy of the 
#' local R session and replicates it in the server. For that, it installs the R libraries needed directly from CRAN. The libraries that are not in CRAN, 
#' must be Github libraries, and they need to be installed in the local computer using the command 'devtools::install_github'. 
#' Furthermore, in this case the call to  flbeiaApp' must be done from a FLBEIAShiny directory where the raw code of the library is located.
#' 
#' 
#' @examples
#'\dontrun{
#' library(FLBEIAshiny)
#' 
#' 
#' #----------------------------------------------------------------
#' # Example with the summary indicators stored in data.frame-s
#' #----------------------------------------------------------------
#' 
#' data(FLBEIAshiny)
#' 
#' 
#' flbeiaApp(RefPts = RefPts,bio = bioQ, flt = fltQ, adv = advQ, 
#'           fltStk = fltStkQ, mt = mtQ, mtStk = mtStkQ, 
#'           years = as.character(2010:2024), 
#'           calculate_npv = FALSE, npv =  NULL, npv.y0 = NULL, npv.yrs = NULL) 
#' 
#'
#' 
#' #----------------------------------------------------------------
#' # Run FLBEIA first and then use the output to launch flbeiaApp.
#' # In this case we use the FLBEIA output directly.
#' #----------------------------------------------------------------
#' 
#' library(FLBEIA)
#' 
#' data(oneIt)
#' 
#' one_sc1 <- FLBEIA(biols = oneItBio,
#'                     SRs = oneItSR,
#'                     BDs = NULL,
#'                  fleets = oneItFl,
#'                  covars = oneItCv,
#'                 indices = NULL,
#'                  advice = oneItAdv,
#'               main.ctrl = oneItMainC,
#'              biols.ctrl = oneItBioC,
#'             fleets.ctrl = oneItFlC,
#'             covars.ctrl = oneItCvC,
#'                obs.ctrl = oneItObsC,
#'             assess.ctrl = oneItAssC,
#'             advice.ctrl = oneItAdvC)
#' 
#' # We change the target reference point in HCR and run a second scenario
#' 
#'  oneItAdvC$stk1$ref.pts['Fmsy',] <- 0.2
#' 
#' one_sc2 <- FLBEIA(biols = oneItBio,
#'                     SRs = oneItSR,
#'                     BDs = NULL,
#'                  fleets = oneItFl,
#'                  covars = oneItCv,
#'                 indices = NULL,
#'                  advice = oneItAdv,
#'               main.ctrl = oneItMainC,
#'              biols.ctrl = oneItBioC,
#'             fleets.ctrl = oneItFlC,
#'             covars.ctrl = oneItCvC,
#'                obs.ctrl = oneItObsC,
#'             assess.ctrl = oneItAssC,
#'             advice.ctrl = oneItAdvC)
#' 
#' scnms <- c('Ftarget_Fmsy', 'Ftarget_0.15')
#' stknms <- 'stk1'
#' RefPts2 <- expand.grid( indicator=c("Bmsy", "Fmsy", "Bpa", "Blim", "Fpa", "Flim"), 
#'                         scenario=scnms, stock=stknms, value=NA)[,c(3,2,1,4)]
#' RefPts2$value <- c( c(800, 0.11, 800, 550, 0.25, 0.50),  
#'                     c(800, 0.2, 800, 550, 0.25, 0.50))
#' 
#' flbeiaObjs2 <- list(Ftarget_Fmsy = one_sc1, Ftarget_0.15 = one_sc2)
#' 
#' 
#' flbeiaApp( flbeiaObjs = flbeiaObjs2, RefPts = RefPts2, years = ac(2000:2025), 
#'            calculate_npv = TRUE, npv.y0 = '2012', npv.yrs = ac(2013:2025)) 
#' 
#' }   
    

flbeiaApp <- function (flbeiaObjs = NULL, 
                       RefPts = NULL, 
                       bio = NULL, 
                       bioIt = NULL,
                       flt = NULL, 
                       fltIt = NULL,
                       fltStk = NULL, 
                       mt = NULL, 
                       mtStk = NULL, 
                       adv = NULL, 
                       risk = NULL, 
                       years = dimnames(flbeiaObjs[[1]][[1]][[1]]@n)[[2]], 
                       proj.yr = NULL,
                       calculate_npv = FALSE, 
                       npv = NULL, 
                       npv.y0 = NULL, 
                       npv.yrs = NULL,
                       desc = NULL,
                       version = 'all',
                       deploy = FALSE,
                       appName = paste('flbeiaApp', sample(1,1), sep = ""),
                       appTitle = appName
                       ) {
  
  # Check: if flbeiaObjs != NULL --> necessarely bio = bioIt = flt = fltIt = fltStk = mt = mtStk = adv = risk = NULL
  if (!is.null(flbeiaObjs) & (!is.null(bio) | !is.null(bioIt) | !is.null(flt) | !is.null(fltIt) | !is.null(fltStk) | 
                              !is.null(mt) | !is.null(mtStk) | !is.null(adv) | !is.null(risk)))
    stop( "When 'flbeiaObjs' is provided as input the following arguments must be NULL: 
           bio, bioIt, flt, fltIt, fltStk, mt, mtStk, adv, risk")
  
  library(FLBEIA)
  library(kobe)
  library(ggplot2)
  library(schoolmath)
  library(dplyr)
  library(scales)
  library(pals)
  

  
 npv2 <- npv

  if(is.null(RefPts)){

    if(missing(flbeiaObjs)){
      stknms <- unique(bio$stock)
      scnms <- unique(bio$scenario)
    }
    else{
      stknms <- names(flbeiaObjs[[1]]$biols)
      scnms <- names(flbeiaObjs)
    }
    RefPts <- data.frame( stock = rep(stknms, each = 6*length(scnms)),
                          scenario = rep(rep(scnms, each = 6),length(stknms)),
                          indicator = rep(c('Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa', 'Flim'), length(stknms)*length(scnms)),
                          value = NA)
    # # # # ## *********************************
    # # # # ## Reference points for Reference point checkbox plots::
    # names(reference_points)<- c("stock", "scenario", "indicator","value")
    # reference_points$indicator <- NA
    # reference_points$indicator[reference_points$indicator =="Bmsy"] <-"ssb"
    # reference_points$indicator[reference_points$indicator =="Fmsy"] <-"f"
    # # # ## *********************************
    
    }
  # else{
  # 
  #   reference_points <- RefPts
  # }

  if(!is.null(flbeiaObjs)){
    if(is.null(names(flbeiaObjs))){
        scenarios <- 1:length(flbeiaObjs)
    }else{
        scenarios <- names(flbeiaObjs)
    }
    
    bio   <- NULL
    bioIt <- NULL
    flt   <- NULL
    fltIt <- NULL
    fltStk <- NULL
    # RefPts <- NULL
    mt     <- NULL
    mtStk  <- NULL
    adv    <- NULL
    risk  <- NULL
    npv2   <- NULL

    for(sc in names(flbeiaObjs)){
      print(sc)
      flbeiaObj <- flbeiaObjs[[sc]]
      aux     <- bioSum(flbeiaObj, scenario = sc, years = years, long = TRUE)
      bio     <- rbind(bio,bioSumQ(aux))
      bioIt   <- rbind(bioIt,aux)
      aux     <- fltSum(flbeiaObj, scenario = sc, years = years, long = TRUE)
      flt     <- rbind(flt,fltSumQ(aux))
      fltIt   <- rbind(fltIt,aux)
      fltStk  <- rbind(fltStk,fltStkSumQ(fltStkSum(flbeiaObj, scenario = sc, years = years, long = TRUE)))
      mt      <- rbind(mt,mtSumQ(mtSum(flbeiaObj, scenario = sc, years = years, long = TRUE)))
      mtStk   <- rbind(mtStk,mtStkSumQ(mtStkSum(flbeiaObj, scenario = sc, years = years, long = TRUE)))
      adv     <- rbind(adv,advSumQ(advSum(flbeiaObj, scenario = sc, years = years, long = TRUE)))

      Bpa <- subset(RefPts, indicator=='Bpa' & scenario == sc)[,'value']
      names(Bpa) <- subset(RefPts, indicator=='Bpa' & scenario == sc)[,'stock']
      Blim <- subset(RefPts, indicator=='Blim' & scenario == sc)[,'value']
      names(Blim) <- subset(RefPts, indicator=='Blim' & scenario == sc)[,'stock']
      risk   <- rbind(risk,riskSum(flbeiaObj, scenario = sc, Bpa = Bpa, Blim = Blim, Prflim = 0, years = years))
  
  if(calculate_npv == TRUE) 
    npv2    <- rbind(npv2,npvQ(npv(flbeiaObj, scenario = sc, y0 = npv.y0, years = npv.yrs )))
    }
    
    bioIt$iter <- as.factor(bioIt$iter)
    
    }
  if(calculate_npv == FALSE & !is.null(npv)) npv2 <- npv
 
 ## --------------------------------------------------------------------------
 
 ## --------------------------------------------------------------------------
 
 # Reduced version ::
 
 # if (reduced == FALSE)
 #   version <- 1
 # if (reduced == TRUE)
 #   version <- 2
 # 
 ## --------------------------------------------------------------------------
 
 ## --------------------------------------------------------------------------
  
 # Deploy to shinyapps.io
 
 # copy contents to temporary directory and write necessary additional lines to
 # ui, server, and global
 # appDir <- file.path(getwd(), "inst")
 # deployDir <- file.path(appDir, "ShinyStan")
 # contents <- system.file("ShinyStan", package = "shinystan")
 # file.copy(from = contents, to = appDir, recursive = TRUE)
 
 # First need to create a shinystan object.
 # sso <- as.shinystan(flbeiaApp) # replace ... with optional arguments or omit it
 # launch_shinystan(sso)
 
 # save sso to deployDir
 # .SHINYSTAN_OBJECT <- sso
 # save(.SHINYSTAN_OBJECT, file = file.path(deployDir, "sso.RData"))
 
 # also in the same directory put a file called global.R that just contains the line 
 # load("shinystan_object.RData")
 
 
 ## --------------------------------------------------------------------------
 
 ## --------------------------------------------------------------------------
 
 ## Data to NA for KObe plots when data is not provided

  t0 <- subset(bio, indicator == 'f')
  t1 <- subset(bio, indicator == 'ssb')

  data <- cbind(t0[,c("stock", "year", "scenario", "q50")], t1[,'q50'])
  names(data) <- c('unit', 'year', 'scenario', 'q50.f', 'q50.ssb')
  data <- cbind(data, Bmsy = as.numeric(NA), Fmsy = as.numeric(NA))

  for(st in unique(data$unit)){

    for(sc in unique(data$scenario)){

      bmsy <- subset(RefPts, stock == st & scenario == sc & indicator == 'Bmsy')
      fmsy <- subset(RefPts, stock == st & scenario == sc & indicator == 'Fmsy')

      data[data$unit == st & data$scenario == sc, 'Bmsy'] <- bmsy$value
      data[data$unit == st & data$scenario == sc, 'Fmsy'] <- fmsy$value
    }}

  data$stock <- data$q50.ssb/data$Bmsy
  data$harvest <- data$q50.f/data$Fmsy
  
  ## --------------------------------------------------------------------------
  
  ## --------------------------------------------------------------------------
  
  # SSB and F time series reescales to Bmsy and Fmsy.
  # bio.msy = long format
  # bio.kobe = wide format to be used with Kobe plot.
  bio.msy   <- bio %>% filter(indicator %in% c('f', 'ssb') & stock %in% RefPts$stock & scenario %in% RefPts$scenario)

#  browser()
  for(st in unique(RefPts$stock)){
    for(sc in unique(RefPts$scenario)){
      
      bio.msy[bio.msy$stock == st & bio.msy$scenario == sc & bio.msy$indicator == 'f', c('q05', 'q50','q95')] <- 
            bio.msy[bio.msy$stock == st & bio.msy$scenario == sc & bio.msy$indicator == 'f',  c('q05', 'q50','q95')]/
            RefPts[RefPts$stock == st & RefPts$scenario == sc & RefPts$indicator == 'Fmsy', 'value']
      
      
      bio.msy[bio.msy$stock == st & bio.msy$scenario == sc & bio.msy$indicator == 'ssb',  c('q05', 'q50','q95')] <- 
        bio.msy[bio.msy$stock == st & bio.msy$scenario == sc & bio.msy$indicator == 'ssb',  c('q05', 'q50','q95')]/
        RefPts[RefPts$stock == st & RefPts$scenario == sc & RefPts$indicator == 'Bmsy', 'value']
      
    }}
  
 # browser()
  
  bio.msy[bio.msy$indicator == 'f','indicator'] <- 'f2fmsy'
  bio.msy[bio.msy$indicator == 'ssb','indicator'] <- 'B2Bmsy'
  
  bio.msy <- bind_rows(bio, bio.msy)
  
   f   <- bio.msy  %>% filter(indicator == 'f2fmsy')
   ssb <- bio.msy  %>% filter(indicator == 'B2Bmsy')
   
   bio.kobe <- bind_cols(f[, c('scenario', 'stock', 'year','q50')], ssb[,'q50'])
   names(bio.kobe) <- c('scenario', 'unit', 'year', 'harvest', 'stock')

  
  # # # ## *********************************
  # # # ## Reference points for Reference point checkbox plots::
  
   # names(reference_points)<- c("stock", "scenario", "indicator","value")
   # reference_points$indicator <- NA
   # reference_points[reference_points$indicator %in% c("Bmsy","Bpa")]        <-"ssb"
   # reference_points[reference_points$indicator %in% c("Fmsy","Fpa","Flim")] <-"f"

  # names(RefPts)<- c("stock", "scenario", "indicator","value")
  RefPts$ind_type <- NA
  RefPts$ind_type[RefPts$ind_type %in% c("Bmsy","Bpa", "Blim")]  <-"ssb"
  RefPts$ind_type[RefPts$ind_type %in% c("Fmsy","Fpa","Flim")]   <-"f"
  RefPts$refpt_type[RefPts$indicator %in% c("Fmsy","Bmsy")]   <-"MSY"
  RefPts$refpt_type[RefPts$indicator %in% c("Fpa","Bpa")]     <-"PA"
  RefPts$refpt_type[RefPts$indicator %in% c("Flim","Blim")]   <-"LIM"
  
  
  ## --------------------------------------------------------------------------
  
  ## --------------------------------------------------------------------------
  
  ## Assign object in globalenv() to code ui and sever

   assign("bio",        bio,envir = globalenv())
   assign("bioIt",      bioIt,envir = globalenv())
   assign("bio.kobe",   bio.kobe,envir = globalenv())
   assign("flt",        flt,envir = globalenv())
   assign("fltIt",      fltIt,envir = globalenv())
   assign("fltStk",     fltStk,envir = globalenv())
   assign("mt",         mt,envir = globalenv())
   assign("mtStk",      mtStk,envir = globalenv())
   assign("adv",        adv,envir = globalenv())
   assign("risk",       risk,envir = globalenv())
   assign("RefPts",     RefPts,envir = globalenv())
   assign("npv2",       npv2,envir = globalenv())
   assign("npv",        npv2,envir = globalenv())
   assign("proj.yr",    proj.yr,envir = globalenv())
   assign("version",    version, envir = globalenv())
   assign("data",       data,envir = globalenv())
   assign("desc",       desc, envir = globalenv())
   # assign("reference_points",  reference_points,envir = globalenv())
   # assign("bio.scaled", bio.scaled,envir = globalenv())
   # assign("flt.scaled", flt.scaled,envir = globalenv())
   # 
   ## --------------------------------------------------------------------------

 # load('FLBEIAApp.Rdata')
   
   print('before deploy')
   
   dir.create(file.path(getwd(), 'inst/flbeiaApp/data')) 
   save(deploy, file= "inst/flbeiaApp/data/deploy.RData")
   save(deploy, file=file.path(.libPaths(), "FLBEIAshiny/flbeiaApp/data/deploy.RData"))
   
    
   
   if (deploy == FALSE){

     print('deploy = FALSE: local application')
     
    shiny::runApp(system.file('flbeiaApp', package='FLBEIAshiny'), launch.browser = TRUE)
   }
   else{
     print('deploy = TRUE: publish the application in an external server')
     
     save.image(file= "inst/flbeiaApp/data/App.RData")
     save.image(file=file.path(.libPaths(), "FLBEIAshiny/flbeiaApp/data/App.RData"))
     
      appDir <- file.path(getwd(), "inst/flbeiaApp")
      
      print(appDir)
   
      rsconnect::deployApp(
        appDir = appDir, #,
        appName = appName,
        appTitle = appTitle
      # usng appFiles = c('ui.R', 'server.',,,) it seems it works but then it does not found the             
        #account = account,
        #lint = TRUE
      )
   }
   
  }

