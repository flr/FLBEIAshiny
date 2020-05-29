#-------------------------------------------------------------------------------
#
#' Launch FLBEIA-Shiny application
#' 
#' FLBEIA Shiny application is an interactive interface to analyze the biological, economic and social indicators obtained through FLBEIA simulation model. It provides lots of graphics at scenario, stock, fleet and metier level to facilitate the analysis of the results and the comparison among scenarios.
#'
#' @param flbeiaObjs A named list with a set of FLBEIA outputs, each element of the list corresponding with one scenario. The names of the list will be used to name the scenarios.
#' @param RefPts A data frame with columns, 'stock', 'scenario', 'indicator', and 'value', with the values of 'Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa' and 'Flim' per stock and scenario. If the value for certain stock and/or scenario is not available NA should be used. If the data.frame is not available in the function call the data frame is created internally with NA for all the cases.   
#' @param bio The output of bioSumQ function.
#' @param bioIt The selection withing the output oneItBio.
#' @param flt The output of fltSumQ function.
#' @param fltIt The selection withing the output oneItFl.
#' @param fltStk The output of fltStkSumQ function.
#' @param mt The output of mtSumQ function.
#' @param mtStk The output of mtStkSumQ function.
#' @param adv The output of advSumQ function.
#' @param risk The output of riskSum function.
#' @param years The years to be included in the application.
#' @param proj.yr The year in which the projection starts.
#' @param calculate_npv logical, should the npv be calculated?
#' @param npv The output of npvQ function.
#' @param npv.y0 The first year in the calculation of net present value (npv).
#' @param npv.yrs The range of years to be considered in the npv calculation.
#' @param desc The description of the case study.
#' @param reduced logical, the version of the FLBEIA shiny app.
#' @param deploy logical, the deployment into shinyapps.io.
#' 
#' @return The function launch a Shiny-App with to analyse the results of FLBEIA in an interactive way.
#' 
#' @details If flbeiaObjs is provided the most of the other arguments (from bio,..., risk and npv) are not needed,  they are internally calculated. If it is not provided, it is neccesary to provide the rest arguments.
#' 
#' @examples
#'\dontrun{
#' library(FLBEIAShiny)          # required to use the IcesHCR. Not available for win64
#' 
#' 
#' #----------------------------------------------------------------
#' # Example with the summary indicators stored in data.frame-s
#' #----------------------------------------------------------------
#' 
#' data(FLBEIAShiny)
#' 
#' 
#' flbeiaApp(RefPts = RefPts,bio = bioQ, flt = fltQ, adv = advQ, 
#'           fltStk = fltStkQ, mt = mtQ, mtStk = mtStkQ, risk = risk,
#'           years = as.character(2010:2024), 
#'           calculate_npv = FALSE, npv =  NULL, npv.y0 = NULL, npv.yrs = NULL) 
#' 
#'
#' 
#' #----------------------------------------------------------------
#' # Run FLBEIA first and then use the output to launch flbeiaApp.
#' # In this case we use the output of FLBEIA directly.
#' #----------------------------------------------------------------
#' library(FLBEIA)
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
#' RefPts <- expand.grid(indicator=c("Bmsy", "Fmsy", "Bpa", "Blim", "Fpa", "Flim"), scenario=scnms, stock=stknms, value=NA)[,c(3,2,1,4)]
#' RefPts$value <- c(c(800, 0.11, 800, 550, 0.25, 0.50),  c(800, 0.2, 800, 550, 0.25, 0.50))
#' 
#' flbeiaObjs <- list(Ftarget_Fmsy = one_sc1, Ftarget_0.15 = one_sc2)
#' 
#' 
#' flbeiaApp(flbeiaObjs = flbeiaObjs, RefPts = RefPts, years = ac(2000:2025), calculate_npv = TRUE, npv.y0 = '2012', npv.yrs = ac(2013:2025)) 
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
                       calculate_npv = NULL, 
                       npv = NULL, 
                       npv.y0 = NULL, 
                       npv.yrs = NULL,
                       desc = NULL#,
                       #reduced = T,
                       #deploy = F
                       ) {
  require(FLBEIA)
  require(kobe)
  require(ggplot2)
  require(schoolmath)

  
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
    reference_points <- data.frame( stock = rep(stknms, each = 6*length(scnms)),
                                    scenario = rep(rep(scnms, each = 6),length(stknms)),
                                    indicator = rep(c('Bmsy','Fmsy', 'Bpa', 'Blim', 'Fpa', 'Flim'), length(stknms)*length(scnms)),
                                    value = NA)
    # # # ## *********************************
    # # # ## Reference points for Reference point checkbox plots::
    names(reference_points)<- c("stock", "scenario", "refpoint","value")
    reference_points$indicator <- NA
    reference_points$indicator[reference_points$refpoint =="Bmsy"] <-"ssb"
    reference_points$indicator[reference_points$refpoint =="Fmsy"] <-"f"
    # # ## *********************************
    
    }
  else{

    reference_points <- RefPts
  }

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
    RefPts <- NULL
    mt     <- NULL
    mtStk  <- NULL
    adv    <- NULL
    risk  <- NULL
    npv2   <- NULL

    for(sc in names(flbeiaObjs)){
      print(sc)
      flbeiaObj <- flbeiaObjs[[sc]]
      aux     <- bioSum(flbeiaObj, scenario = sc, years = years)
      bio     <- rbind(bio,bioSumQ(aux))
      bioIt   <- rbind(bioIt,aux)
      aux     <- fltSum(flbeiaObj, scenario = sc, years = years)
      flt     <- rbind(flt,fltSumQ(aux))
      fltIt   <- rbind(fltIt,aux)
      fltStk  <- rbind(fltStk,fltStkSumQ(fltStkSum(flbeiaObj, scenario = sc, years = years)))
      mt      <- rbind(mt,mtSumQ(mtSum(flbeiaObj, scenario = sc, years = years)))
      mtStk   <- rbind(mtStk,mtStkSumQ(mtStkSum(flbeiaObj, scenario = sc, years = years)))
      adv     <- rbind(adv,advSumQ(advSum(flbeiaObj, scenario = sc, years = years)))

      Bpa <- subset(reference_points, indicator=='Bpa' & scenario == sc)[,'value']
      names(Bpa) <- subset(reference_points, indicator=='Bpa' & scenario == sc)[,'stock']
      Blim <- subset(reference_points, indicator=='Blim' & scenario == sc)[,'value']
      names(Blim) <- subset(reference_points, indicator=='Blim' & scenario == sc)[,'stock']
      risk   <- rbind(risk,riskSum(flbeiaObj, scenario = sc, Bpa = Bpa, Blim = Blim, Prflim = 0, years = years))
  
  if(calculate_npv == TRUE) npv2    <- rbind(npv2,npvQ(npv(flbeiaObj, scenario = sc, y0 = npv.y0, years = npv.yrs )))
    }}
  if(calculate_npv == FALSE & !is.null(npv)) npv2 <- npv
 
 ## --------------------------------------------------------------------------
 
 ## Data to NA for KObe plots when data is not provided

  t0 <- subset(bio, indicator == 'f')
  t1 <- subset(bio, indicator == 'ssb')

  data <- cbind(t0[,c("stock", "year", "scenario", "q50")], t1[,'q50'])
  names(data) <- c('unit', 'year', 'scenario', 'q50.f', 'q50.ssb')
  data <- cbind(data, Bmsy = NA,Fmsy = NA)

  for(st in unique(data$unit)){

    for(sc in unique(data$scenario)){

      bmsy <- subset(reference_points, stock == st & scenario == sc & refpoint == 'Bmsy')
      fmsy <- subset(reference_points, stock == st & scenario == sc & refpoint == 'Fmsy')

      data[data$unit == st & data$scenario == sc, 'Bmsy'] <- bmsy$value
      data[data$unit == st & data$scenario == sc, 'Fmsy'] <- fmsy$value
    }}

  data$stock <- data$q50.ssb/data$Bmsy
  data$harvest <- data$q50.f/data$Fmsy
  ## --------------------------------------------------------------------------
  
  ## --------------------------------------------------------------------------
  
  ## rescale all the coordinates within 0 and 1 and 
  ## melt the dataset in order to plot it easily with ggplot.
  require(dplyr)
  require(scales)
  
  bio.scaled <- bio %>% group_by(stock, scenario) %>% mutate(value2 = rescale(q50))
  bio.scaled <- as.data.frame(bio.scaled)
  
  
  flt.scaled <- flt %>% group_by(fleet, scenario) %>% mutate(value2 = rescale(q50))
  flt.scaled <- as.data.frame(flt.scaled)
  
  
  # # # ## *********************************
  # # # ## Reference points for Reference point checkbox plots::
  
   # names(reference_points)<- c("stock", "scenario", "refpoint","value")
   # reference_points$indicator <- NA
   # reference_points[reference_points$refpoint =="Bmsy"] <-"ssb"
   # reference_points[reference_points$refpoint =="Fmsy"] <-"f"

  # names(RefPts)<- c("stock", "scenario", "refpoint","value")
  # RefPts$indicator <- NA
  # RefPts$indicator[RefPts$refpoint =="Bmsy"] <-"ssb"
  # RefPts$indicator[RefPts$refpoint =="Fmsy"] <-"f"
  
  
  ## --------------------------------------------------------------------------
  
  ## --------------------------------------------------------------------------
  
  ## Assign object in globalenv() to code ui and sever

   assign("bio",        bio,envir = globalenv())
   assign("bioIt",      bioIt,envir = globalenv())
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
   assign("data",       data,envir = globalenv())
   assign("reference_points",  reference_points,envir = globalenv())
   assign("bio.scaled",        bio.scaled,envir = globalenv())
   assign("flt.scaled",        flt.scaled,envir = globalenv())
   
   ## --------------------------------------------------------------------------

 # load('FLBEIAApp.Rdata')
  shiny::runApp(system.file('flbeiaApp', package='FLBEIAShiny'), launch.browser = TRUE)
   
  }

