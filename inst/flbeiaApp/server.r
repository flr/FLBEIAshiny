#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CASE SPECIFIC SIZE IN PLOT
# These 4 pieces of code are needed
#
# plotPolar = the code with all the code of ggplot...
# plotP = the name we used in ui to call the plot.
# 
#
# plot_height <- function() {
#     facets <- length(input$scenario)
#     values$facetsRows <- ifelse(facets/input$nColP == 1, 600, ifelse(facets/input$nColP == 2, 800, 
#                                                                  ifelse(facets/input$nColP == 3, 100, 300*ceiling(facets/input$nColP))))
#     return(values$facetsRows)
# }
# 
# plot_width <- function() {
#   
#   values$facetsCols <- ifelse(input$nColP == 1, 600, ifelse(input$nColP <= 2, 900, 1200))
#   return(values$facetsCols)
# }

# output$plotPs<-renderPlot({
#   
#   print(plotPolar())
# } #, height = PlotHeight_stk
# )
# 
# # wrap plotOutput in renderUI
# output$plotP <- renderUI({
#   plotOutput("plotPs", height = plot_height(), width = plot_width())
# })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# Change the size of the plot area: https://groups.google.com/g/shiny-discuss/c/dkZxTvfHOvo?pli=1

source ("global.R") # radar plot function

server <- function(input, output, session){

  
  observe({
    
    if (version == 1){ 
      
      #shinyjs::show(id = "Fleets")
      showTab(inputId = "tabs", target = "Fleets")
      showTab(inputId = "tabs", target = "Fleets by stock")
      showTab(inputId = "tabs", target = "Metiers")
      showTab(inputId = "tabs", target = "Metiers by stock")

      
      }else {
      
      hideTab(inputId = "tabs", target = "Fleets")
      hideTab(inputId = "tabs", target = "Fleets by stock")
      hideTab(inputId = "tabs", target = "Metiers")
      hideTab(inputId = "tabs", target = "Metiers by stock")
    }
    })
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_about  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
  
  output$value <- renderText({ 
    req(input$submit)
    input$caption 
    })
  
   observeEvent(input$submit, {
     
   if(input$submit %% 2 == 1){
     shinyjs::hide(id = "caption")
     shinyjs::show(id = "value")
    }else{         
     shinyjs::show(id = "caption")
     shinyjs::hide(id = "value")
    }
})
   
   #CASE STUDY TEXT
   
   output$cs <- renderTable({
     desc
   }, colnames = F)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
   #### PAGE_simulation STOCK  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
  
  # PlotHeight_stk <- reactive({
  #   
  #   nids <- length(input$stockS)
  #   
  #   return(300*nids)})
  # 
  
  observe ({
    dataS<-reactive({
      req(input$stockS)
      bio[bio$year>=input$rangeS[1] & bio$year<=input$rangeS[2] 
          & bio$stock%in%input$stockS
          & bio$indicator%in%input$indicatorS
          & bio$scenario%in%input$scenarioS,]
    })
    
    dataSI<-reactive({
      req(input$iterS)
      bioIt[bioIt$year>=input$rangeS[1] & bioIt$year<=input$rangeS[2] 
             & bioIt$stock%in%input$stockS
             & bioIt$indicator%in%input$indicatorS
             & bioIt$scenario%in%input$scenarioS
             & bioIt$iter%in%input$iterS,]
    })

    datarpS<-reactive({
        req(input$stockS)
        RefPts[RefPts$stock%in%input$stockS
                & RefPts$indicator%in%input$indicatorS
                & RefPts$scenario%in%input$scenarioS,]
      })
    
      # dataSH <-reactive({
      #   req(input$stockS)
      #   bio[bio$year>=input$rangeS[1] & bio$year<=input$rangeS[2] 
      #       & bio$stock%in%input$stockS
      #       & bio$indicator%in% c('f2Ftarget', 'B2Btarget', 'ssb2Btarget')
      #       & bio$scenario%in%input$scenarioS,]
      # })
      
    plotStock <- function(){
      
      p <-ggplot()+
        geom_line(data = dataS(), aes(x=year, y=q50, color=scenario), size = input$lwdS) +
        ylab("")+xlab("Year")+
        theme_bw()+
        theme( strip.text=element_text(size=16),
               title=element_text(size=16),
               text=element_text(size=16))
      
      # Iteraction
       if(!is.null(input$iterS)){
         p <- p + geom_line(data = dataSI(), aes(x=year, y=value, group = interaction(scenario, iter), color = scenario,  linetype = iter), lwd=1)+
           scale_linetype_manual(values = c(2:6))
       }

      if(!is.null(proj.yr)){
        p <- p +  geom_vline(data = dataS(), aes(xintercept=proj.yr), color="grey", linetype="dashed", lwd =1) # projection starting year 
      }
      
      if(input$dotLineS == TRUE) p <- p +  geom_point(data = dataS(), aes(x=year, y=q50, color=scenario), size = input$dszS)
      
      # Refence points
        if (input$refpointS == TRUE ){
          validate (
            need(nrow(datarpS())>0, "Please check if reference points are loaded or adequate indicator selected"))
          #p <- p +geom_hline(data = datarpS(), aes(yintercept=value), color="red", linetype="dotted", lwd =1)
          p <- p +geom_hline(data = datarpS(), aes(yintercept=value, group = interaction(scenario, refpt_type),  color= scenario, linetype=refpt_type), lwd =1)+
            scale_linetype_manual(values = c(2:4))
          #! MK: debes cambiar para que acepte mas de un pto de referencia (poner distintos tipos de linea)
          }
      
      # Confidence intervals
      if (input$fitCIS == TRUE){
        p <- p + geom_ribbon(data = dataS(), aes(x=year, ymin=q05, ymax=q95,fill = scenario), alpha=0.3)#+
                 #geom_ribbon(data = dataSI(), aes(x=year, ymin=q05, ymax=q95,group = interaction(scenario, iter), fill = scenario), alpha=0.1)
      }
      
      if(input$fitS == FALSE){
          p <- p + facet_grid(stock~indicator)
      }
      else{
          p <- p + facet_wrap(stock~indicator, scale = 'free_y', ncol = input$nColS)
      }
      
   #   browser()
      cond1   <- any(sapply(c('pFlim','pFpa','pFtarget', 'pBlim','pBpa','pBtarget'), function(x) any(grepl(x, input$indicatorS))))
      cond2   <- any(sapply(c('ssb2Btarget', 'f2Ftarget'), function(x) any(grepl(x, input$indicatorS))))
      
        
      if(cond1){  
        p <-  p + geom_hline(data= dataS() %>% filter(indicator %in% c('pFlim','pFpa','pFtarget',
                                                                       'pBlim','pBpa','pBtarget')),
                           aes(yintercept = 0.05), linetype = 'dashed')
        if(cond2){ # cond1 + cond2
          p <-  p + geom_hline(data= dataS() %>% filter(indicator %in% c('ssb2Btarget', 'f2Ftarget')),
                               aes(yintercept = 1), linetype = 'dashed')}
        else{p} # only cond1
      }
      else{ 
        if(cond2){ # only cond2
        #  browser()
          p <-  p + geom_hline(data= dataS() %>% filter(indicator %in% c('ssb2Btarget', 'f2Ftarget')),
                               aes(yintercept = 1), linetype = 'dashed')}
        else{p} # nor cond1 or cond1
      }
    }

    
    # Case dependent plot size.
    #     https://stackoverflow.com/questions/30422849/how-to-make-height-argument-dynamic-in-renderplot-in-shiny-r-package
    # this function defines a height of the plot
    values <- reactiveValues()
    plot_height <- function() {
      # calculate values$facetCount
      values$facetsRows <- ifelse(length(input$stockS) == 1, 600, ifelse(length(input$stockS) == 2, 800, ifelse(length(input$stockS) == 3, 900, 200*length(input$stockS))))
      return(values$facetsRows)
    }
    plot_width <- function() {
      # calculate values$facetCount
      values$facetsCols <- ifelse(length(input$indicatorS)==1, 900, 1500)
      return(values$facetsCols)
    }
    
    output$plotSs<-renderPlot({
      
      print(plotStock())
    } #, height = PlotHeight_stk
    )
    
    # wrap plotOutput in renderUI
    output$plotS <- renderUI({
      plotOutput("plotSs", height = plot_height(), width = plot_width())
    })
    
    
    
    # Code to download the plot
    getW <- function(){
      return(input$fileWS)
    }
    
    getH <- function(){
      return(input$fileHS)
    }
    
    getS <- function(){
      return(input$fileScS)
    }
    
    # Download the plot
    output$down <- downloadHandler(
      filename =  function() {
        paste(input$filenmS, input$fileTypeS, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotStock(), width = getW(), height = getH(), units = 'cm', scale = getS())
        } 
    )
    
  })# end of the observe stock 

  print('one')
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation STOCK AREA  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
  
  # PlotHeight_stk <- reactive({
  #   
  #   nids <- length(input$stockS)
  #   
  #   return(300*nids)})
  # # 
  # 
  observe ({
    dataSA<-reactive({
      bio <- bio %>% filter(year >= input$rangeSA[1], year <= input$rangeSA[2],
                            stock %in% input$stockSA,
                            indicator %in% input$indicatorSA,
                            scenario %in% input$scenarioSA)
      if(input$percSA == TRUE){
      bio <- bio  %>%  ungroup() %>%  
         group_by(year, scenario, indicator) %>%
        mutate(p = q50/sum(q50)) %>% mutate(q50=p)
      
      }
      print(bio[1:5,])
    return(bio)
      })

    plotStockArea <- function(){

      p <-ggplot(data = dataSA())+
        geom_area(aes(x=year, y=q50, fill=stock), size=0.5, colour="grey") +
        ylab("")+xlab("Year")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
               title=element_text(size=16),
               text=element_text(size=16))

      if(!is.null(proj.yr)){
        p <- p +  geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year
      }
      
      if(input$fitSA == FALSE){
        p <- p + facet_grid(scenario~indicator)
      }
      else{
        p <- p + facet_wrap(scenario~indicator, scale = 'free_y', ncol = input$nColSA)
      }
      
    }


    output$plotSAs <-renderPlot({

      print(plotStockArea())
    } #, height = PlotHeight_stk
    )

    
    # Case dependent plot size.
    #     https://stackoverflow.com/questions/30422849/how-to-make-height-argument-dynamic-in-renderplot-in-shiny-r-package
    # this function defines a height of the plot
    values <- reactiveValues()
    plot_height <- function() {
      # calculate values$facetCount
      values$facetsRows <- ifelse(length(input$scenarioSA) == 1, 600, ifelse(length(input$scenarioSA) == 2, 800, ifelse(length(input$scenarioSA) == 3, 900, 200*length(input$scenarioSA))))
      return(values$facetsRows)
    }
    plot_width <- function() {
      # calculate values$facetCount
      values$facetsCols <- ifelse(length(input$indicatorSA)==1, 900, 1500)
      return(values$facetsCols)
    }
    
    # wrap plotOutput in renderUI
    output$plotSA <- renderUI({
      plotOutput("plotSAs", height = plot_height(), width = plot_width())
    })
    

    # Code to download the plot
    getWSA <- function(){
      return(input$fileWSA)
    }

    getHSA <- function(){
      return(input$fileHSA)
    }

    getSSA <- function(){
      return(input$fileScSA)
    }

    # Download the plot
    output$downSA <- downloadHandler(
      filename =  function() {
        paste(input$filenmSA, input$fileTypeSA, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotStockArea(), width = getWSA(), height = getHSA(), units = 'cm', scale = getSSA())
      }
    )

  })# end of the observe stock

  print('one - b')
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
#### PAGE_simulation STOCK_kobe plot ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
 observe({ 
   dataK<-reactive({
        req(input$stockK)
  
    res <- bio.kobe[bio.kobe$year>=input$rangeK[1] & bio.kobe$year<=input$rangeK[2] 
        & bio.kobe$unit%in%input$stockK
        & bio.kobe$scenario%in%input$scenarioK,]

    res
    }
  )

  
  plotKobe <- function(){

    dd <- dataK()
    dy0 <- subset(dd, year == unique(dd$year)[1])
    dy1 <- subset(dd, year == unique(dd$year)[length(unique(dd$year))])
    
    kobePhase(dataK(), ylim = c(0, max(dataK()$harvest)), xlim = c(0, max(dataK()$stock))) + 
      geom_point(aes(stock,harvest, group = scenario, col = scenario)) + 
      geom_path( aes(stock,harvest, group = scenario, col = scenario)) + 
      geom_text(aes(stock, harvest, label = year), data = dy0, pch = 16, col = 1) +
      geom_text(aes(stock, harvest, label = year), data = dy1, pch = 4, col = 1) +
      facet_wrap(~unit, ncol = input$nColK) +
      theme(text=element_text(size=16),
            title=element_text(size=16),
            strip.text=element_text(size=16)) #+
  #    labs(caption = 'First year = black dot & Final year = black cross')
  }
  
  output$plotKs <- renderPlot({
 #   browser()
    if (is.null(dataK())) return()
    plotKobe()
  })
  
  # Case dependent plot size.
  values <- reactiveValues()
  plot_height <- function() {
    # calculate values$facetCount
    values$facetsRows <- ifelse(length(input$stockK) <=2, 400, ifelse(length(input$stockK) <= 4, 600, 
                                      ifelse(length(input$stockK) <= 6, 800, 250*ceiling(length(input$stockK)/2))))
    return(values$facetsRows)
  }
  plot_width <- function() {
    # calculate values$facetCount
    values$facetsCols <- ifelse(length(input$stockK)==1, 600, 900)
    return(values$facetsCols)
  }
  
  # wrap plotOutput in renderUI
  output$plotK <- renderUI({
    plotOutput("plotKs", height = plot_height(), width = plot_width())
  })
  
  # Code to download the plot
  getWSK <- function(){
    return(input$fileWSK)
  }
  
  getHSK <- function(){
    return(input$fileHSK)
  }
  
  getSSK <- function(){
    return(input$fileScSK)
  }
  
  # Download the plot
  output$downSK <- downloadHandler(
    filename =  function() {
      paste(input$filenmSK, input$fileTypeSK, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, plotKobe(), width = getWSK(), height = getHSK(), units = 'cm', scale = getSSK())
    } 
  )
  
 })
  
  print('two')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation STOCK_Spider plot  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
    dataSP<-reactive({
      req(input$baseSP)

      if (input$baseSP == "radio1"){
          req(input$stockSP)
       dat1 <- subset(bio, year == input$baseSP2 & stock %in% input$stockSP & 
                           indicator %in% input$indicatorSP & scenario%in%input$scenarioSP)
       dat2 <- subset(bio, year == input$baseSP1 & stock %in% input$stockSP & 
                           indicator %in% input$indicatorSP & scenario%in%input$scenarioSP)
       
       dat1 <- dat1 %>% group_by(stock, indicator, scenario)
       dat2 <- dat2 %>% group_by(stock, indicator, scenario)
       
       dat2 <- dat2[, c('scenario', 'stock', 'indicator', 'q50')]
       names(dat2)[4] <- 'q502' 
      }
         
      if (input$baseSP == "radio2"){ # base1 is scenario and base2 is year
        req(input$stockSP)
        # data frame with all the scenarios and the selected year
        dat1 <- subset(bio, year == input$baseSP4 & stock %in% input$stockSP & 
                         indicator %in% input$indicatorSP & scenario%in%input$scenarioSP)
        # data frame with Base scenario and the selected year
        dat2 <- subset(bio, year == input$baseSP2 & stock %in% input$stockSP & 
                         indicator %in% input$indicatorSP & scenario == input$baseSP3)
        
        dat1 <- dat1 %>% group_by(stock, indicator, scenario)
        dat2 <- dat2 %>% group_by(stock, indicator)
     
        dat2 <- dat2[, c('stock', 'indicator', 'q50')]
        names(dat2)[3] <- 'q502' 
      }
      
        dat <- dat1 %>% left_join(dat2)
        
        dat <- dat %>% mutate(Ratio = (q50-q502)/q502)
        
        dat <- dat[order(dat$scenario), ]
      
      dat
      })

    
  plotSpider <-function(){

       dt <- dataSP()
       
       dt0 <- dt
       dt0$Ratio <- 0
      
       lines <- input$GrpPanSP
      
       if(lines == 'stock'){
        p <-  ggplot(data=dataSP(), aes(x=scenario, y=Ratio, col=stock, fill=stock, group=stock))+
         # geom_polygon(alpha=0.2, lwd=1)+
         geom_polygon(fill=NA, lwd=1)+
         geom_point(cex=1.5)+
         geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
         coord_radar()+
         theme_bw()+
         theme(text=element_text(size=14),
               strip.text=element_text(size=14),
               title=element_text(size=18,face="bold"))+
         ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
          facet_wrap(indicator~., ncol = input$nColSP)
        
        }
       else{ #lines == indicator
         p <-  ggplot(data=dataSP(), aes(x=scenario, y=Ratio, col=indicator, fill=indicator, group=indicator))+
           # geom_polygon(alpha=0.2, lwd=1)+
           geom_polygon(fill=NA, lwd=1)+
           geom_point(cex=1.5)+
           geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
  #         geom_hline(aes(yintercept=0), lwd=1, lty=2) +
           coord_radar()+
           theme_bw()+
           theme(text=element_text(size=14),
                 strip.text=element_text(size=14),
                 title=element_text(size=18,face="bold"))+
           ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
           facet_wrap(stock~.,  ncol = input$nColSP)
       }
      return(p)
      
    }
  
  output$plotSPs <- renderPlot({
    #   browser()
    if (is.null(dataSP())) return()
    plotSpider()
  })
  
  # Case dependent plot size.
  values <- reactiveValues()
  plot_height <- function() {
    lines <- input$GrpPanSP
    if(lines == 'stock'){ facets <- input$indicatorSP} 
    else{ facets <- input$stockSP}
    
    # calculate values$facetCount
    values$facetsRows <- ifelse(length(facets) <=2, 400, ifelse(length(facets) <= 4, 600, 
                                                                      ifelse(length(facets) <= 6, 800, 250*ceiling(length(facets)/2))))
    return(values$facetsRows)
  }
  plot_width <- function() {
    lines <- input$GrpPanSP
    if(lines == 'stock'){ facets <- input$indicatorSP} 
    else{ facets <- input$stockSP}
    # calculate values$facetCount
    values$facetsCols <- ifelse(length(input$stockK)==1, 600, 900)
    return(values$facetsCols)
  }

  
  # wrap plotOutput in renderUI
  output$plotSP<- renderUI({
    plotOutput("plotSPs", height = plot_height(), width =plot_width())
  })
  
  
  # Code to download the plot
  getWSP <- function(){
    return(input$fileWSP)
  }
  
  getHSP <- function(){
    return(input$fileHSP)
  }
  
  getScSP <- function(){
    return(input$fileScSP)
  }
  
  # Download the plot
  output$downSP <- downloadHandler(
    filename =  function() {
      paste(input$filenmSP, input$fileTypeSP, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, plotSP(), width = getWSP(), height = getHSP(), units = 'cm', scale = getScSP())
    } 
  )
  
print('three spider')
    
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
#### PAGE_simulation FLEET  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
#### PAGE_simulation FLEET_TIMES SERIES  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
  # PlotHeight_flt <- reactive({
  #   nids <- length(input$fleetF)
  #   return(300*nids)})
  # 
  
  observe ({
    dataF<-reactive({
      req(input$fleetF)
      flt[flt$year>=input$rangeF[1] & flt$year<=input$rangeF[2] 
          & flt$fleet%in%input$fleetF
          & flt$scenario%in%input$scenarioF
          & flt$indicator%in%input$indicatorF,]
    })
    
    dataFI<-reactive({
      req(input$iterF)
      fltIt[fltIt$year>=input$rangeF[1] & fltIt$year<=input$rangeF[2] 
               & fltIt$fleet%in%input$fleetF
               & fltIt$indicator%in%input$indicatorF
               & fltIt$scenario%in%input$scenarioF
               & fltIt$iter%in%input$iterF,]
    })
    
    
    plotFleet <- function(){
      
      p <- ggplot()+
                geom_line(data= dataF(), aes(x=year, y=q50, color=scenario),lwd=1)+
                ylab("")+ xlab("Year")+
                theme_bw()+
                theme( strip.text=element_text(size=16),
                        title=element_text(size=16),
                        text=element_text(size=16))+
                scale_x_continuous(limits = c(input$rangeF[1], input$rangeF[2]))
      
      # Iteraction
      if(!is.null(input$iterF)){
        p <- p + geom_line(data = dataFI(), aes(x=year, y=q50, group = interaction(scenario, iter), color = scenario,  linetype = iter), lwd=1)+
          scale_linetype_manual(values = c(2:6))
      }
      
      
      if(!is.null(proj.yr)){
        p <- p + geom_vline(data=dataF(), aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year 
          
      }
      
      
      # With Conf Int.
      if (input$fitCIF == TRUE){
        p <- p + geom_ribbon(data = dataF(),  aes(x=year, ymin=q05, ymax=q95,fill = scenario), alpha=0.3)#+
                 #geom_ribbon(data = dataFI(), aes(x=year, ymin=q05, ymax=q95,group = interaction(scenario, iter), fill = scenario), alpha=0.1)
      }
      
      if(input$fitF==TRUE){
        p <- p + facet_wrap(fleet ~ indicator, ncol = input$nColF, scales="free_y")
      }
      else{
        p <- p + facet_grid(fleet ~ indicator)  
      }
      
      return(p)
      }
    
   
    output$plotF <-renderPlot({
      print(plotFleet())
    }#, height = PlotHeight_flt
    )
    
    # Code to download the plot
    getFW <- function(){
      return(input$fileWF)
    }
    
    getFH <- function(){
      return(input$fileHF)
    }
    
    getFS <- function(){
      return(input$fileScF)
    }
    
    # Download the plot
    output$downF <- downloadHandler(
      filename =  function() {
        paste(input$filenmF, input$fileTypeF, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotFleet(), width = getFW(), height = getFH(), units = 'cm', scale = getFS())
      } 
    )
    
    })#end of the observer
  print('four') 

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
  #### PAGE_simulation FLEET_NPV  ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
  
    # print('caracola02')   
    dataN<-reactive({
        req(input$fleetN)
        npv[npv$fleet%in%input$fleetN & npv$scenario%in%input$scenarioN,]})

    plotNPV <- function(){
      ggplot(dataN(), aes(x=fleet, y=q50, group=scenario))+
        geom_point(aes(color=fleet),cex=2)+
        geom_errorbar(aes(ymin=q05, ymax=q95, color=fleet), lwd=1)+
        theme_bw()+
        facet_wrap(~scenario, ncol = input$nColNPV)+
        theme(text=element_text(size=16),
              title=element_text(size=16),
              strip.text=element_text(size=16),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())+
        ylab("NPV")
    }
    
    output$plotFN<-renderPlot({
      plotNPV()
    })
      
      # Code to download the plot
      getFNW <- function(){
        return(input$fileWFN)
      }
      
      getFNH <- function(){
        return(input$fileHFN)
      }
      
      getFNS <- function(){
        return(input$fileScFN)
      }
      
      # Download the plot
      output$downFN <- downloadHandler(
        filename =  function() {
          paste(input$filenmFN, input$fileTypeFN, sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          ggsave(file, plotNPV(), width = getFNW(), height = getFNH(), units = 'cm', scale = getFNS())
        } 
      )
    
  
  
      print('five')  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #### PAGE_simulation FLEET_Spider plot  ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
      dataFSP<-reactive({
        req(input$baseFSP)
        
        if (input$baseFSP == "radio1F"){
          req(input$fleetFSP)
          dat1 <- subset(flt, year == input$baseFSP2 & fleet %in% input$fleetFSP & 
                           indicator %in% input$indicatorFSP & scenario%in%input$scenarioFSP)
          dat2 <- subset(flt, year == input$baseFSP1 & fleet %in% input$fleetFSP & 
                           indicator %in% input$indicatorFSP & scenario%in%input$scenarioFSP)
          
          dat1 <- dat1 %>% group_by(fleet, indicator, scenario)
          dat2 <- dat2 %>% group_by(fleet, indicator, scenario)
          
          dat2 <- dat2[, c('scenario', 'fleet', 'indicator', 'q50')]
          names(dat2)[4] <- 'q502' 
        }
        
        if (input$baseFSP == "radio2F"){ # base1 is scenario and base2 is year
          req(input$fleetFSP)
          # data frame with all the scenarios and the selected year
          dat1 <- subset(flt, year == input$baseFSP4 & fleet %in% input$fleetFSP & 
                           indicator %in% input$indicatorFSP & scenario%in%input$scenarioFSP)
          # data frame with Base scenario and the selected year
          dat2 <- subset(flt, year == input$baseFSP2 & fleet %in% input$fleetFSP & 
                           indicator %in% input$indicatorFSP & scenario == input$baseFSP3)
          
          dat1 <- dat1 %>% group_by(fleet, indicator, scenario)
          dat2 <- dat2 %>% group_by(fleet, indicator)
          
          dat2 <- dat2[, c('fleet', 'indicator', 'q50')]
          names(dat2)[3] <- 'q502' 
        }
        
        dat <- dat1 %>% left_join(dat2)
        
        dat <- dat %>% mutate(Ratio = (q50-q502)/q502)
        
        dat <- dat[order(dat$scenario), ]
        
        dat
      })
      
      
      plotFSpider <-function(){
        
        dt <- dataFSP()
        
        dt0 <- dt
        dt0$Ratio <- 0
        
        lines <- input$GrpPanFSP
        
        if(lines == 'fleet'){
          p <-  ggplot(data=dataFSP(), aes(x=scenario, y=Ratio, col=fleet, fill=fleet, group=fleet))+
            # geom_polygon(alpha=0.2, lwd=1)+
            geom_polygon(fill=NA, lwd=1)+
            geom_point(cex=1.5)+
            geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
            coord_radar()+
            theme_bw()+
            theme(text=element_text(size=14),
                  strip.text=element_text(size=14),
                  title=element_text(size=18,face="bold"))+
            ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
            facet_wrap(indicator~., ncol = input$nColFSP)
          
        }
        else{ #lines == indicator
          p <-  ggplot(data=dataFSP(), aes(x=scenario, y=Ratio, col=indicator, fill=indicator, group=indicator))+
            # geom_polygon(alpha=0.2, lwd=1)+
            geom_polygon(fill=NA, lwd=1)+
            geom_point(cex=1.5)+
            geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
            #         geom_hline(aes(yintercept=0), lwd=1, lty=2) +
            coord_radar()+
            theme_bw()+
            theme(text=element_text(size=14),
                  strip.text=element_text(size=14),
                  title=element_text(size=18,face="bold"))+
            ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
            facet_wrap(fleet~.,  ncol = input$nColFSP)
        }
        return(p)
        
      }
      
      output$plotFSPs <- renderPlot({
        #   browser()
        if (is.null(dataFSP())) return()
        plotFSpider()
      })
      
      # Case dependent plot size.
      values <- reactiveValues()
      plot_height <- function() {
        lines <- input$GrpPanFSP
        if(lines == 'stock'){ facets <- input$indicatorFSP} 
        else{ facets <- input$fleetFSP}
        
        # calculate values$facetCount
        values$facetsRows <- ifelse(length(facets) <=2, 400, ifelse(length(facets) <= 4, 600, 
                                                                    ifelse(length(facets) <= 6, 800, 250*ceiling(length(facets)/2))))
        return(values$facetsRows)
      }
      plot_width <- function() {
        lines <- input$GrpPanFSP
        if(lines == 'fleet'){ facets <- input$indicatorFSP} 
        else{ facets <- input$fleetFSP}
        # calculate values$facetCount
        values$facetsCols <- ifelse(length(input$fleetFSP)==1, 600, 900)
        return(values$facetsCols)
      }
      
      
      # wrap plotOutput in renderUI
      output$plotFSP<- renderUI({
        plotOutput("plotFSPs", height = plot_height(), width =plot_width())
      })
      
      
      # Code to download the plot
      getWSP <- function(){
        return(input$fileWFSP)
      }
      
      getHSP <- function(){
        return(input$fileHFSP)
      }
      
      getScSP <- function(){
        return(input$fileScFSP)
      }
      
      # Download the plot
      output$downFSP <- downloadHandler(
        filename =  function() {
          paste(input$filenmFSP, input$fileTypeFSP, sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          ggsave(file, plotFSP(), width = getWSP(), height = getHSP(), units = 'cm', scale = getScSP())
        } 
      )
      
      
    print('six spider')
    
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
#### PAGE_simulation METIER_Times series  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
  # PlotHeight_mt <- reactive({
  #   
  #   nids <- length(input$metierM)
  #   
  #   return(300*nids)})
  # 
  
  observe ({
    
    updateSelectInput(session, inputId =  "metierM", 
                      # label = h4("Stock"), 
                      choices =   unique(mt[mt$fleet %in% input$fleetM, 'metier']), 
                      selected =  unique(mt[mt$fleet %in% input$fleetM, 'metier'])[1])#, server = TRUE)#,
  }) 
  
  observe ({
    dataM<-reactive({
      req(input$metierM)
      mt[mt$year>=input$rangeM[1] & mt$year<=input$rangeM[2] & mt$fleet%in%input$fleetM & mt$metier%in%input$metierM
                                  & mt$scenario%in%input$scenarioM & mt$indicator%in%input$indicatorM,]
    })
    
    plotMetier <- function(){
        p <-ggplot(dataM(), aes(x=as.numeric(year), y=q50, color=scenario))+
                  geom_line(aes(color=scenario),lwd=1)+
                  ylab("")+xlab("Year")+
                  theme_bw()+
                  theme( strip.text=element_text(size=16),
                          title=element_text(size=16),
                        text=element_text(size=16))+
                  scale_x_continuous(limits = c(input$rangeM[1], input$rangeM[2]))
      
        if(!is.null(proj.yr)){
         p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year 
          
        }
        
        if(input$fitCIM == TRUE)
            p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        
        if(input$fitM==TRUE){
          p <- p + facet_wrap(metier ~ indicator, scale = 'free_y',  ncol = input$nColM)
        }
        else{
          p <- p + facet_grid(metier ~ indicator)
        }
        return(p)}
    
    
    output$plotMM<-renderPlot({
      print(plotMetier())}
      #, height = PlotHeight_mt
      )
    
    # Code to download the plot
    getMW <- function(){
      return(input$fileWM)
    }
    
    getMH <- function(){
      return(input$fileHM)
    }
    
    getMS <- function(){
      return(input$fileScM)
    }
    
    # Download the plot
    output$downM <- downloadHandler(
      filename =  function() {
        paste(input$filenmM, input$fileTypeM, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotMetier(), width = getMW(), height = getMH(), units = 'cm', scale = getMS())
      } 
    )
    
  })#end of the observer
  
  print('seven')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation FLEET BY_Times series  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
  # print('caracola06')      
  # PlotHeight_Fby <- reactive({
  #   
  #   nids <- length(input$fleetFby)*length(input$stockFby)
  #   
  #   return(300*nids)})
  
  observe ({
    
    updateSelectInput(session, inputId  = "stockFby", 
                               choices  = unique(subset(fltStk, fleet %in% input$fleetFby)$stock), 
                               selected = unique(subset(fltStk, fleet %in% input$fleetFby)$stock)[1]) #, server = TRUE)#,
    })    

  observe ({
      dataFby<-reactive({
          fltStk <- subset(fltStk, year %in% input$rangeFby[1]:input$rangeFby[2] &
                                   fleet %in% input$fleetFby & stock %in% input$stockFby &
                                   indicator %in% input$indicatorFby & scenario %in% input$scenarioFby)
          })
      
   #   browser()

      # print('caracola061')  

      
      plotFleetby <- function(){
            
        p <- ggplotFby<-ggplot(dataFby(), aes(x=as.numeric(year), y=q50, color=scenario))+
                geom_line(aes(color=scenario),size = input$lwdFby)+
                ylab("")+
                xlab("Year")+
                theme_bw()+
                theme( strip.text=element_text(size=16),
                      title=element_text(size=16),
                      text=element_text(size=16))
        
        if(input$fitCIFby == TRUE){
          p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        }
        
        if(input$dotLineFby == TRUE) p <- p +  geom_point(data = dataFby(), aes(x=year, y=q50, color=scenario), size = input$dszFby)
        
        if(!is.null(proj.yr)){
          p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year 
          
        }
        
         if(input$fitFby == FALSE){
           p <- p + facet_grid(fleet*stock ~ indicator)
         }
         else{
           p <- p + facet_wrap(fleet*stock ~ indicator, ncol = input$nColFby, scales="free_y")
         }
        return(p)}
      
      
      output$plotFby <-renderPlot({
        print(plotFleetby())
      }#, height = PlotHeight_Fby
      )
      
      # Code to download the plot
      getFbyW <- function(){
        return(input$fileWFby)
      }
      
      getFbyH <- function(){
        return(input$fileHFby)
      }
      
      getFbyS <- function(){
        return(input$fileScFby)
      }
      
      # Download the plot
      output$downFby <- downloadHandler(
        filename =  function() {
          paste(input$filenmFby, input$fileTypeFby, sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          ggsave(file, plotFleetby(), width = getFbyW(), height = getFbyH(), units = 'cm', scale = getFbyS())
        } 
      )
})
  print('eight')

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation FLEET-STOCK AREA  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

  # Make choices and selection in 'stockFby' dependent on the context of fleetbyA
  observe ({
    updateSelectInput(session, inputId  = "stockbyA", 
                      choices  = unique(subset(fltStk, fleet %in% input$fleetbyA)$stock), 
                      selected = unique(subset(fltStk, fleet %in% input$fleetbyA)$stock)) #, server = TRUE)#,
  })  
  
  
  observe ({
    databyA<-reactive({
      fltStk <- subset(fltStk, year %in% input$rangebyA[1]:input$rangebyA[2] &
                         fleet %in% input$fleetbyA & stock %in% input$stockbyA &
                         indicator %in% input$indicatorbyA & scenario %in% input$scenariobyA)

      if(input$percbyA == TRUE){
        fltStk <- fltStk  %>% ungroup() %>%  
          group_by(year, scenario, indicator, fleet) %>%
          mutate(p = q50/sum(q50)) %>% mutate(q50=p)
      }
      return(fltStk)
    })
    
    plotFleetStockArea <- function(){
      
      p <-ggplot(data = databyA())+
        geom_area(aes(x=year, y=q50, fill=stock), size=0.5, colour="grey") +
        ylab("")+xlab("Year")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16))
      
      if(!is.null(proj.yr)){
        p <- p +  geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year
      }
      
      if(input$fitbyA == FALSE){
        p <- p + facet_grid(fleet*scenario~indicator)
      }
      else{
        p <- p + facet_wrap(fleet*scenario~indicator, ncol = input$nColbyA, scale = 'free_y')
      }
      
    }
    
    
    output$plotFSbyA <-renderPlot({
      
      print(plotFleetStockArea())
    } #, height = PlotHeight_stk
    )
    
    
    # Code to download the plot
    getWbyA <- function(){
      return(input$fileWbyA)
    }
    
    getHbyA <- function(){
      return(input$fileHbyA)
    }
    
    getSbyA <- function(){
      return(input$fileScbyA)
    }
    
    # Download the plot
    output$downbyA <- downloadHandler(
      filename =  function() {
        paste(input$filenmbyA, input$fileTypebyA, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotFleetStockArea(), width = getWbyA(), height = getHbyA(), units = 'cm', scale = getSbyA())
      }
    )
    
  })# end of the observe stock
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #### PAGE_simulation FLEET_STOCK_Spider plot  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  observe ({
    updateSelectInput(session, inputId  = "stockFSPby", 
                      choices  = unique(subset(fltStk, fleet %in% input$fleetFSPby)$stock), 
                      selected = unique(subset(fltStk, fleet %in% input$fleetFSPby)$stock)) #, server = TRUE)#,
  })  
  
  dataFSPby<-reactive({
    req(input$baseFSPby)
    
    if (input$baseFSPby == "radio1Fby"){
      req(input$fleetFSPby)
      dat1 <- subset(fltStk, year == input$baseFSPby2 & fleet %in% input$fleetFSPby & stock %in% input$stockFSPby & 
                       indicator %in% input$indicatorFSPby & scenario%in%input$scenarioFSPby)
      dat2 <- subset(fltStk, year == input$baseFSPby1 & fleet %in% input$fleetFSPby & stock %in% input$stockFSPby & 
                       indicator %in% input$indicatorFSPby & scenario%in%input$scenarioFSPby)
      
      dat1 <- dat1 %>% group_by(fleet, indicator, scenario, stock)
      dat2 <- dat2 %>% group_by(fleet, indicator, scenario, stock)
      
      dat2 <- dat2[, c('scenario', 'fleet', 'stock', 'indicator', 'q50')]
      names(dat2)[5] <- 'q502' 
    }
    
    
    if (input$baseFSPby == "radio2Fby"){ # base1 is scenario and base2 is year
      req(input$fleetFSPby)
      # data frame with all the scenarios and the selected year
      dat1 <- subset(fltStk, year == input$baseFSPby4 & fleet %in% input$fleetFSPby & stock %in% input$stockFSPby & 
                       indicator %in% input$indicatorFSPby & scenario%in%input$scenarioFSPby)
      # data frame with Base scenario and the selected year
      dat2 <- subset(fltStk, year == input$baseFSPby2 & fleet %in% input$fleetFSPby & stock %in% input$stockFSPby & 
                       indicator %in% input$indicatorFSPby & scenario == input$baseFSPby3)
      
      dat1 <- dat1 %>% group_by(fleet, indicator, scenario, stock)
      dat2 <- dat2 %>% group_by(fleet, indicator, stock)
      
      dat2 <- dat2[, c('fleet', 'stock', 'indicator', 'q50')]
      names(dat2)[4] <- 'q502' 
    }
    
    dat <- dat1 %>% left_join(dat2)
    
    dat <- dat %>% mutate(Ratio = (q50-q502)/q502)
    
    dat <- dat[order(dat$scenario), ]
    
    dat
  })
  
  
  plotFbySpider <-function(){
    
    dt <- dataFSPby()
    
    dt0 <- dt
    dt0$Ratio <- 0
    
    lines <- input$GrpPanFSPby
    
    a <- dataFSPby()
    
    print(dt0)
    
    if(lines == 'fleet'){
      p <-  ggplot(data=dataFSPby(), aes(x=scenario, y=Ratio, col=fleet, fill=fleet, group=fleet))+
        # geom_polygon(alpha=0.2, lwd=1)+
        geom_polygon(fill=NA, lwd=1)+
        geom_point(cex=1.5)+
        geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
        coord_radar()+
        theme_bw()+
        theme(text=element_text(size=14),
              strip.text=element_text(size=14),
              title=element_text(size=18,face="bold"))+
        ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
        facet_wrap(indicator~stock, ncol = input$nColFSPby)
      
    }
    else{ #lines == indicator
      if(lines == 'stock'){
          p <-  ggplot(data=dataFSPby(), aes(x=scenario, y=Ratio, col=stock, fill=stock, group=stock))+
          # geom_polygon(alpha=0.2, lwd=1)+
            geom_polygon(fill=NA, lwd=1)+
            geom_point(cex=1.5)+
            geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
            #         geom_hline(aes(yintercept=0), lwd=1, lty=2) +
            coord_radar()+
            theme_bw()+
            theme(text=element_text(size=14),
                strip.text=element_text(size=14),
                title=element_text(size=18,face="bold"))+
            ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
          facet_wrap(fleet~indicator,  ncol = input$nColFSPby)
      }
      else{ # lines indicator
        p <-  ggplot(data=dataFSPby(), aes(x=scenario, y=Ratio, col=indicator, fill=indicator, group=indicator))+
          # geom_polygon(alpha=0.2, lwd=1)+
          geom_polygon(fill=NA, lwd=1)+
          geom_point(cex=1.5)+
          geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
          #         geom_hline(aes(yintercept=0), lwd=1, lty=2) +
          coord_radar()+
          theme_bw()+
          theme(text=element_text(size=14),
                strip.text=element_text(size=14),
                title=element_text(size=18,face="bold"))+
          ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
          facet_wrap(fleet~stock,  ncol = input$nColFSPby)
      }
    }
    return(p)
    
  }
  
  output$plotFSPbys <- renderPlot({
    #   browser()
    if (is.null(dataFSPby())) return()
    plotFbySpider()
  })
  
  # Case dependent plot size.
  values <- reactiveValues()
  plot_height <- function() {
    lines <- input$GrpPanFSPby
    if(lines == 'stock'){ facets <- length(input$indicatorFSPby)*length(input$fleetFSPby)} 
    else{ 
      if(lines == 'fleet'){facets <- length(input$indicatorFSPby)*length(input$stockFSPby)} 
      else{facets <- length(input$fleetFSPby)*length(input$stockFSPby)}
    }
    
    # calculate values$facetCount
    values$facetsRows <- ifelse(facets/input$nColFSPby == 1, 600, ifelse(facets/input$nColFSPby == 2, 800, 
                                                                ifelse(facets/input$nColFSPby == 3, 100, 300*ceiling(facets/input$nColFSPby))))
    return(values$facetsRows)
  }
  
  plot_width <- function() {
    
    lines <- input$GrpPanFSPby
    if(lines == 'stock'){ facets <- length(input$indicatorFSPby)*length(input$fleetFSPby)} 
    else{ 
      if(lines == 'fleet'){facets <- length(input$indicatorFSPby)*length(input$stockFSPby)} 
      else{facets <- length(input$fleetFSPby)*length(input$stockFSPby)}
    }
    

    values$facetsCols <- ifelse(input$nColFSPby == 1, 600, ifelse(input$nColFSPby <= 2, 900, 1200))
    return(values$facetsCols)
  }
  
  
  # wrap plotOutput in renderUI
  output$plotFSPby<- renderUI({
    plotOutput("plotFSPbys", height = plot_height(), width =plot_width())
  })
  
  
  # Code to download the plot
  getWSP <- function(){
    return(input$fileWFSPby)
  }
  
  getHSP <- function(){
    return(input$fileHFSPby)
  }
  
  getScSP <- function(){
    return(input$fileScFSPby)
  }
  
  # Download the plot
  output$downFSPby <- downloadHandler(
    filename =  function() {
      paste(input$filenmFSPby, input$fileTypeFSPby, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, plotFSPby(), width = getWSP(), height = getHSP(), units = 'cm', scale = getScSP())
    } 
  )
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation METIER AREA  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

  observe ({
    updateSelectInput(session, inputId  = "metierMA", 
                      choices  = unique(subset(mt, fleet %in% input$fleetMA)$metier), 
                      selected = unique(subset(mt, fleet %in% input$fleetMA)$metier)) #, server = TRUE)#,
  })  
  
  
  observe ({
    dataMA<-reactive({
      mt <- subset(mt, year %in% input$rangeMA[1]:input$rangeMA[2] &
                         fleet %in% input$fleetMA & metier %in% input$metierMA &
                         indicator %in% input$indicatorMA & scenario %in% input$scenarioMA)
      
      if(input$percMA == TRUE){
        mt <- mt  %>% ungroup() %>%  
          group_by(year, scenario, indicator, fleet) %>%
          mutate(p = q50/sum(q50)) %>% mutate(q50=p)
      }
   
      
      return(mt)
    })
    
    plotFleetMetierArea <- function(){
      
      
      p <-ggplot(data = dataMA())+
        geom_area(aes(x=year, y=q50, fill=metier), size=0.5, colour="grey") +
        ylab("")+xlab("Year")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16))
      
      if(!is.null(proj.yr)){
        p <- p +  geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year
      }
      
      if(input$fitMA == FALSE){
        p <- p + facet_grid(fleet*scenario~indicator)
      }
      else{
        p <- p + facet_wrap(fleet*scenario~indicator, ncol = input$nColMA, scale = 'free_y')
      }
      
    }
    
    
    output$plotFSMA <-renderPlot({
      
      print(plotFleetMetierArea())
    } #, height = PlotHeight_stk
    )
    
    
    # Code to download the plot
    getWMA <- function(){
      return(input$fileWMA)
    }
    
    getHMA <- function(){
      return(input$fileHMA)
    }
    
    getSMA <- function(){
      return(input$fileScMA)
    }
    
    # Download the plot
    output$downMA <- downloadHandler(
      filename =  function() {
        paste(input$filenmMA, input$fileTypeMA, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotFleetMetierArea(), width = getWMA(), height = getHMA(), units = 'cm', scale = getSMA())
      }
    )
    
  })# end of the observe metier
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation METIER BY_Times series  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  # 
  # PlotHeight_Mby <- reactive({
  #   
  #   nids <- length(input$fleetMby)*length(input$stockMby)
  #   
  #   return(300*nids)})
  
  
  observe ({
    
    updateSelectInput(session, inputId =  "metierMby", 
                      # label = h4("Stock"), 
                      choices =   unique(mtStk[mtStk$fleet %in% input$fleetMby, 'metier']), 
                      selected =  unique(mtStk[mtStk$fleet %in% input$fleetMby, 'metier'])[1])#, server = TRUE)#,
  })
  
  observe ({
    updateSelectInput(session, inputId =  "stockMby", 
                      # label = h4("Stock"), 
                      choices =   unique(mtStk[mtStk$metier %in% input$metierMby & mtStk$fleet %in% input$fleetMby, 'stock']), 
                      selected =  unique(mtStk[mtStk$metier %in% input$metierMby & mtStk$fleet %in% input$fleetMby, 'stock'])[1])
  }) 
  
  
  observe ({
      dataMby<-reactive({
      
      mtStk[mtStk$year>=input$rangeMby[1]           & mtStk$year<=input$rangeMby[2] & mtStk$stock %in% input$stockMby
          & mtStk$metier %in% input$metierMby       & mtStk$fleet %in% input$fleetMby
          & mtStk$indicator %in% input$indicatorMby & mtStk$scenario %in% input$scenarioMby,]
    })
    
 
    plotMetierby <- function(){
        p <- ggplot(dataMby(), aes(x=as.numeric(year), y=q50, color=scenario))+
                geom_line(aes(color=scenario),lwd=1)+
                ylab("")+
                xlab("Year")+
                theme_bw()+
                theme( strip.text=element_text(size=16),
                      title=element_text(size=16),
                      text=element_text(size=16))

        if (input$fitCIMby == TRUE){
          p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        } 
        if(input$fitMby==TRUE){
          p <- p + facet_wrap(metier*stock ~ indicator, ncol = input$nColMby, scales="free_y")
        }
        if(!is.null(proj.yr)){
          p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year 
          
        }
        else{
          p <- p + facet_grid(metier*stock ~ indicator)
        }
        return(p)
    }
    
    output$plotMby <- renderPlot({
             print(plotMetierby())
      }#, height = PlotHeight_Mby
      )
         
    
    # Code to download the plot
    getMbyW <- function(){
      return(input$fileWMby)
    }
    
    getMbyH <- function(){
      return(input$fileHMby)
    }
    
    getMbyS <- function(){
      return(input$fileScMby)
    }
    
    # Download the plot
    output$downMby <- downloadHandler(
      filename =  function() {
        paste(input$filenmMby, input$fileTypeMby, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotMetierby(), width = getMbyW(), height = getMbyH(), units = 'cm', scale = getMbyS())
      } 
    )
})
         
  print('nine')
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation FLEET-METIER-STOCK AREA  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
  
  # Make choices and selection in 'stockFby' dependent on the context of fleetMbyA
  observe ({
    updateSelectInput(session, inputId  = "metierMbyA", 
                      choices  = unique(subset(mtStk, fleet %in% input$fleetMbyA)$metier), 
                      selected = unique(subset(mtStk, fleet %in% input$fleetMbyA)$metier)) #, server = TRUE)#,
  })  
  
  observe ({
    updateSelectInput(session, inputId  = "stockMbyA", 
                      choices  = unique(subset(mtStk, fleet %in% input$fleetMbyA & metier %in% input$metierMbyA)$stock), 
                      selected = unique(subset(mtStk, fleet %in% input$fleetMbyA & metier %in% input$metierMbyA)$stock)) #, server = TRUE)#,
  })  
  
  observe ({
    dataMbyA<-reactive({
      mtStk <- subset(mtStk, year %in% input$rangeMbyA[1]:input$rangeMbyA[2] &
                             fleet %in% input$fleetMbyA & metier %in% input$metierMbyA & stock %in% input$stockMbyA &
                             indicator %in% input$indicatorMbyA & scenario %in% input$scenarioMbyA)
      
      if(input$percMbyA == TRUE){
        mtStk <- mtStk  %>% ungroup() %>%  
          group_by(year, scenario, indicator, fleet, metier) %>%
          mutate(p = q50/sum(q50)) %>% mutate(q50=p)
      }
      return(mtStk)
    })
    
    plotFleetStockArea <- function(){
      
      p <-ggplot(data = dataMbyA())+
        geom_area(aes(x=year, y=q50, fill=stock), size=0.5, colour="grey") +
        ylab("")+xlab("Year")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16))
      
      if(!is.null(proj.yr)){
        p <- p +  geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year
      }
      
      if(input$fitMbyA == FALSE){
        p <- p + facet_grid(metier*scenario~indicator)
      }
      else{
        p <- p + facet_wrap(meiter*scenario~indicator, ncol = input$nColMbyA, scale = 'free_y')
      }
      
    }
    
    
    output$plotFSMbyA <-renderPlot({
      
      print(plotFleetStockArea())
    } #, height = PlotHeight_stk
    )
    
    
    # Code to download the plot
    getWMbyA <- function(){
      return(input$fileWMbyA)
    }
    
    getHMbyA <- function(){
      return(input$fileHMbyA)
    }
    
    getSMbyA <- function(){
      return(input$fileScMbyA)
    }
    
    # Download the plot
    output$downMbyA <- downloadHandler(
      filename =  function() {
        paste(input$filenmMbyA, input$fileTypeMbyA, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotFleetStockArea(), width = getWMbyA(), height = getHMbyA(), units = 'cm', scale = getSMbyA())
      }
    )
    
  })# end of the observe stock
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation ADVICE_Times series  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
     PlotHeight_adv <- reactive({
       
       nids <- length(input$indicatorA)
       
       return(300*nids)})
     
         # print('caracola08')     
    observe ({
      dataA<-reactive({
        req(input$stockA)
        adv[adv$year>=input$rangeA[1]         & adv$year<=input$rangeA[2] & adv$stock%in%input$stockA
          & adv$indicator%in%input$indicatorA & adv$scenario%in%input$scenarioA,]})
      
      
      plotAdvice <- function(){
       p <-ggplot(dataA(), aes(x=as.numeric(year), y=q50, color=scenario))+
              geom_line(lwd=1)+
              ylab("")+ xlab("Year")+
              theme_bw()+
              theme( strip.text=element_text(size=16),
                      title=element_text(size=16),
                      text=element_text(size=16))
        
        if(!is.null(proj.yr)){
          p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year 
          
        }
        
        
        if (input$fitCIA == TRUE){
          p <- p +  geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        }
        if(input$fitA==TRUE){
          p <- p + facet_wrap(indicator~stock, scales="free_y",ncol = input$nColA)
        }
        else{
          p <- p + facet_grid(indicator~stock)
        }
        return(p)
        }
      
      
      output$plotA <- renderPlot({
        print(plotAdvice())
      }#, height = PlotHeight_adv
      )
      
      
      # Code to download the plot
      getAW <- function(){
        return(input$fileWA)
      }
      
      getAH <- function(){
        return(input$fileHA)
      }
      
      getAS <- function(){
        return(input$fileScA)
      }
      
      # Download the plot
      output$downA <- downloadHandler(
        filename =  function() {
          paste(input$filenmA, input$fileTypeA, sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          ggsave(file, plotAdvice(), width = getAW(), height = getAH(), units = 'cm', scale = getAS())
        } 
      )
        
      
    })# end of the observe advice
    
    print('ten')
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
    #### PAGE_simulation Summary_polar plots  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
    # 
    # PlotHeight_sum <- reactive({
    #   
    #   nids <- length(input$scenarioP)
    #   
    #   return(300*nids)})
    
    # print('caracola09')   
    #reactive: ssb and f
    st1 <- reactive({subset(bio, scenario %in% input$scenarioP & indicator %in% c("ssb", "f") & stock %in% input$stockP & year==input$yearP)[,c("stock","year","indicator", "scenario", "q50")]})
    st2 <- reactive({subset(bio, scenario %in% input$scenarioP & indicator %in% c("ssb", "f") & stock %in% input$stockP & year %in% input$rangeP)[,c("stock","year","indicator", "scenario", "q50")]})  
         
    #reactive: profits and capacity
    fl1 <- reactive({subset(flt, scenario %in% input$scenarioP & indicator %in% c("grossSurplus", "capacity") & fleet %in% input$fleetP & year==input$yearP)[,c("fleet","year","indicator", "scenario", "q50")]})
    fl2 <- reactive({subset(flt, scenario %in% input$scenarioP & indicator %in% c("grossSurplus", "capacity") & fleet %in% input$fleetP & year==input$yearP)[,c("fleet","year","indicator", "scenario", "q50")]})


    plotPolar <- function(){
      # New data entry
      dat.stpolar <- NULL
      dat.flpolar <- NULL
      
      st3 <- aggregate(q50 ~ stock + indicator + scenario, data=st2(), FUN=mean)
      fl3 <- aggregate(q50 ~ fleet + indicator + scenario, data=fl2(), FUN=mean)

      # cuadrante superior: 2 biological indicators by stock:
      st <- merge(st1(), st3, by=c("indicator","stock", "scenario"))
      st$ratio <- st$q50.y/st$q50.x
      st.dat <- st
      st.dat$stock <- paste("stock.",st.dat$stock,sep="")
      
      # cuadrante inferior: 2 economical indicators
      fl <- merge(fl1(), fl3, by=c("indicator","fleet", "scenario"), all.x=TRUE)
      fl$ratio <- fl$q50.y/fl$q50.x
      fl.dat <- fl
      fl.dat$fleet <- paste("fleet.",fl.dat$fleet,sep="")
      
      # number of stocks and fleets
      nst <- length(unique(st.dat$stock)) # number of stocks
      nfl <- length(unique(fl.dat$fleet))
      
      w <- scm(nst, nfl)
      wst <- w/nst
      wfl <- w/nfl
      
      # Index to plot them
      for(sc in input$scenarioP){
        st.dat[st.dat$scenario == sc, 'ind'] <- seq(0, wst*(length(st.dat[st.dat$scenario == sc, 'ratio'])-1), by=wst) + wst/2 
        fl.dat[fl.dat$scenario == sc, 'ind'] <- wst*length(st.dat[st.dat$scenario == sc, 'ratio']) + seq(0, wfl*(length(fl.dat[fl.dat$scenario == sc, 'ratio'])-1), by=wfl) + wfl/2
      }
      
      # save into a general case
      dat.stpolar <- rbind(dat.stpolar, st.dat)
      dat.flpolar <- rbind(dat.flpolar, fl.dat)
            
      # Palettes for fleet and stock (alphabetic order 1:fleet and 2:stock)
      # # save into a general case
      # dat.stpolar <- rbind(dat.stpolar, st.dat)
      # dat.flpolar <- rbind(dat.flpolar, fl.dat)
      
      # Palettes for fleet and stock (alphabetic order 1:fleet and 2:stock)
      # Add more tones to this palette :
      palfl <- pals::glasbey()[length(glasbey()):1][1:nfl]
      palst <- pals::glasbey()[1:nfl]
      
      pal <- c(palfl, palst) # it will sort the categories in alphabetic order

      ymax <- max(c(dat.stpolar$ratio, dat.flpolar$ratio))*(1+sqrt(5))/2
  
      # The number of 
      #    # print('caracola22')    
      # Polar plot (ggplot)
      p <- ggplot(dat.stpolar, aes(x=ind, y=ratio))+
        geom_bar(data=dat.stpolar, aes(fill=stock), stat="identity", position="dodge", width=wst)+
        geom_bar(data=dat.flpolar, aes(x=ind, y=ratio, fill=fleet), stat="identity", position="dodge", width=wfl)+
        scale_fill_manual(values = pal)+
        theme_bw()+
        facet_wrap(scenario~., ncol = input$nColP)+
        coord_polar(start=-pi/2)+
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.line.x = element_blank(),
              text=element_text(size=16),
              title=element_text(size=16,face="bold"),
              strip.text=element_text(size=16))+
        geom_hline(aes(yintercept=1))+
        geom_vline(aes(xintercept=0), lwd=1)+
        geom_vline(aes(xintercept=wst*nst), lwd=1)+
        geom_vline(aes(xintercept=wst*nst+wst*nst), lwd=1)+
        geom_vline(aes(xintercept=wst*nst+wst*nst+wfl*nfl), lwd=1)+
        xlim(c(0,4*w))+
        annotate(geom="text",x=w/2, y=ymax, label=c("SSB"), size=6)+
        annotate(geom="text",x=w*3/2, y=ymax, label=c("F"), size=6)+
        annotate(geom="text",x=w*5/2, y=ymax, label=c("Capacity"), size=6)+
        annotate(geom="text",x=w*7/2, y=ymax, label=c("Gross-Surplus"), size=6)+
        labs(fill="")+
        geom_text(aes(x=1, y = min(dat.flpolar$ratio),label = sum(npv$q50)))
      
      return(p)
      
    }
    
    output$plotP <- renderPlot({
      # browser()
        print(plotPolar())
    }#, height = PlotHeight_sum
    )
    
    
    # Case dependent plot size.
    values <- reactiveValues()
    plot_height <- function() {
      
      facets <- length(input$scenarioP)
      
      values$facetsRows <- ifelse(facets/input$nColP == 1, 600, ifelse(facets/input$nColP == 2, 800, 
                                                                           ifelse(facets/input$nColP == 3, 100, 300*ceiling(facets/input$nColP))))
      return(values$facetsRows)
    }
    
    plot_width <- function() {

      values$facetsCols <- ifelse(input$nColP == 1, 600, ifelse(input$nColP <= 2, 900, 1200))
      return(values$facetsCols)
    }
    
    
    output$plotPs<-renderPlot({
      
      print(plotPolar())
    } #, height = PlotHeight_stk
    )
    
    # wrap plotOutput in renderUI
    output$plotP <- renderUI({
      plotOutput("plotPs", height = plot_height(), width = plot_width())
    })
    
    
    
    
    # Code to download the plot
    getPW <- function(){
      return(input$fileWP)
    }
    
    getPH <- function(){
      return(input$fileHP)
    }
    
    getPS <- function(){
      return(input$fileScP)
    }
    
    # Download the plot
    output$downP <- downloadHandler(
      filename =  function() {
        paste(input$filenmP, input$fileTypeP, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotPolar(), width = getPW(), height = getPH(), units = 'cm', scale = getPS())
      } 
    )
    
} #end of the server