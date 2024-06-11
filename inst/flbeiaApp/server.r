
# Change the size of the plot area: https://groups.google.com/g/shiny-discuss/c/dkZxTvfHOvo?pli=1

source ("global.R") # radar plot function

server <- function(input, output, session){
  

  observe({

      # By default stock and advice are always there.
      
      # Version fleet => default + fleet ones  + summary
      # Version Metier => fleet + metier
      
      if (version == 'all'){ 
        
        #shinyjs::show(id = "Fleets")
        showTab(inputId = "tabs", target = "Stocks")
        showTab(inputId = "tabs", target = "Advice")
        showTab(inputId = "tabs", target = "Fleets")
        showTab(inputId = "tabs", target = "Fleets and stocks")
        showTab(inputId = "tabs", target = "Fleets and metiers")
        showTab(inputId = "tabs", target = "Fleets, metiers and stocks")
        showTab(inputId = "tabs", target = "Summary")
        
      }else {
        
        if(version == 'stock'){
          showTab(inputId = "tabs", target = "Stocks")
          showTab(inputId = "tabs", target = "Advice")
          hideTab(inputId = "tabs", target = "Fleets")
          hideTab(inputId = "tabs", target = "Fleets and stocks")
          hideTab(inputId = "tabs", target = "Summary")
          hideTab(inputId = "tabs", target = "Fleets and metiers")
          hideTab(inputId = "tabs", target = "Fleets, metiers and stocks")
        }
        else{
          showTab(inputId = "tabs", target = "Stocks")
          showTab(inputId = "tabs", target = "Advice")
          showTab(inputId = "tabs", target = "Fleets")
          showTab(inputId = "tabs", target = "Fleets and stocks")
          showTab(inputId = "tabs", target = "Summary")
          hideTab(inputId = "tabs", target = "Fleets and metiers")
          hideTab(inputId = "tabs", target = "Fleets, metiers and stocks")
        }
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
      
    plotStock <- function(){
      
      p <-ggplot()+
        geom_line(data = dataS(), aes(x=year, y=q50, color=scenario), size = input$lwdS) +
        ylab("")+xlab("Year")+
        theme_bw()+
        theme( strip.text=element_text(size=16),
               title=element_text(size=16),
               text=element_text(size=16), legend.position="top")
      
      # Iteraction
       if(!is.null(input$iterS)){
         p <- p + geom_line(data = dataSI(), aes(x=year, y=value, group = interaction(scenario, iter), color = scenario,  linetype = iter), lwd=1)+
           scale_linetype_manual(values = c(2:6))
       }

      if(!is.null(proj.yr)){
        p <- p +  geom_vline(data = dataS(), aes(xintercept=proj.yr), color="grey", linetype="dashed", lwd=1) # projection starting year 
      }
      
      if(input$dotLineS == TRUE) p <- p +  geom_point(data = dataS(), aes(x=year, y=q50, color=scenario), size = input$dszS)
      
      # Refence points
        if (input$refpointS == TRUE ){
          validate (
            need(nrow(datarpS())>0, "Please check if reference points are loaded or adequate indicator selected"))
          #p <- p +geom_hline(data = datarpS(), aes(yintercept=value), color="red", linetype="dotted", lwd =1)
          p <- p + geom_hline(data = datarpS(), aes(yintercept=value, group = interaction(scenario, refpt_type),  color=scenario, linetype=refpt_type), lwd=1)+
            scale_linetype_manual(values = c(2:4))
          #! MK: decide cambiar lineas tipo por colores de lineas según: 
            # MSY = BERDEA
            # LIM = GORRIA
            # PA = BELTZA
            # group.colors <- c(MSY = "#00ff", LIM = "#ff0000", PA ="#0000ff")
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
      
      cond1   <- any(sapply(c('pFlim','pFpa','pFtarget', 'pBlim','pBpa','pBtarget'), function(x) any(grepl(x, input$indicatorS))))
      cond2   <- any(sapply(c('ssb2Btarget', 'f2Ftarget'), function(x) any(grepl(x, input$indicatorS))))
        
      if(cond1){  
        p <-  p + geom_hline(data= dataS() %>% filter(indicator %in% c('pFlim','pFpa','pFtarget','pBlim','pBpa','pBtarget')),
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
    values <- reactiveValues()
    
    plot_height <- function() {
      
      if(input$fitS==FALSE){nrow <- length(input$stockS)}
      else{nrow <- ceiling(length(input$stockS)*length(input$indicatorS)/input$nColS)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      if(input$fitS==FALSE){ncol <- length(input$indicatorS)}
      else{ncol <- input$nColS}
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotSs<-renderPlot({print(plotStock())})
    
    # wrap plotOutput in renderUI
    output$plotS <- renderUI({plotOutput("plotSs", height = plot_height(), width = plot_width())})
    
    # Code to download the plot
    getW <- function(){return(input$fileWS)}
    getH <- function(){return(input$fileHS)}
    getS <- function(){return(input$fileScS)}
    
    # Download the plot
    output$downS <- downloadHandler(
      filename =  function() {
        paste(input$filenmS, input$fileTypeS, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotStock(), width = getW(), height = getH(), units = 'cm', scale = getS())
        } 
    )

    
  })# end of the observe stock 


  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation STOCK AREA  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
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
               text=element_text(size=16),
               legend.position="top")

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

    values <- reactiveValues()
    
    # Case dependent plot size.
    plot_height <- function() {
      
      nSc <- length(input$scenarioSA)
      nI <- length(input$indicatorSA)

      if(input$fitSA==FALSE){nrow <- nSc}
      else{nrow <- ceiling(nSc*nI/input$nColSA)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      nSc <- length(input$scenarioSA)
      nI <- length(input$indicatorSA)
      
      if(input$fitSA==FALSE){ncol <- nI}
      else{ncol <- min(c(input$nColSA, nSc*nI))}
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotSAs <-renderPlot({print(plotStockArea())})
    
    # wrap plotOutput in renderUI
    output$plotSA <- renderUI({plotOutput("plotSAs", height = plot_height(), width = plot_width())})
    

    # Code to download the plot
    getWSA <- function(){return(input$fileWSA)}
    getHSA <- function(){return(input$fileHSA)}
    getSSA <- function(){return(input$fileScSA)}

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
            strip.text=element_text(size=16),
            legend.position="top") #+
  #    labs(caption = 'First year = black dot & Final year = black cross')
  }


  # Case dependent plot size.
  values <- reactiveValues()

  plot_height <- function() {

    nrow <- ceiling(length(input$stockK)/input$nColK)
    values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
    return(values$height)
  }
  plot_width <- function() {

    ncol <- min(input$nColK, length(input$stockK))
    values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))

    return(values$width)
  }

  # wrap plotOutput in renderUI
  output$plotKs <- renderPlot({
    if (is.null(dataK())) return()
    plotKobe()})

  output$plotK <- renderUI({plotOutput("plotKs", height = plot_height(), width = plot_width())})

  # Code to download the plot
  getWSK <- function(){return(input$fileWSK)}
  getHSK <- function(){return(input$fileHSK)}
  getSSK <- function(){return(input$fileScSK)}

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
               title=element_text(size=18,face="bold"),
               legend.position="top")+
         ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
          facet_wrap(indicator~., ncol = input$nColSP)

        }
       else{ #lines == indicator
         p <-  ggplot(data=dataSP(), aes(x=scenario, y=Ratio, col=indicator, fill=indicator, group=indicator))+
           geom_polygon(fill=NA, lwd=1)+
           geom_point(cex=1.5)+
           geom_path(data = dt0, aes(x=scenario, y=Ratio), colour = 'black', linetype = 'dashed', size = 1)+
           coord_radar()+
           theme_bw()+
           theme(text=element_text(size=14),
                 strip.text=element_text(size=14),
                 title=element_text(size=18,face="bold"),
                 legend.position="top")+
           ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
           facet_wrap(stock~.,  ncol = input$nColSP)
       }
    }


  # Case dependent plot size.
  # values <- reactiveValues()
  # 
  # plot_height <- function() {
  #   
  #   lines <- input$GrpPanSP
  #   
  #   if(lines == 'stock'){nrow <-  ceiling(length(input$indicatorSP)/input$nColSP)}
  #   else{                nrow <-  ceiling(length(input$stockSP)/input$nColSP)} 
  # 
  #   if(nrow  == 1){values$height <- 600}
  #   else{           values$height <- 300*nrow} 
  #   
  # 
  #   return(values$height)}
  # 
  # plot_width <- function() {
  #   ncol         <- input$nColSP
  #   values$width <- ifelse(ncol == 1, 400, 800)
  #   return(values$width)}
  
  output$plotSPs <- renderPlot({print(plotSpider())})
  
  # wrap plotOutput in renderUI
  output$plotSP<- renderUI({plotOutput("plotSPs",height = 700, width = 1200)})

  
  # Code to download the plot
  getWSP  <- function(){return(input$fileWSP)}
  getHSP  <- function(){return(input$fileHSP)}
  getScSP <- function(){return(input$fileScSP)}


  # Download the plot
  output$downSP <- downloadHandler(
    filename =  function() {
      paste(input$filenmSP, input$fileTypeSP, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, plotSpider()) #,  width = getWSP(), height = getHSP(), units = 'cm')#, scale = getScSP())
    }
  )

  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
#### PAGE_simulation FLEET  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
  #### PAGE_simulation FLEET_TIMES SERIES  ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

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
                geom_line(data= dataF(), aes(x=year, y=q50, color=scenario),size = input$lwdF)+
                ylab("")+ xlab("Year")+
                theme_bw()+
                theme( strip.text=element_text(size=16),
                        title=element_text(size=16),
                        text=element_text(size=16),
                       legend.position="top")+
                scale_x_continuous(limits = c(input$rangeF[1], input$rangeF[2]))
      
      # Iteraction
      if(!is.null(input$iterF)){
        p <- p + geom_line(data = dataFI(), aes(x=year, y=q50, group = interaction(scenario, iter), color = scenario,  linetype = iter), lwd=1)+
          scale_linetype_manual(values = c(2:6))
      }
      
      if(!is.null(proj.yr)){p <- p + geom_vline(data=dataF(), aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1)} # projection starting year 
      
      if(input$dotLineF == TRUE) p <- p +  geom_point(data = dataF(), aes(x=year, y=q50, color=scenario), size = input$dszF)
      
      # With Conf Int.
      if (input$fitCIF == TRUE){p <- p + geom_ribbon(data = dataF(),  aes(x=year, ymin=q05, ymax=q95,fill = scenario), alpha=0.3)}#+
               
      if(input$fitF==TRUE){p <- p + facet_wrap(fleet ~ indicator, ncol = input$nColF, scales="free_y")}
      else{p <- p + facet_grid(fleet ~ indicator)}
      
      return(p)
      }
    
    values <- reactiveValues()
    
    plot_height <- function() {
      
      if(input$fitF==FALSE){nrow <- length(input$fleetF)}
      else{nrow <- ceiling(length(input$fleetF)*length(input$indicatorF)/input$nColF)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      if(input$fitF==FALSE){ncol <- length(input$indicatorF)}
      else{ncol <- input$nColF}
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotFs<-renderPlot({print(plotFleet())})
    
    # wrap plotOutput in renderUI
    output$plotF <- renderUI({plotOutput("plotFs", height = plot_height(), width = plot_width())})
    
    # Code to download the plot
    getFW <- function(){return(input$fileWF)}
    getFH <- function(){return(input$fileHF)}
    getFS <- function(){return(input$fileScF)}
    
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


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
    #### PAGE_simulation FLEET_NPV  ####
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
  
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
              axis.ticks.x=element_blank(),
              legend.position="top")+
        ylab("NPV")
    }
    
    output$plotFN<-renderPlot({ plotNPV() })
      
      # Code to download the plot
      getFNW <- function(){return(input$fileWFN)}
      getFNH <- function(){return(input$fileHFN)}
      getFNS <- function(){return(input$fileScFN)}
      
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
                  title=element_text(size=18,face="bold"), 
                  legend.position="top")+
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
                  title=element_text(size=18,face="bold"), legend.position="top")+
            ylab("") +#ylim(c(0,max(c(1,dt$Ratio)))) +
            facet_wrap(fleet~.,  ncol = input$nColFSP)
        }
      }
      
      output$plotFSPs <- renderPlot({print(plotFSpider())})
      
      # Case dependent plot size.
      values <- reactiveValues()
      plot_height <- function() {
        
        if(input$GrpPanFSP=='fleet'){nrow <- ceiling(length(input$indicatorFSP)/input$nColFSP)}
                                else{nrow <- ceiling(length(input$fleetFSP)/input$nColFSP)}
        values$height <- ifelse(nrow == 1, 600, ifelse(nrow == 2, 1000, ifelse(nrow == 3, 1200, 350*nrow)))
        print(height)
        return(values$height)
      }
      
      # plot_width <- function() {
      #   ncol <- input$nColFSP
      #   values$width <- ifelse(ncol == 1, 600, 1200)
      # 
      #   return(values$width)
      # }
      # 
      # wrap plotOutput in renderUI
      output$plotFSP<- renderUI({plotOutput("plotFSPs", height = 700, width = 1200)})
      
      # Code to download the plot
      getWSP <- function(){return(input$fileWFSP)}
      getScSP <- function(){return(input$fileScFSP)}
      
      # Download the plot
      output$downFSP <- downloadHandler(filename =  function() {paste(input$filenmFSP, input$fileTypeFSP, sep=".")},
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          ggsave(file, plotFSpider())#, width = getWSP(), units = 'cm')#, scale = getScSP())
        } 
      )

    
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
#### PAGE_simulation METIER_Times series  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
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
        p <-ggplot(dataM(), aes(x=as.numeric(year), y=q50, color=scenario), size = input$lwdM)+
                  geom_line(aes(color=scenario),lwd=1)+
                  ylab("")+xlab("Year")+
                  theme_bw()+
                  theme( strip.text=element_text(size=16),
                          title=element_text(size=16),
                        text=element_text(size=16), legend.position="top")+
                  scale_x_continuous(limits = c(input$rangeM[1], input$rangeM[2]))
      
        if(!is.null(proj.yr)){
         p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year 
          
        }
        
        if(input$dotLineM == TRUE) p <- p +  geom_point(data = dataM(), aes(x=year, y=q50, color=scenario), size = input$dszM)
        
        if(input$fitCIM == TRUE)   p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        
        if(input$fitM==TRUE){p <- p + facet_wrap(metier ~ indicator, scale = 'free_y',  ncol = input$nColM)}
        else{p <- p + facet_grid(metier ~ indicator)}
        return(p)}
    
    values <- reactiveValues()
    
    plot_height <- function() {
      
      if(input$fitM==FALSE){nrow <- length(input$metierM)}
      else{nrow <- ceiling(length(input$metierM)*length(input$indicatorM)/input$nColM)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      if(input$fitM==FALSE){ncol <- length(input$indicatorM)}
      else{ncol <- min(c(input$nColM, length(input$indicatorM)*length(input$metierM)))}
      
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotMMs<-renderPlot({print(plotMetier())})
    
    # wrap plotOutput in renderUI
    output$plotMM <- renderUI({plotOutput("plotMMs", height = plot_height(), width = plot_width())})
    
    
    # Code to download the plot
    getMW <- function(){return(input$fileWM)}
    getMH <- function(){return(input$fileHM)}
    getMS <- function(){return(input$fileScM)}
    
    # Download the plot
    output$downM <- downloadHandler(filename =  function() {paste(input$filenmM, input$fileTypeM, sep=".")},
             # content is a function with argument file. content writes the plot to the device
            content = function(file) {
            ggsave(file, plotMetier(), width = getMW(), height = getMH(), units = 'cm', scale = getMS())
      })
    
  })#end of the observer
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation FLEET BY_Times series  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  
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
      

      
      plotFleetby <- function(){
            
        p <- ggplotFby<-ggplot(dataFby(), aes(x=as.numeric(year), y=q50, color=scenario))+
                geom_line(aes(color=scenario),size = input$lwdFby)+
                ylab("")+
                xlab("Year")+
                theme_bw()+
                theme( strip.text=element_text(size=16),
                      title=element_text(size=16),
                      text=element_text(size=16), legend.position="top")
        
        if(input$fitCIFby == TRUE){
          p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        }
        
        if(input$dotLineFby == TRUE) p <- p +  geom_point(data = dataFby(), aes(x=year, y=q50, color=scenario), size = input$dszFby)
        
        if(!is.null(proj.yr)){
          p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1) # projection starting year 
          
        }
        
         if(input$fitFby == FALSE){
           if(input$rowFby == 'Fleet-Stock'){
             p <- p + facet_grid(fleet*stock ~ indicator)}
             else{
               if(input$rowFby == 'Fleet-Indicator'){p <- p + facet_grid(fleet*indicator ~ stock)}
               else{ p <- p + facet_grid(stock*indicator ~ fleet)}
         }}
         else{
           if(input$rowFby == 'Fleet-Stock'){
             p <- p + facet_wrap(fleet*stock ~ indicator, scales="free_y",  ncol = input$nColFby)}
           else{
             if(input$rowFby == 'Fleet-Indicator'){p <- p + facet_wrap(fleet*indicator ~ stock, ncol = input$nColFby, scales="free_y")}
             else{ p <- p + facet_wrap(stock*indicator ~ fleet, ncol = input$nColFby, scales="free_y")}
           }}
        return(p)}
      
      
      values <- reactiveValues()
      
      plot_height <- function() {
        
        nS <- length(input$stockFby)
        nF <- length(input$fleetFby)
        nI <- length(input$indicatorFby)
        
        if(input$fitFby==FALSE){
            nrow <- ifelse(input$rowFby == 'Fleet-Stock', nF*nS, ifelse(input$rowFby == 'Fleet-Indicator', nF*nI, nI*nS))}
        else{nrow <- ceiling(nS*nF*nI/input$nColFby)}
        
        values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
        return(values$height)
      }
      
      plot_width <- function() {
        
        nS <- length(input$stockFby)
        nF <- length(input$fleetFby)
        nI <- length(input$indicatorFby)
        
        if(input$fitFby==FALSE){ncol <- ifelse(input$rowFby == 'Fleet-Stock', nI, ifelse(input$rowFby == 'Fleet-Indicator', nS, nF))}
        else{ncol <- min(input$nColFby, nS*nF*nI)}
        values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
        return(values$width)
      }
      
      output$plotFbys <-renderPlot({print(plotFleetby())})

      # wrap plotOutput in renderUI
      output$plotFby <- renderUI({plotOutput("plotFbys", height = plot_height(), width = plot_width())})
      
      # Code to download the plot
      getFbyW <- function(){return(input$fileWFby)}
      getFbyH <- function(){return(input$fileHFby)}
      getFbyS <- function(){return(input$fileScFby)}
      
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
              text=element_text(size=16), legend.position="top")
      
      if(!is.null(proj.yr)){p <- p +  geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1)}
      
      if(input$fitbyA == FALSE){p <- p + facet_grid(fleet*scenario~indicator)}
      else{p <- p + facet_wrap(fleet*scenario~indicator, ncol = input$nColbyA, scale = 'free_y')}
      
    }
    
    values <- reactiveValues()
    
    plot_height <- function() {
      
      nF  <- length(input$fleetbyA)
      nI  <- length(input$indicatorbyA)
      nSc <- length(input$scenariobyA)
      
      if(input$fitbyA==FALSE){nrow <- nF*nSc}
      else{nrow <- ceiling(nF*nSc*nI/input$nColbyA)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      nF  <- length(input$fleetbyA)
      nI  <- length(input$indicatorbyA)
      nSc <- length(input$scenariobyA)
      
      if(input$fitbyA==FALSE){ncol <- length(input$indicatorbyA)}
      else{ncol <- input$nColbyA}
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotFSbyAs<-renderPlot({print(plotFleetStockArea())})
    
    # wrap plotOutput in renderUI
    output$plotFSbyA <- renderUI({plotOutput("plotFSbyAs", height = plot_height(), width = plot_width())})

    # Code to download the plot
    getWbyA <- function(){return(input$fileWbyA)}
    getHbyA <- function(){return(input$fileHbyA)}
    getSbyA <- function(){return(input$fileScbyA)}
    
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
              title=element_text(size=18,face="bold"), legend.position="top")+
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
                title=element_text(size=18,face="bold"), legend.position="top")+
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
                title=element_text(size=18,face="bold"), legend.position="top")+
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
  # values <- reactiveValues()
  # plot_height <- function() {
  #   lines <- input$GrpPanFSPby
  #   if(lines == 'stock'){ facets <- length(input$indicatorFSPby)*length(input$fleetFSPby)} 
  #   else{ 
  #     if(lines == 'fleet'){facets <- length(input$indicatorFSPby)*length(input$stockFSPby)} 
  #     else{facets <- length(input$fleetFSPby)*length(input$stockFSPby)}
  #   }
  #   
  #   # calculate values$facetCount
  #   values$facetsRows <- ifelse(facets/input$nColFSPby == 1, 600, ifelse(facets/input$nColFSPby == 2, 800, 
  #                                                               ifelse(facets/input$nColFSPby == 3, 100, 300*ceiling(facets/input$nColFSPby))))
  #   return(values$facetsRows)
  # }
  # 
  # plot_width <- function() {
  #   
  #   lines <- input$GrpPanFSPby
  #   if(lines == 'stock'){ facets <- length(input$indicatorFSPby)*length(input$fleetFSPby)} 
  #   else{ 
  #     if(lines == 'fleet'){facets <- length(input$indicatorFSPby)*length(input$stockFSPby)} 
  #     else{facets <- length(input$fleetFSPby)*length(input$stockFSPby)}
  #   }
  #   
  # 
  #   values$facetsCols <- ifelse(input$nColFSPby == 1, 600, ifelse(input$nColFSPby <= 2, 900, 1200))
  #   return(values$facetsCols)
  # }
  
  
  # wrap plotOutput in renderUI
  output$plotFSPby<- renderUI({
     plotOutput("plotFSPbys", height = 700, width = 1200)
  })
  
  # Code to download the plot
  getWSP  <- function(){ return(input$fileWFSPby)}
  getHSP  <- function(){return(input$fileHFSPby)}
  getScSP <- function(){return(input$fileScFSPby)}
  
  # Download the plot
  output$downFSPby <- downloadHandler(
    filename =  function() {
      paste(input$filenmFSPby, input$fileTypeFSPby, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, plotFbySpider())#, width = getWSP(), height = getHSP(), units = 'cm', scale = getScSP())
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
              text=element_text(size=16), legend.position="top")
      
      if(!is.null(proj.yr)){p <- p +  geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1)} # projection starting year
  
      if(input$fitMA == FALSE){p <- p + facet_grid(fleet*scenario~indicator)}
      else{ p <- p + facet_wrap(fleet*scenario~indicator, ncol = input$nColMA, scale = 'free_y')}
      
    }
    
    values <- reactiveValues()
    
    plot_height <- function() {
      
      nI  <- length(input$indicatorMA)
      nSc <- length(input$scenarioMA)
      nF  <- length(input$fleetMA)
      
      if(input$fitMA==FALSE){nrow <- nF*nSc}
      else{nrow <- ceiling(nI*nSc*nF/input$nColMA)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      nI  <- length(input$indicatorMA)
      nSc <- length(input$scenarioMA)
      nF  <- length(input$fleetMA)
      
      if(input$fitMA==FALSE){ncol <- nI}
      else{ncol <- min(nI*nSc*nF, input$nColMA)}
      
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotFSMAs <-renderPlot({print(plotFleetMetierArea())})
    
    # wrap plotOutput in renderUI
    output$plotFSMA <- renderUI({plotOutput("plotFSMAs", height = plot_height(), width = plot_width())})
    
    
    # Code to download the plot
    getWMA <- function(){return(input$fileWMA)}
    getHMA <- function(){return(input$fileHMA)}
    getSMA <- function(){return(input$fileScMA)}
    
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
                      text=element_text(size=16), legend.position="top")

        if (input$fitCIMby == TRUE){p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)} 
        if(input$fitMby==TRUE){ p <- p + facet_wrap(metier*stock ~ indicator, ncol = input$nColMby, scales="free_y")}
        if(!is.null(proj.yr)){p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1)} # projection starting year 
          else{p <- p + facet_grid(metier*stock ~ indicator)}
      
        return(p)
    }
    
    values <- reactiveValues()
    
    plot_height <- function() {
      
      if(input$fitMby==FALSE){nrow <- length(input$fleetMby)}
      else{nrow <- ceiling(length(input$fleetMby)*length(input$indicatorMby)/input$nColMby)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      if(input$fitMby==FALSE){ncol <- length(input$indicatorMby)}
      else{ncol <- input$nColMby}
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotMbys<-renderPlot({print(plotMetierby())})
    
    # wrap plotOutput in renderUI
    output$plotMby <- renderUI({plotOutput("plotMbys", height = plot_height(), width = plot_width())})
 
    # Code to download the plot
    getMbyW <- function(){return(input$fileWMby)}
    getMbyH <- function(){return(input$fileHMby)}
    getMbyS <- function(){return(input$fileScMby)}
    
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
    
    plotMetierStockArea <- function(){
      
      p <-ggplot(data = dataMbyA())+
        geom_area(aes(x=year, y=q50, fill=stock), size=0.5, colour="grey") +
        ylab("")+xlab("Year")+
        theme_bw()+
        theme(strip.text=element_text(size=16),
              title=element_text(size=16),
              text=element_text(size=16), legend.position="top")
      
      if(!is.null(proj.yr)){p <- p +  geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1)} # projection starting year
      
      if(input$fitMbyA == FALSE){p <- p + facet_grid(metier*scenario~indicator)}
        else{p <- p + facet_wrap(meiter*scenario~indicator, ncol = input$nColMbyA, scale = 'free_y')}
      
   }
    
    
    values <- reactiveValues()
    
    plot_height <- function() {
      
      if(input$fitMbyA==FALSE){nrow <- length(input$metierMbyA)}
      else{nrow <- ceiling(length(input$metierMbyA)*length(input$indicatorMbyA)/input$nColMbyA)}
      
      values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      
      if(input$fitMbyA==FALSE){ncol <- length(input$indicatorMbyA)}
      else{ncol <- input$nColMbyA}
      values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
      return(values$width)
    }
    
    output$plotFSMbyAs<-renderPlot({print(plotMetierStockArea())})
    
    # wrap plotOutput in renderUI
    output$plotFSMbyA <- renderUI({plotOutput("plotFSMbyAs", height = plot_height(), width = plot_width())})

    
    
    # Code to download the plot
    getWMbyA <- function(){return(input$fileWMbyA)}
    getHMbyA <- function(){return(input$fileHMbyA)}
    getSMbyA <- function(){return(input$fileScMbyA)}
    
    # Download the plot
    output$downMbyA <- downloadHandler(
      filename =  function() {
        paste(input$filenmMbyA, input$fileTypeMbyA, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotMetierStockArea(), width = getWMbyA(), height = getHMbyA(), units = 'cm', scale = getSMbyA())
      }
    )
    
  })# end of the observe stock
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
  #### PAGE_simulation ADVICE_Times series  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
    observe ({
      dataA<-reactive({
        req(input$stockA)
        adv[adv$year>=input$rangeA[1]         & adv$year<=input$rangeA[2] & adv$stock%in%input$stockA
          & adv$indicator%in%input$indicatorA & adv$scenario%in%input$scenarioA,]})
      
      
      plotAdvice <- function(){
       p <-ggplot(dataA(), aes(x=as.numeric(year), y=q50, color=scenario))+
              geom_line(size = input$lwdA)+
              ylab("")+ xlab("Year")+
              theme_bw()+
              theme( strip.text=element_text(size=16),
                      title=element_text(size=16),
                      text=element_text(size=16), legend.position="top")
        
       if(!is.null(proj.yr)){p <- p + geom_vline(aes(xintercept=proj.yr), color="grey", linetype="dotted", lwd =1)} # projection starting year 
          
       if (input$fitCIA == TRUE){p <- p +  geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)}
        
       if(input$fitA==TRUE){p <- p + facet_wrap(indicator~stock, scales="free_y",ncol = input$nColA)}
          else{p <- p + facet_grid(indicator~stock)}
       
       if(input$dotLineA == TRUE) p <- p +  geom_point(data = dataA(), aes(x=year, y=q50, color=scenario), size = input$dszA)
       
        return(p)
        }
      
      
      values <- reactiveValues()
      
      plot_height <- function() {
        
        if(input$fitA==FALSE){nrow <- length(input$indicatorA)}
        else{nrow <- ceiling(length(input$stockA)*length(input$indicatorA)/input$nColA)}
        
        values$height <- ifelse(nrow == 1, 400, ifelse(nrow == 2, 600, ifelse(nrow == 3, 800, 250*nrow)))
        return(values$height)
      }
      
      plot_width <- function() {
        
        if(input$fitA==FALSE){ncol <- length(input$stockA)}
        else{ncol <- min(c(input$nColA, length(input$stockA)*length(input$indicatorA)))}
        
        values$width <- ifelse(ncol == 1, 650, ifelse(ncol == 2, 970, 1300))
        return(values$width)
      }
      
      output$plotAs <- renderPlot({print(plotAdvice())})
      
      # wrap plotOutput in renderUI
      output$plotA <- renderUI({plotOutput("plotAs", height = plot_height(), width = plot_width())})
      
    #  output$plotA <- renderUI({plotOutput("plotAs")}) #, height = plot_height(), width = plot_width())})
      
      # Code to download the plot
      getWA <- function(){return(input$fileWA)}
      getHA <- function(){return(input$fileHA)}
      getSA <- function(){return(input$fileScA)}
      
      # Download the plot
      output$downA <- downloadHandler(
        filename =  function() {
          paste(input$filenmA, input$fileTypeA, sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          ggsave(file, plotAdvice(), width = getWA(), height = getHA(), units = 'cm', scale = getSA())
        } 
      )
        
      
    })# end of the observe advice
    
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
#### PAGE_simulation Summary_polar plots  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-  
    #reactive: ssb and f
    st1 <- reactive({subset(bio, scenario %in% input$scenarioP & indicator %in% c("ssb", "f") & stock %in% input$stockP & year==input$yearP)[,c("stock","year","indicator", "scenario", "q50")]})
    st2 <- reactive({subset(bio, scenario %in% input$scenarioP & indicator %in% c("ssb", "f") & stock %in% input$stockP & year %in% input$rangeP)[,c("stock","year","indicator", "scenario", "q50")]})  
         
    #reactive: profits and capacity
    fl1 <- reactive({subset(flt, scenario %in% input$scenarioP & indicator %in% c("grossSurplus", "capacity") & fleet %in% input$fleetP & year==input$yearP)[,c("fleet","year","indicator", "scenario", "q50")]})
    fl2 <- reactive({subset(flt, scenario %in% input$scenarioP & indicator %in% c("grossSurplus", "capacity") & fleet %in% input$fleetP & year %in% input$rangeP)[,c("fleet","year","indicator", "scenario", "q50")]})


    plotPolar <- function(){
      # New data entry
      dat.stpolar <- NULL
      dat.flpolar <- NULL
      
      st3 <- aggregate(q50 ~ stock + indicator + scenario, data=st2(), FUN=mean, na.rm = TRUE)
      fl3 <- aggregate(q50 ~ fleet + indicator + scenario, data=fl2(), FUN=mean, na.rm = TRUE)

     # browser()
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
      palst <- pals::glasbey()[1:nst]
      
      pal <- c(palfl, palst) # it will sort the categories in alphabetic order

      ymax <- max(c(dat.stpolar$ratio, dat.flpolar$ratio), na.rm = TRUE)*1.05#(1+sqrt(5))/2


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
              strip.text=element_text(size=16), legend.position="top")+
        geom_hline(aes(yintercept=1))+
        geom_vline(aes(xintercept=0), lwd=1)+
        geom_vline(aes(xintercept=wst*nst), lwd=1)+
        geom_vline(aes(xintercept=wst*nst+wst*nst), lwd=1)+
        geom_vline(aes(xintercept=wst*nst+wst*nst+wfl*nfl), lwd=1)+
        xlim(c(0,4*w))+
        annotate(geom="text",x=w/2, y=ymax*1.02, label=c("F"), size=6)+
        annotate(geom="text",x=w*3/2, y=ymax*1.02, label=c("SSB"), size=6)+
        annotate(geom="text",x=w*5/2, y=ymax*1.02, label=c("Capacity"), size=6)+
        annotate(geom="text",x=w*7/2, y=ymax*1.02, label=c("Gross-Surplus"), size=6)+
        labs(fill="")+
        geom_text(aes(x=1, y = min(dat.flpolar$ratio),label = sum(npv$q50)))
      
      return(p)
      
    }
    
    
    values <- reactiveValues()
    
    plot_height <- function() {
       nrow <- ceiling(length(input$scenarioP)/input$nColP)
      values$height <- ifelse(nrow == 1, 600, ifelse(nrow == 2, 900, ifelse(nrow == 3, 1100, 300*nrow)))
      return(values$height)
    }
    
    plot_width <- function() {
      ncol <- min(c(input$nColP, input$scenarioP))
      values$width <- ifelse(ncol == 1, 550, ifelse(ncol == 2, 1000, 1300))
      return(values$width)
    }
    
    output$plotPs<-renderPlot({print(plotPolar())})
    
    # wrap plotOutput in renderUI
    output$plotP <- renderUI({plotOutput("plotPs", height = plot_height(), width = plot_width())})
  
    
    
    # Code to download the plot
    getPW <- function(){return(input$fileWP)}
    getPH <- function(){return(input$fileHP)}
    getPS <- function(){return(input$fileScP)}
    
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