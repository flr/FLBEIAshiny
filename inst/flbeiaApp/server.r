
server <- function(input, output, session){

#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation STOCK 
#-----------------------------------------------------------------------------------------------------------------------  
  
  
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
      bio.iter[bio.iter$year>=input$rangeS[1] & bio.iter$year<=input$rangeS[2] 
             & bio.iter$stock%in%input$stockS
             & bio.iter$indicator%in%input$indicatorS
             & bio.iter$scenario%in%input$scenarioS
             & bio.iter$iter%in%input$iterS,]
    })

    datarpS<-reactive({
        req(input$stockS)
        RefPts[RefPts$stock%in%input$stockS
                & RefPts$indicator%in%input$indicatorS
                & RefPts$scenario%in%input$scenarioS,]
      })
    
    
    plotStock <- function(){
      
      p <-ggplot()+
        geom_line(data = dataS(), aes(x=year, y=q50, color=scenario), lwd = 1) +
        geom_vline(data = dataS(), aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
        ylab("")+xlab("Year")+
        theme_bw()+
        theme( strip.text=element_text(size=16),
               title=element_text(size=16),
               text=element_text(size=16))
      
      # Iteraction
       if(!is.null(input$iterS)){
         p <- p + geom_line(data = dataSI(), aes(x=year, y=q50, group = interaction(scenario, iter), color = scenario,  linetype = iter), lwd=1)+
           scale_linetype_manual(values = c(2:6))
       }

      # Refence points
      if (input$refpointS== TRUE){
        p <- p +geom_hline(data = datarpS(), aes(yintercept=value), color="red", linetype="dotted", lwd =1)
      }
      
      # Confidence intervals
      if (input$fitCIS == TRUE){
        p <- p + geom_ribbon(data = dataS(), aes(x=year, ymin=q05, ymax=q95,fill = scenario), alpha=0.3)+
                 geom_ribbon(data = dataSI(), aes(x=year, ymin=q05, ymax=q95,group = interaction(scenario, iter), fill = scenario), alpha=0.1)
      }
      
      if(input$fitS == FALSE){
          p <- p + facet_grid(stock~indicator)
      }
      else{
          p <- p + facet_wrap(stock~indicator, scale = 'free_y')
      }
      
      }
    
    
    output$plotS<-renderPlot({
      print(plotStock())
    } #, height = PlotHeight_stk
    )
    
    
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
#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation STOCK_kobe plot 
#-----------------------------------------------------------------------------------------------------------------------  
  
 observe({ dataK<-reactive({
        req(input$stockK)
  
    data[data$year>=input$rangeK[1] & data$year<=input$rangeK[2] 
        & data$unit%in%input$stockK
        & data$scenario%in%input$scenarioK,]
  })

  
  plotKobe <- function(){
      kobePhase(dataK())+
      geom_point(aes(stock,harvest, group=unit, col=scenario))+
      geom_text(data=dataK(),aes(stock,harvest, col=scenario, group=unit, label=year))+
      geom_path(aes(stock, harvest, group=unit, col=scenario), data=dataK())+
      facet_wrap(~unit)+
      theme(text=element_text(size=16),
            title=element_text(size=16),
            strip.text=element_text(size=16))
  }
  
  output$plotK <- renderPlot({

    plotKobe()
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
#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation STOCK_Biological risk
#-----------------------------------------------------------------------------------------------------------------------  
  
  dataR<-reactive({
    req(input$stockR)
    risk[risk$year>=input$rangeR[1] & risk$year<=input$rangeR[2] 
         & risk$unit%in%input$stockR
         & risk$scenario%in%input$scenarioR
         & risk$indicator%in%input$brpR,]
  })
  
  
  plotSR <- function(){
    ggplot(dataR(), aes(x=year, y=value, group=scenario, color=scenario))+
      geom_line(aes(color=scenario), lwd = 1)+
      geom_vline(aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
      facet_grid(indicator~unit)+
      theme_bw()+
      theme(text=element_text(size=16),
            title=element_text(size=16),
            strip.text=element_text(size=16)#,
            #axis.text.x = element_text(angle = 90, hjust = 1)
            )+
      xlab("Year")+ ylab("Probability")
  }
  
  output$plotR<-renderPlot({
    plotSR()
  })
  
  
  # Code to download the plot
  getWSR <- function(){
    return(input$fileWSR)
  }
  
  getHSR <- function(){
    return(input$fileHSR)
  }
  
  getSSR <- function(){
    return(input$fileScSR)
  }
  
  # Download the plot
  output$downSR <- downloadHandler(
    filename =  function() {
      paste(input$filenmSR, input$fileTypeSR, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      ggsave(file, plotSR(), width = getWSR(), height = getHSR(), units = 'cm', scale = getSSR())
    } 
  )
  
  print('three') 
  
  #-----------------------------------------------------------------------------------------------------------------------  
  # PAGE_simulation STOCK_Spider plot
  #-----------------------------------------------------------------------------------------------------------------------  
  
    dataSP<-reactive({
      
      if (input$yearSP == "radio1"){
        req(input$stockSP)
      
      dat<-bio.scaled[bio.scaled$year == input$yearSP0
          & bio.scaled$stock%in%input$stockSP
          & bio.scaled$indicator%in%input$indicatorSP
          & bio.scaled$scenario%in%input$scenarioSP,]
      
      }else{

      if (input$yearSP == "radio2"){
          req(input$stockSP)
       dat <- bio.scaled[bio.scaled$year%in%c(input$yearSP1,input$yearSP2)
          & bio.scaled$stock%in%input$stockSP
          & bio.scaled$indicator%in%input$indicatorSP
          & bio.scaled$scenario%in%input$scenarioSP,]
       
       dat<- dat %>% group_by (stock, scenario, indicator) %>%
         summarize(Ratio = c(value2[1] / value2[2]))

        } 
      
      dat
      }
    })

    
  output$plotSP<-renderPlot({

     if (input$yearSP == "radio1"){
  
       ggplot(data=dataSP(), aes(x=scenario, y=value2, col=stock, fill=stock, group=stock))+
         # geom_polygon(alpha=0.2, lwd=1)+
         geom_polygon(fill=NA, lwd=1)+
         geom_point(cex=1.5)+
         facet_grid (. ~ indicator)+
         coord_radar()+
         theme_bw()+
         theme(text=element_text(size=14),
               strip.text=element_text(size=14),
               title=element_text(size=18,face="bold"))+
         ylab("")+
         ylim(c(0,1))
     }else{
    
     if (input$yearSP == "radio2"){

      ggplot(data=dataSP(), aes(x=scenario, y=Ratio, col=stock, fill=stock, group=stock))+
        # geom_polygon(alpha=0.2, lwd=1)+
        geom_polygon(fill=NA, lwd=1)+
        geom_point(cex=1.5)+
        facet_grid (. ~ indicator)+
        coord_radar()+
        theme_bw()+
        theme(text=element_text(size=16),
              strip.text=element_text(size=16),
              title=element_text(size=18,face="bold"))+
        ylab("")+
        ylim(c(0,1))

     }
     }
      
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
    
  
  
#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation FLEET
#-----------------------------------------------------------------------------------------------------------------------  
  
  #---------------------------------------------------
  # PAGE_simulation FLEET_TIMES SERIES
  #---------------------------------------------------
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
      flt.iter[flt.iter$year>=input$rangeF[1] & flt.iter$year<=input$rangeF[2] 
               & flt.iter$fleet%in%input$fleetF
               & flt.iter$indicator%in%input$indicatorF
               & flt.iter$scenario%in%input$scenarioF
               & flt.iter$iter%in%input$iterF,]
    })
    
    
    plotFleet <- function(){
      
      p <- ggplot()+
                geom_line(data= dataF(), aes(x=year, y=q50, color=scenario),lwd=1)+
                geom_vline(data=dataF(), aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
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
      
      # With Conf Int.
      if (input$fitCIF == TRUE){
        p <- p + geom_ribbon(data= dataF(), aes(x=year, y=q50, color=scenario), aes(x=year, ymin=q05, ymax=q95,fill = scenario), alpha=0.3)+
                 geom_ribbon(data = dataFI(), aes(x=year, ymin=q05, ymax=q95,group = interaction(scenario, iter), fill = scenario), alpha=0.1)
      }
      
      if(input$fitF==TRUE){
        p <- p + facet_wrap(fleet ~ indicator, ncol=length(input$fleetF), scales="free_y")
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

    #-------------------------------
    # PAGE_simulation FLEET_NPV
    #-------------------------------
  
    # print('caracola02')   
    dataN<-reactive({
        req(input$fleetN)
        npv[npv$fleet%in%input$fleetN & npv$scenario%in%input$scenarioN,]})

    plotNPV <- function(){
      ggplot(dataN(), aes(x=fleet, y=q50, group=scenario))+
        geom_point(aes(color=fleet),cex=2)+
        geom_errorbar(aes(ymin=q05, ymax=q95, color=fleet), lwd=1)+
        theme_bw()+
        facet_wrap(~scenario)+
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
    #-------------------------------
    # PAGE_simulation FLEET_Risk
    #-------------------------------
  
    dataE<-reactive({
          req(input$fleetE)
          risk[risk$unit%in%input$fleetE & risk$scenario%in%input$scenarioE & risk$indicator=="pPrflim",]})

      
    plotFLRisk <- function(){
        ggplot(dataE(), aes(x=year, y=value, color=scenario))+
        geom_line(aes(color=scenario),lwd=1)+
        geom_vline(aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
        facet_wrap(~unit, scales="free")+
        ylab("")+ xlab("Year")+
        theme_bw()+
        theme( strip.text=element_text(size=16),
               title=element_text(size=16),
               text=element_text(size=16))
      }
      
    output$plotFR <-renderPlot({
            plotFLRisk()
    })
    
    
    # Code to download the plot
    getFRW <- function(){
      return(input$fileWFR)
    }
    
    getFRH <- function(){
      return(input$fileHFR)
    }
    
    getFRS <- function(){
      return(input$fileScFR)
    }
    
    # Download the plot
    output$downFR <- downloadHandler(
      filename =  function() {
        paste(input$filenmFR, input$fileTypeFR, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotFLRisk(), width = getFRW(), height = getFRH(), units = 'cm', scale = getFRS())
      } 
    )
    print('six')
    
    #------------------------------------------------
    # PAGE_simulation FLEET_Spider plot
    #------------------------------------------------
    
    dataFP<-reactive({
      
      if (input$yearFP == "radioF1"){
        req(input$fleetFP)
        
        dat<-flt.scaled[flt.scaled$year == input$yearFP0
                        & flt.scaled$fleet%in%input$fleetFP
                        & flt.scaled$indicator%in%input$indicatorFP
                        & flt.scaled$scenario%in%input$scenarioFP,]
        
      }else{
        
        if (input$yearFP == "radioF2"){
          req(input$fleetFP)
          dat <- flt.scaled[flt.scaled$year%in%c(input$yearFP1,input$yearFP2)
                            & flt.scaled$fleet%in%input$fleetFP
                            & flt.scaled$indicator%in%input$indicatorFP
                            & flt.scaled$scenario%in%input$scenarioFP,]
          
          dat<- dat %>% group_by (fleet, scenario, indicator) %>%
            summarize(Ratio = c(value2[1] / value2[2]))
          
        } 
        
        dat
      }
    })
    
    
    output$plotFP<-renderPlot({
      
      if (input$yearFP == "radioF1"){
        
        ggplot(data=dataFP(), aes(x=scenario, y=value2, col=fleet, fill=fleet, group=fleet))+
          # geom_polygon(alpha=0.2, lwd=1)+
          geom_polygon(fill=NA, lwd=1)+
          geom_point(cex=1.5)+
          facet_grid (. ~ indicator)+
          coord_radar()+
          theme_bw()+
          theme(text=element_text(size=14),
                strip.text=element_text(size=14),
                title=element_text(size=18,face="bold"))+
          ylab("")+
          ylim(c(0,1))
      }else{
        
        if (input$yearFP == "radioF2"){
          
          ggplot(data=dataFP(), aes(x=scenario, y=Ratio, col=fleet, fill=fleet, group=fleet))+
            # geom_polygon(alpha=0.2, lwd=1)+
            geom_polygon(fill=NA, lwd=1)+
            geom_point(cex=1.5)+
            facet_grid (. ~ indicator)+
            coord_radar()+
            theme_bw()+
            theme(text=element_text(size=16),
                  strip.text=element_text(size=16),
                  title=element_text(size=18,face="bold"))+
            ylab("")+
            ylim(c(0,1))
          
        }
      }
      
    })
    
    # Code to download the plot
    getWFP <- function(){
      return(input$fileWFP)
    }
    
    getHFP <- function(){
      return(input$fileHFP)
    }
    
    getScFP <- function(){
      return(input$fileScFP)
    }
    
    # Download the plot
    output$downFP <- downloadHandler(
      filename =  function() {
        paste(input$filenmFP, input$fileTypeFP, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggsave(file, plotFP(), width = getWFP(), height = getHFP(), units = 'cm', scale = getScFP())
      } 
    )
    
    print('six spider')
    
  
#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation METIER_Times series 
#-----------------------------------------------------------------------------------------------------------------------  
  
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
                  geom_vline(aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
                  ylab("")+xlab("Year")+
                  theme_bw()+
                  theme( strip.text=element_text(size=16),
                          title=element_text(size=16),
                        text=element_text(size=16))+
                  scale_x_continuous(limits = c(input$rangeM[1], input$rangeM[2]))
      
        if(input$fitCIM == TRUE)
            p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        
        if(input$fitM==TRUE){
          p <- p + facet_wrap(metier ~ indicator, scale = 'free_y')
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
#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation FLEET BY_Times series 
#-----------------------------------------------------------------------------------------------------------------------  
  
  # print('caracola06')      
  # PlotHeight_Fby <- reactive({
  #   
  #   nids <- length(input$fleetFby)*length(input$stockFby)
  #   
  #   return(300*nids)})
  
  observe ({
    
    updateSelectInput(session, inputId  = "stockFby", 
                               choices  = unique(fltStk[fltStk$fleet %in% input$fleetFby, 'stock']), 
                               selected = unique(fltStk[fltStk$fleet %in% input$fleetFby, 'stock'])[1])#, server = TRUE)#,
    })    

  observe ({
      dataFby<-reactive({
        req(input$fleetFby)
          fltStk[fltStk$year>=input$rangeFby[1]         & fltStk$year<=input$rangeFby[2]
               & fltStk$stock%in%input$stockFby         & fltStk$fleet%in%input$fleetFby
               & fltStk$indicator%in%input$indicatorFby & fltStk$scenario%in%input$scenarioFby,]
      })
      
   #   browser()

      # print('caracola061')  

      
      plotFleetby <- function(){
            
        p <- ggplotFby<-ggplot(dataFby(), aes(x=as.numeric(year), y=q50, color=scenario))+
                geom_line(aes(color=scenario),lwd=1)+
                geom_vline(aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
                ylab("")+
                xlab("Year")+
                theme_bw()+
                theme( strip.text=element_text(size=16),
                      title=element_text(size=16),
                      text=element_text(size=16))
        
        if(input$fitCIFby == TRUE){
          p <- p + geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        }
        
        if(input$fitFby == FALSE){
          p <- p + facet_grid(fleet*stock ~ indicator)
        }
        else{
          p <- p + facet_wrap(fleet*stock ~ indicator, ncol=length(input$stockFby), scales="free_y")
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
#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation METIER BY_Times series 
#-----------------------------------------------------------------------------------------------------------------------  
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
                geom_vline(aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
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
          p <- p + facet_wrap(metier*stock ~ indicator, ncol=length(input$metierMby), scales="free_y")
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

#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation ADVICE_Times series 
#-----------------------------------------------------------------------------------------------------------------------  
  
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
        p <- ggplotA <-ggplot(dataA(), aes(x=as.numeric(year), y=q50, color=scenario))+
              geom_line(lwd=1)+
              geom_vline(aes(xintercept=2017), color="grey", linetype="dotted", lwd =1)+ # projection starting year 
              ylab("")+ xlab("Year")+
              theme_bw()+
              theme( strip.text=element_text(size=16),
                      title=element_text(size=16),
                      text=element_text(size=16))
        
        if (input$fitCIA == TRUE){
          p <- p +  geom_ribbon(aes(x=as.numeric(year), ymin=q05, ymax=q95,fill = scenario), alpha=0.3)
        }
        if(input$fitA==TRUE){
          p <- p + facet_wrap(indicator~stock, scales="free_y",ncol=length(input$stockA))
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
  
#-----------------------------------------------------------------------------------------------------------------------  
# PAGE_simulation Summary_polar plots 
#-----------------------------------------------------------------------------------------------------------------------  
    # 
    # PlotHeight_sum <- reactive({
    #   
    #   nids <- length(input$scenarioP)
    #   
    #   return(300*nids)})
    
    # print('caracola09')   
    #reactive: ssb and f
    st1 <- reactive({bio[bio$scenario %in% input$scenarioP & (bio$indicator=="ssb" | bio$indicator=="f") & bio$year==input$yearP, c("stock","year","indicator", "scenario", "q50")]})
    st2 <- reactive({bio[bio$scenario %in% input$scenarioP & (bio$indicator=="ssb" | bio$indicator=="f") & (bio$year>=input$rangeP[1] & bio$year<=input$rangeP[2]), c("stock","year","indicator", "scenario", "q50")]})
     
    #reactive: profits and capacity
    fl1 <- reactive({flt[flt$scenario %in% input$scenarioP & (flt$indicator=="grossSurplus" | flt$indicator=="capacity") & flt$year==input$yearP, c("fleet","year","indicator", "scenario", "q50")]})
    fl2 <- reactive({flt[flt$scenario %in% input$scenarioP & (flt$indicator=="grossSurplus" | flt$indicator=="capacity") & (flt$year>=input$rangeP[1] & flt$year<=input$rangeP[2]), c("fleet","year","indicator", "scenario", "q50")]})
    
    
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
      palfl <- RColorBrewer::brewer.pal(9 , "Pastel1") 
      palst <- RColorBrewer::brewer.pal(9, "Set1") 
      
      palfl <- colorRampPalette(palfl)(nfl)
      palst <- colorRampPalette(palst)(nst)
      
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
        facet_wrap(scenario~., ncol = 1)+
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
        print(plotPolar())
    }#, height = PlotHeight_sum
    )
    
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