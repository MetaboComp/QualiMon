monitorUI<-function(id){
  ns<-NS(id)

  tagList(
    uiOutput(ns('monitor.1')),

    fluidRow(
      column(
        width=6,
        tabBox(
          width=12,
          #height="600px",
          tabPanel(
            "Status plot",
            uiOutput(ns("statusPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "Int outlier plot",
            uiOutput(ns("intOutPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "RT outlier plot",
            uiOutput(ns("rtOutPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "Height outlier plot",
            uiOutput(ns("heightOutPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "FWHM outlier plot",
            uiOutput(ns("fwhmOutPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "TF outlier plot",
            uiOutput(ns("tfOutPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "SN outlier plot",
            uiOutput(ns("snOutPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "DataPoints outlier plot",
            uiOutput(ns("datapointsOutPlotOrNot")),
            br(),
            br()
          ),
          tabPanel(
            "Noise outlier plot",
            uiOutput(ns("noiseOutPlotOrNot")),
            br(),
            br()
          )
        )
      ),

      column(
        width=6,
        tabBox(
          width=12,
          height="500px",
          tabPanel(
            "n Peaks plot",
            uiOutput(ns("nPeaksPlotOrNot")),
            br(),
            textOutput(ns("plotPeaksClick"))
          ),
          tabPanel(
            "IPO plot",
            uiOutput(ns("IPOPlotOrNot")),
            br(),
            textOutput(ns("plotIPOClick"))
          ),
          tabPanel(
            "n Landmarks plot",
            uiOutput(ns("nLMPlotOrNot")),
            br(),
            textOutput(ns("plotnLMClick"))
          ),
          tabPanel(
            "TIC plot",
            uiOutput(ns("TICPlotOrNot")),
            br(),
            textOutput(ns("plotTICClick"))
          )
        )
      )
    )#,
    #
    #     fluidRow(
    #       tabBox(
    #         title="Status breakdown",
    #         width=12,
    #
    #         tabPanel(
    #           "10 last samples",
    #           plotlyOutput(ns("badSample_10"))
    #         ),
    #         tabPanel(
    #           "All bad samples",
    #           fluidRow(
    #             column(
    #               width=8,
    #               plotlyOutput(ns("badSample_all.1"))
    #             ),
    #             column(
    #               width=4,
    #               plotlyOutput(ns("badSample_all.2"))
    #             )
    #           )
    #         ),
    #         tabPanel(
    #           "Choose any sample",
    #           plotOutput(ns("badSample_any"))
    #         )
    #
    #       )
    #     )
  )


}

monitorServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      graph_colors<-c("green", "red", "darkblue", "orange", "black")

      filePath<-NULL

      statusPlot <- reactiveVal()
      intPlot <- reactiveVal()
      rtPlot <- reactiveVal()
      heightPlot <- reactiveVal()
      fwhmPlot <- reactiveVal()
      tfPlot <- reactiveVal()
      snPlot <- reactiveVal()
      dpPlot <- reactiveVal()
      noisePlot <- reactiveVal()
      dataAvailable <- reactiveVal()
      firstPass <- reactiveVal()
      inter <- AsyncInterruptor$new()
      fut <- NULL

      ################################
      ###Rendering the config input###
      ################################

      #### Rendering config-file choice, but only if configWizard hasn't been performed during this session? Overly complicated? ####
      output$monitor.1 <- renderUI({
        fluidRow(
          box(
            title = "Controls",
            width = 4,
            status=r$monitor$boxStatus,
            solidHeader=T,
            fluidRow(
              column(
                width=6,
                br(),
                shinyFilesButton(id=ns('preConfig'), label="Choose config file", title="Choose the config file to use for analysis", multiple=F),
                br(),
              ),
              if(!is.null(r$configWiz$preConfig)){
                column(
                  width=6,
                  tags$b("Initiate live monitoring"),
                  prettySwitch(inputId=ns('start'), "Start", value=r$monitor$start),
                )
              }
            ),
            br(),
            # if(!is.null(r$monitor$methods)){
            tagList(
              fluidRow(
                column(
                  width=6,
                  selectInput(ns("sampType"), "Injection type", choices=c("sample","sQC"), selected=input$sampType)
                ),
                column(width=6,
                       selectInput(ns("chromPol"), "Chrom & Pol", choices=c("RP","RN","HP","HN"), selected=input$chromPol) #Update this with available chromPol?
                )
              )
            ),
            # },
            fluidRow(
              column(
                width=6,

                selectInput(ns("projName"), "Project to monitor", choices=c("All samples",r$monitor$projects), selected=input$projName)
              )
            ),
            fluidRow(
              column(
                width=12,
                if(is.null(r$configWiz$preConfig)){
                  tags$b("No config file loaded.")
                } else {
                  tagList(
                    br(),
                    tags$b("Config file: "),
                    r$configWiz$preConfig,
                    br()
                  )
                },
                if(!is.null(r$monitor$curr_status)){
                  if(r$monitor$start==T){
                    tagList(
                      tags$b("Current status: "),
                      # r$monitor$curr_status
                    )
                  }
                }
              )
            )

          ),
          box(
            title="Overview",
            width=6,
            fluidRow(
              #column(
              #width=6,
              if(!is.null(r$monitor$nSamps)){
                valueBoxOutput(ns("infoSamples"))
              },
              if(!is.null(r$monitor$nBatches)){
                valueBoxOutput(ns("infoBatches"))
              }
              #)
            )
          )
        )
      })

      #Status of button-box based on steps taken by user
      observe({
        if(is.null(r$monitor$start) && is.null(r$configWiz$preConfig)){
          r$monitor$boxStatus='danger'
        } else if ((r$monitor$start==F && !is.null(r$configWiz$preConfig)) || (is.null(input$start) && !is.null(r$configWiz$preConfig))){
          r$monitor$boxStatus='warning'
        } else if (r$monitor$start==T && !is.null(r$configWiz$preConfig)){
          r$monitor$boxStatus='success'
        }
      })

      ###############################
      ###Rendering plots and stuff###
      ###############################

      ####################################
      ####Info boxes output generation####

      output$infoSamples<-renderInfoBox({
        valueBox(
          r$monitor$nSamps, "Samples", icon=icon("vial"), color="teal"
        )
      })

      output$infoBatches<-renderInfoBox({
        valueBox(
          r$monitor$nBatches, "Batches", icon=icon("vials"), color="orange"
        )
      })

      #####################
      #### Left tabbox ####
      ######################################
      ####Status plot - Checking samples####
      output$statusPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotStatus"))
        }
      })

      #Status plot - Actual plot when enough samples
      output$plotStatus<-renderPlotly({

        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }


          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'status')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ########################
      ####Int outlier plot####
      output$intOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotIntOutlier"))
        }
      })

      output$plotIntOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmIntOutliers')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ########################
      ####RT outlier plot####
      output$rtOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotRTOutlier"))
        }
      })

      output$plotRTOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmRTOutliers')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ##########################
      ####Height outlier plot####
      output$heightOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotHeightOutlier"))
        }
      })

      output$plotHeightOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmHeightOutliers')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ########################
      ####FWHM outlier plot####
      output$fwhmOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotFWHMOutlier"))
        }
      })

      output$plotFWHMOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmFWHMOutliers')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ########################
      ####TF outlier plot####
      output$tfOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotTFOutlier"))
        }
      })

      output$plotTFOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmTFOutliers')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ########################
      ####SN outlier plot####
      output$snOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotSNOutlier"))
        }
      })

      output$plotSNOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmSNOutliers')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ###############################
      ####DataPoints outlier plot####
      output$datapointsOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotDataPointsOutlier"))
        }
      })

      output$plotDataPointsOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmDPOutliers')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ##########################
      ####Noise outlier plot####
      output$noiseOutPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<2){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotNoiseOutlier"))
        }
      })

      output$plotNoiseOutlier<-renderPlotly({
        if(nrow(r$monitor$plotData$DT) > 0){
          nCols <- max(r$monitor$plotData$DT$sampleIter) #max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$configWiz$config$nSampsMonitor){
            toRemove <- r$configWiz$config$nSampsMonitor
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 1
            startNumb <- 1
            nCols <- 1
          }

          if((toRemove-1)==0){
            rowsToRemove = -(nCols+2)
          } else if((startNumb-toRemove) < 0) {
            rowsToRemove = -(startNumb)
          } else {
            rowsToRemove = -(toRemove-1)
          }

          dcastObj <- dcast(r$monitor$plotData$DT,
                            sampleNumber~sampleIter,
                            value.var = 'lmNoise')[, tail(.SD, rowsToRemove), ]

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$monitor$plotData$DTunique$name[dcastObj$sampleNumber]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj[,sampleNumber:=NULL], dendrogram="none", showticklabels = c(F,F), plotmethod="ggplot")
        } else {
          NULL
        }
      })

      ######################
      #### Right tabbox ####
      ######################################
      ####nPeaks plot - Checking samples####
      output$nPeaksPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<1){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotnPeaks"))
        }
      })

      ####nPeaks plot - Actual plot####
      output$plotnPeaks<-renderPlotly({
        if(!is.null(r$monitor$plotData$DTunique)){
          p <- plot_ly(type="scatter", mode="lines+markers")

          #Setting limits based on chromPol
          if(r$monitor$chromPolFormat=="RP"){
            upper_abline <- r$configWiz$config$RPnPeaksSoftLim
            lower_abline <- r$configWiz$config$RPnPeaksHardLim
          }
          else if (r$monitor$chromPolFormat=="RN"){
            upper_abline <- r$configWiz$config$RNnPeaksSoftLim
            lower_abline <- r$configWiz$config$RNnPeaksHardLim
          }
          else if (r$monitor$chromPolFormat=="HP"){
            upper_abline <- r$configWiz$config$HPnPeaksSoftLim
            lower_abline <- r$configWiz$config$HPnPeaksHardLim
          }
          else if (r$monitor$chromPolFormat=="HN"){
            upper_abline <- r$configWiz$config$HNnPeaksSoftLim
            lower_abline <- r$configWiz$config$HNnPeaksHardLim
          }

          # If not first batch, printing lines with different colors based on batch belonging
          if(length(r$monitor$batchFreq)!=1){
            sampsSum = 1

            for(i in 1:length(r$monitor$batchFreq)){

              p<-add_trace(p,
                           x=as.integer(c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))),
                           y=as.integer(r$monitor$plotData$DTunique$nPeaks[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]),
                           type="scatter",
                           mode="lines+markers",
                           marker=list(
                             color=graph_colors[i%%5+1]
                           ),
                           line=list(color=graph_colors[i%%5+1]),
                           name=r$monitor$batchFreq[i],
                           text = paste("Batch: ", r$monitor$batchFreq[i], "\n", r$monitor$plotData$sampleNames[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]), #-1
                           color = graph_colors[i%%5+1],
                           colors = graph_colors[i%%5+1],
                           connectgaps=TRUE
              )
              sampsSum=sampsSum+r$monitor$plotData$sampsInBatch[i]
            }

            # If first batch, different printing settings
          } else {
            p <- add_trace(p,
                           x=as.integer(c(1:r$monitor$plotData$sampsInBatch[1])),
                           y=as.integer(r$monitor$plotData$DTunique$nPeaks),
                           type="scatter",
                           mode="lines+markers",
                           marker=list(
                             color=graph_colors[1]
                           ),
                           line=list(color=graph_colors[1]),
                           name= "nPeaks",
                           text = ~paste("Batch: ", r$monitor$batchFreq[1], "\n", r$monitor$plotData$sampleNames[c(1:r$monitor$plotData$sampsInBatch[1])]),
                           color = graph_colors[1],
                           colors = graph_colors[1]
            )
          }
          # Adding ablines based on chromPol of samples
          if(upper_abline != 0 && lower_abline != 0){
            p<-add_trace(p,
                         x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                         y = upper_abline,
                         type="scatter",
                         mode="lines",
                         line=list(color ="orange"),
                         name="Soft limit"
            )
            p<-add_trace(p, x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                         y = lower_abline,
                         type="scatter",
                         mode="lines",
                         line=list(color ="red"),
                         name="Hard limit"
            )
          }
        }
      })

      ###################################
      ####IPO plot - Checking samples####

      output$IPOPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<1){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotIPO"))
        }
      })

      ####IPO plot - Actual plot####
      output$plotIPO<-renderPlotly({
        if(!is.null(r$monitor$plotData$DTunique)){
          p <- plot_ly(type="scatter", mode="lines+markers")

          #Setting limits based on chromPol
          if(r$monitor$chromPolFormat=="RP"){
            upper_abline <- r$configWiz$config$RPIPOSoftLim
            lower_abline <- r$configWiz$config$RPIPOHardLim
          }
          else if (r$monitor$chromPolFormat=="RN"){
            upper_abline <- r$configWiz$config$RNIPOSoftLim
            lower_abline <- r$configWiz$config$RNIPOHardLim
          }
          else if (r$monitor$chromPolFormat=="HP"){
            upper_abline <- r$configWiz$config$HPIPOSoftLim
            lower_abline <- r$configWiz$config$HPIPOHardLim
          }
          else if (r$monitor$chromPolFormat=="HN"){
            upper_abline <- r$configWiz$config$HNIPOSoftLim
            lower_abline <- r$configWiz$config$HNIPOHardLim
          }

          # If not first batch, printing lines with different colors based on batch belonging
          if(length(r$monitor$batchFreq)!=1){
            sampsSum = 1

            for(i in 1:length(r$monitor$batchFreq)){
              p <- add_trace(p,
                             x=as.integer(c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))),
                             y=as.double(r$monitor$plotData$DTunique$IPOscore[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]),
                             type="scatter",
                             mode="lines+markers",
                             marker=list(
                               color=graph_colors[i%%5+1]
                             ),
                             line=list(color=graph_colors[i%%5+1]),
                             name=r$monitor$batchFreq[i],
                             text = paste("Batch: ", r$monitor$batchFreq[i], "\n", r$monitor$plotData$sampleNames[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]),
                             color = graph_colors[i%%5+1],
                             colors =graph_colors[i%%5+1],
                             connectgaps=TRUE
              )
              sampsSum=sampsSum+r$monitor$plotData$sampsInBatch[i]#+1
            }

            # If first batch, different printing settings
          } else {
            p <- add_trace(p,
                           x=as.integer(c(1:r$monitor$plotData$sampsInBatch[1])),
                           y=r$monitor$plotData$DTunique$IPOscore,
                           type="scatter",
                           mode="lines+markers",
                           marker=list(
                             color=graph_colors[1]
                           ),
                           line=list(color=graph_colors[1]),
                           name= "IPO score",
                           text = paste("Batch: ", r$monitor$batchFreq[1], "\n", r$monitor$plotData$sampleNames[c(1:r$monitor$plotData$sampsInBatch[1])]),
                           color = graph_colors[1],
                           colors = graph_colors[1]
            )
          }

          # Adding ablines based on chromPol of samples
          if(upper_abline != 0 && lower_abline != 0){
            p <- add_trace(p,
                           x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                           y = upper_abline,
                           type="scatter",
                           mode="lines",
                           line=list(color ="orange"),
                           name="Soft limit"
            )
            p <- add_trace(p,
                           x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                           y = lower_abline,
                           type="scatter",
                           mode="lines",
                           line=list(color ="red"),
                           name="Hard limit"
            )
          }
        }
      })


      ###################################
      ####nLM plot - Checking samples####

      output$nLMPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<1){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotnLM"))
        }
      })

      ####nLM plot - Actual plot####
      output$plotnLM<-renderPlotly({
        if(!is.null(r$monitor$plotData$DTunique)){
          p <- plot_ly(type="scatter", mode="lines+markers")

          #Setting limits based on chromPol
          if(r$monitor$chromPolFormat=="RP"){
            upper_abline <- r$configWiz$config$RPnLMSoftLim
            lower_abline <- r$configWiz$config$RPnLMHardLim
          }
          else if (r$monitor$chromPolFormat=="RN"){
            upper_abline <- r$configWiz$config$RNnLMSoftLim
            lower_abline <- r$configWiz$config$RNnLMHardLim
          }
          else if (r$monitor$chromPolFormat=="HP"){
            upper_abline <- r$configWiz$config$HPnLMSoftLim
            lower_abline <- r$configWiz$config$HPnLMHardLim
          }
          else if (r$monitor$chromPolFormat=="HN"){
            upper_abline <- r$configWiz$config$HNnLMSoftLim
            lower_abline <- r$configWiz$config$HNnLMHardLim
          }

          # If not first batch, printing lines with different colors based on batch belonging
          if(length(r$monitor$batchFreq)!=1){
            sampsSum = 1

            for(i in 1:length(r$monitor$batchFreq)){
              p <- add_trace(p,
                             x=as.integer(c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))),
                             y=as.integer(r$monitor$plotData$DTunique$nLMs[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]),
                             type="scatter",
                             mode="lines+markers",
                             marker=list(
                               color=graph_colors[i%%5+1]
                             ),
                             line=list(color=graph_colors[i%%5+1]),
                             name=r$monitor$batchFreq[i],
                             text = paste("Batch: ", r$monitor$batchFreq[i], "\n", r$monitor$plotData$sampleNames[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]),
                             color = graph_colors[i%%5+1],
                             colors =graph_colors[i%%5+1],
                             connectgaps=TRUE
              )
              sampsSum=sampsSum+r$monitor$plotData$sampsInBatch[i]#+1
            }

            # If first batch, different printing settings
          } else {
            p <- add_trace(p,
                           x=as.integer(c(1:r$monitor$plotData$sampsInBatch[1]),
                                        y=r$monitor$plotData$DTunique$nLMs),
                           type="scatter",
                           mode="lines+markers",
                           marker=list(
                             color=graph_colors[1]
                           ),
                           line=list(color=graph_colors[1]),
                           name= "n LaMas",
                           text = paste("Batch: ", r$monitor$batchFreq[1], "\n", r$monitor$plotData$sampleNames[c(1:r$monitor$plotData$sampsInBatch[1])]),
                           color = graph_colors[1],
                           colors = graph_colors[1]
            )
          }

          # Adding ablines based on chromPol of samples
          if(upper_abline != 0 && lower_abline != 0){
            p <- add_trace(p,
                           x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                           y = upper_abline,
                           type="scatter",
                           mode="lines",
                           line=list(color ="orange"),
                           name="Soft limit"
            )
            p <- add_trace(p,
                           x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                           y = lower_abline,
                           type="scatter",
                           mode="lines",
                           line=list(color ="red"),
                           name="Hard limit"
            )
          }
        }
      })

      ###################################
      ####TIC plot - Checking samples####

      output$TICPlotOrNot<-renderUI({
        req(r$monitor$enoughSamples)
        if(r$monitor$enoughSamples<1){
          tags$b("Too few samples to plot this plot")
        } else {
          plotlyOutput(ns("plotTIC"))
        }
      })

      ####TIC plot - Actual plot####
      output$plotTIC<-renderPlotly({
        if(!is.null(r$monitor$plotData$DTunique)){
          p <- plot_ly(type="scatter", mode="lines+markers")

          #Setting limits based on chromPol
          if(r$monitor$chromPolFormat=="RP"){
            upper_abline <- r$configWiz$config$RPTICSoftLim
            lower_abline <- r$configWiz$config$RPTICHardLim
          }
          else if (r$monitor$chromPolFormat=="RN"){
            upper_abline <- r$configWiz$config$RNTICSoftLim
            lower_abline <- r$configWiz$config$RNTICHardLim
          }
          else if (r$monitor$chromPolFormat=="HP"){
            upper_abline <- r$configWiz$config$HPTICSoftLim
            lower_abline <- r$configWiz$config$HPTICHardLim
          }
          else if (r$monitor$chromPolFormat=="HN"){
            upper_abline <- r$configWiz$config$HNTICSoftLim
            lower_abline <- r$configWiz$config$HNTICHardLim
          }

          # If not first batch, printing lines with different colors based on batch belonging
          if(length(r$monitor$batchFreq)!=1){
            sampsSum = 1

            for(i in 1:length(r$monitor$batchFreq)){
              p <- add_trace(p,
                             x=as.integer(c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))),
                             y=as.double(r$monitor$plotData$DTunique$TIC[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]),
                             type="scatter",
                             mode="lines+markers",
                             marker=list(
                               color=graph_colors[i%%5+1]
                             ),
                             line=list(color=graph_colors[i%%5+1]),
                             name=r$monitor$batchFreq[i],
                             text = paste("Batch: ", r$monitor$batchFreq[i], "\n", r$monitor$plotData$sampleNames[c(sampsSum:(sampsSum+r$monitor$plotData$sampsInBatch[i]))]),
                             color = graph_colors[i%%5+1],
                             colors =graph_colors[i%%5+1],
                             connectgaps=TRUE
              )
              sampsSum=sampsSum+r$monitor$plotData$sampsInBatch[i]#+1
            }

            # If first batch, different printing settings
          } else {
            p <- add_trace(p,
                           x=as.integer(c(1:r$monitor$plotData$sampsInBatch[1])),
                           y=r$monitor$plotData$DTunique$TIC,
                           type="scatter", mode="lines+markers",
                           marker=list(
                             color=graph_colors[1]
                           ),
                           line=list(color=graph_colors[1]),
                           name= "TIC",
                           text = paste("Batch: ", r$monitor$batchFreq[1], "\n", r$monitor$plotData$sampleNames[c(1:r$monitor$plotData$sampsInBatch[1])]),
                           color = graph_colors[1],
                           colors = graph_colors[1]
            )
          }

          # Adding ablines based on chromPol of samples
          if(upper_abline != 0 && lower_abline != 0){
            p <- add_trace(p,
                           x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                           y = upper_abline,
                           type="scatter",
                           mode="lines",
                           line=list(color ="orange"),
                           name="Soft limit"
            )
            p <- add_trace(p, x=as.integer(c(min(r$monitor$plotData$DTunique$sampleNumber), max(r$monitor$plotData$DTunique$sampleNumber))),
                           y = lower_abline,
                           type="scatter",
                           mode="lines",
                           line=list(color ="red"),
                           name="Hard limit"
            )
          }
          p
        }
      })

      ###################################
      ####Bad sample status breakdown####

      # output$badSample_10 <- renderPlot({
      #
      # })
      #
      # output$badSample_all.1 <- renderPlotly({
      #   if(!is.null(r$monitor$plotData$visDataStatus)){
      #     colnames(r$monitor$plotData$visDataStatus.2)<-c("Int outl.", "RT outl.", "Height outl.", "FWHM outl.", "TF outl.", "SN outl.", "DataP. outl.", "Num. LaMas outl.")
      #     heatmaply(r$monitor$plotData$visDataStatus.2, cellnote=r$monitor$plotData$visDataStatus, dendrogram='none', showticklabels=c(T,F))
      #   }
      # })
      #
      # output$badSample_all.2 <- renderPlotly({
      #   if(!is.null(r$monitor$plotData$visDataStatus.3)){
      #     colnames(r$monitor$plotData$visDataStatus.4)<-c("IPO score outl.", "N peaks outl.", "TIC outl.", "Noise outl.")
      #     heatmaply(r$monitor$plotData$visDataStatus.4, cellnote = r$monitor$plotData$visDataStatus.3, dendrogram='none', showticklabels=c(T,F), scale="column") #legend=F,
      #   }
      # })
      #
      # output$badSample_any <- renderPlot({
      #
      # })

      ####################################
      #######Server side operations#######
      ####################################

      #### Config chooser for picking config-file####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$preConfig
        },
        handlerExpr={
          shinyFileChoose(input,"preConfig", root=c(r$configWiz$roots,wd="."), filetypes=c('','txt'), session=session)

          if(!is.null(input$preConfig) && length(grep(".txt",as.character(input$preConfig)))>0){
            fileSelMonitor<-parseFilePaths(r$configWiz$roots,input$preConfig)
            r$configWiz$preConfig<-as.character(fileSelMonitor$datapath)
            r$configWiz$config<-readConfigFile(r$configWiz$preConfig)
            #Setting firstPass to NULL to make sure that DB not continously read if checkLM isn't running
            firstPass(NULL)

            #Checking if file exists
            if(file.exists(r$configWiz$config$dbName)){
              #Checking if LaMas available for all chromPols, if not warn user that samples will be disregarded
              conn1 <- dbConnect(RSQLite::SQLite(), r$configWiz$config$dbName)
              sqliteSetBusyHandler(conn1, 40000)
              s1 <- sprintf("SELECT DISTINCT chromPol FROM landmarks")
              r$monitor$availableChromPols <- unlist(dbGetQuery(conn1, s1))
              dbDisconnect(conn1)

              if(length(r$monitor$availableChromPols) < 4){
                missingChromPols <- c("RP", "RN", "HP", "HN")
                missingChromPols <- missingChromPols[-which(missingChromPols %in% r$monitor$availableChromPols)]

                showModal(
                  modalDialog(
                    title="Warning! Missing LaMas!",
                    paste0("No LaMas found for ", paste(missingChromPols,collapse=", "), "."),
                    paste0("Samples of these chromPols will overlooked during monitoring."),
                    easyClose=TRUE
                  )
                )
              }
            } else {
              showModal(
                modalDialog(
                  title="Warning! No database file detected",
                  paste0("No database found in directory: ", r$configWiz$config$dbName),
                  paste0("Create a .db file from the \"FindLama\" tab"),
                  easyClose=T
                )
              )
            }
          }
        }
      )

      #### Handling start button and everything that follows####

      #Start button event - Running 'FolderMonitoring' in the background
      observeEvent(
        ignoreNULL=T,
        eventExpr={
          input$start
        },
        handlerExpr = {
          time <- Sys.time()

          if(input$start==F){
            r$monitor$start<-F

          } else {
            if(r$runBatch$running==T){
              showModal(
                modalDialog(
                  title="Batch job running",
                  "Can't live monitor while running a batch job. Please abort the batch-job or wait until it's finished to monitor.",
                  easyClose=F
                )
              )
            } else {
              r$monitor$start<-T
              config<-r$configWiz$preConfig
              availableChromPols <- r$monitor$availableChromPols

              #Future async operation that can handle errors from initFolderMonitoring
              fut <- future({
                initFolderMonitoring(config, availableChromPols, progressMonitor=function(i) inter$execInterrupts())
              }, silent=T) %...>%
                (function(result){
                }) %...!%
                (function(error){
                  stopMulticoreFuture(fut)
                  r$monitor$start<-F
                  warning(error)
                })
            }
          }
          NULL

          #Looks weird but only way to snap out of waiting for future
          # if(r$monitor$start==TRUE){}

          #Making sure user input is carried through rerendering of box
          updateSelectInput(
            session = session,
            inputId="chromPol",
            label = "Select chromPol",
            selected = r$monitor$chromPol
          )
          updateSelectInput(
            session = session,
            inputId="sampType",
            selected = r$monitor$sampType
          )
        }
      )

      observeEvent(
        ignoreNULL=T,
        eventExpr={
          r$monitor$start
        },
        handlerExpr={
          req(fut)
          if(r$monitor$start==F){
            inter$interrupt("Stopped monitoring")
            stopMulticoreFuture(fut)
          }
        }
      )

      #### Initialization and storing of sampType, chromPol and getVolumes in r####
      observe({
        r$monitor$sampType <- input$sampType
        r$monitor$chromPolFormat <- input$chromPol
        r$monitor$projName <- input$projName
        changedMode(1)
      })

      #### Live information on what the app is doing ####
      # observe({
      #   invalidateLater(millis=2000, session = session)
      #   if(file.exists("data/status/status.txt") && req(r$monitor$start)==T){
      #     try({
      #       r$monitor$curr_status <- read.table(paste0("data/status/status.txt"))
      #     },
      #     silent=TRUE)
      #   }
      # })

      #### Continuously reading the report-file associated with the current analysis####
      # oldNProject <- reactiveVal(0)
      oldNSamps <- reactiveVal(0)
      changedMode <- reactiveVal(0)

      observe({
        invalidateLater(millis=20000, session = session)
        req(r$monitor$chromPolFormat)
        req(r$monitor$start)
        r$monitor$start

        isolate({
          req(r$configWiz$config$dbName)

          #If DB file exists
          if(file.exists(r$configWiz$config$dbName)){
            s1 <- sprintf("SELECT count(*) FROM sqlite_schema WHERE type='view' AND name='%s'",
                          paste0("QTable_",r$configWiz$config$sampleMatrix))

            conn2 <- dbConnect(RSQLite::SQLite(), r$configWiz$config$dbName)
            sqliteSetBusyHandler(conn2, 40000)
            tableExists <- as.integer(dbGetQuery(conn2, s1))

            #If there is at least one sample of the matrix in the DB
            if(tableExists > 0){
              #Collecting project names present
              s2 <- sprintf("SELECT DISTINCT projName FROM [%s]", # q WHERE q.chromPol='%s'
                            paste0("QTableDistinct_",r$configWiz$config$sampleMatrix))#, #sampleMatrix
              # r$monitor$chromPolFormat,
              # r$monitor$sampType) #chromPol
              # isolate({
              #   r$monitor$projects <- unlist(dbGetQuery(conn, s2))
              #   names(r$monitor$projects) <- NULL
              # })

              #Collecting number of samples
              s3 <- sprintf("SELECT count(*) FROM [%s]",
                            paste0("QTableDistinct_",
                                   r$configWiz$config$sampleMatrix))
              nSampsTrigger <- as.integer(dbGetQuery(conn2, s3))
              firstPass(1)

              #If there is a larger number of samples than previously
              if(nSampsTrigger > oldNSamps() || !is.null(changedMode())){
                oldNSamps(nSampsTrigger)
                dataAvailable(1)
                changedMode(NULL)
              }
            }

            dbDisconnect(conn2)
          }
        })
      })

      #### Using automatically read report-data into the format used for plotting ####
      observe({
        req(dataAvailable())

        if(file.exists(r$configWiz$config$dbName) && r$monitor$projName!=""){

          if(r$monitor$projName != "All samples"){
            s3 <- sprintf("SELECT MAX(sampleIter) FROM [%s] q WHERE q.chromPol='%s' AND q.projName='%s' AND q.type='%s'",
                          paste0("QTable_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          r$monitor$projName,
                          r$monitor$sampType)
          } else {
            s3 <- sprintf("SELECT MAX(sampleIter) FROM [%s] q WHERE q.chromPol='%s' AND q.type='%s'",
                          paste0("QTable_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          r$monitor$sampType)
          }
          conn3 <- dbConnect(RSQLite::SQLite(), r$configWiz$config$dbName)

          #Checking moving window size and number of samples
          nCols <- as.integer(dbGetQuery(conn3, s3))

          if(nCols > r$configWiz$config$nSampsMonitor){
            startNumb <- nCols-r$configWiz$config$nSampsMonitor
          } else {
            startNumb <- 0
          }


          #Reading LaMa related data
          if(r$monitor$projName != "All samples"){
            s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.projName='%s' AND q.sampleIter>%s AND q.type='%s' AND q.status >= 0", #Changed
                          paste0("QTable_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          r$monitor$projName,
                          startNumb,
                          r$monitor$sampType)

            s2 <- sprintf("SELECT i.name, i.nLMs, i.nPeaks, i.TIC, i.IPOscore, i.batchWeek, i.sampleNumber FROM [%s] i WHERE i.chromPol='%s' AND i.projName='%s' AND i.type='%s'",
                          paste0("QTableDistinct_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          r$monitor$projName,
                          r$monitor$sampType)

            s3 <- sprintf("SELECT count(*) FROM [%s] q WHERE q.chromPol='%s' AND q.projName='%s' AND q.type='%s'",
                          paste0("QTableDistinct_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          r$monitor$projName,
                          r$monitor$sampType)
          } else {
            s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>%s AND q.type='%s' AND q.status >= 0", #Changed
                          paste0("QTable_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          startNumb,
                          r$monitor$sampType)

            s2 <- sprintf("SELECT i.name, i.nLMs, i.nPeaks, i.TIC, i.IPOscore, i.batchWeek, i.sampleNumber FROM [%s] i WHERE i.chromPol='%s' AND i.type='%s'",
                          paste0("QTableDistinct_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          r$monitor$sampType)

            s3 <- sprintf("SELECT count(*) FROM [%s] q WHERE q.chromPol='%s' AND q.type='%s'",
                          paste0("QTableDistinct_",r$configWiz$config$sampleMatrix),
                          r$monitor$chromPolFormat,
                          r$monitor$sampType)
          }

          sqliteSetBusyHandler(conn3, 40000)
          visDataPermDT<-as.data.table(dbGetQuery(conn3, s1))
          sampleLevelDT<-as.data.table(dbGetQuery(conn3, s2))
          nSamps<-as.data.table(dbGetQuery(conn3, s3))
          dbDisconnect(conn3)

          #Collecting number of LMs from DB
          if(!is.null(r$monitor$chromPolFormat)){
            r$monitor$nLMs <- nrow(fetchLM(r$configWiz$config$dbName, r$monitor$chromPolFormat))
          }

          if(nrow(visDataPermDT)>0){

            #### MOVE THIS SHIT INTO SQL ####
            r$monitor$batchFreq <- names(table(sampleLevelDT$batchWeek))
            r$monitor$batchFreq <- r$monitor$batchFreq[order(match(r$monitor$batchFreq, unique(sampleLevelDT$batchWeek)))]
            r$monitor$nSamps<- nSamps#max(visDataPermDT[,.(sampleNumber)])
            r$monitor$nBatches<-length(r$monitor$batchFreq)

          } else {
            r$monitor$enoughSamples<-0
            r$monitor$nSamps<-nSamps

            if(nSamps > 0){
              r$monitor$nBatches<-1
            } else {
              r$monitor$nBatches<-0
            }
            dataAvailable(NULL)
            showNotification("Not enough samples to plot data.")
          }

          #### LOADING DATA ####
          if(nrow(visDataPermDT)>0 && !is.null(r$monitor$sampType)){
            # withProgress(message='Plotting data', value=0,{
            sampleNames <- unique(sampleLevelDT$name)

            #If-statement to check how many rows in read data to decide on reading or not plotting certain plots
            if(nrow(sampleLevelDT)<1){
              r$monitor$enoughSamples<-0
            } else if (nrow(sampleLevelDT)==3){
              r$monitor$enoughSamples<-1
            } else {
              r$monitor$enoughSamples<-2
            }

            #If enough samples to make plots
            if(r$monitor$enoughSamples>0){
              #Calculating number of samples in each batch
              sampsInBatch<-rep(0,length(r$monitor$batchFreq))
              for(i in 1:length(r$monitor$batchFreq)){
                sampsInBatch[i]<- as.integer(table(sampleLevelDT$batchWeek)[i])
              }
              names(sampsInBatch) <- names(table(sampleLevelDT$batchWeek))
              sampsInBatch <- sampsInBatch[order(match(names(sampsInBatch), r$monitor$batchFreq))]


              sampsSum<-sampsInBatch[1]
              batchSampsIncr<-vector()
              for(i in 1:(length(sampsInBatch)-1)){
                batchSampsIncr[i]<-sampsSum
                sampsSum<-sampsSum+sampsInBatch[i+1]
              }
              batchSampsIncr[i+1]<-sum(sampsInBatch)

              #### Bad sample status plot ####
              visDataStatus <- NULL
              visDataStatus.2 <- NULL
              visDataStatus.3 <- NULL
              visDataStatus.4 <- NULL

              r$monitor$plotData=list("DTunique"=sampleLevelDT,
                                      "DT"=visDataPermDT,
                                      "sampsInBatch"=sampsInBatch,
                                      "sampleNames" = sampleNames,
                                      "batchSampsIncr"=batchSampsIncr,
                                      "visDataStatus"=visDataStatus,
                                      "visDataStatus.2"=visDataStatus.2,
                                      "visDataStatus.3"=visDataStatus.3,
                                      "visDataStatus.4"=visDataStatus.4)
              dataAvailable(NULL)
            }
          }
        }
      })
    }
  )
}
