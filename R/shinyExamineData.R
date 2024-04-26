examineDataUI<-function(id){
  ns<-NS(id)

  tagList(
    fluidRow(
      column(
        width=6,
        box(title="Settings",
            # status = r$examineData$boxStatus,
            solidHeader=T,
            width=12,

            fluidRow(
              uiOutput(ns("settings"))
            )
        )
      ),
      column(
        width=6,
        box(title="Overview",
            # status=r$examineData$boxStatus,
            solidHeader=T,
            width=12,
            fluidRow(
              uiOutput(ns("infoSamplesBox")),
              uiOutput(ns("infoBatchesBox"))
            )
        )
      )

    ),
    fluidRow(
      column(
        width=6,
        tabBox(
          width=12,
          height="800px",
          tabPanel(
            "Status plot",
            plotlyOutput(ns("plotStatus")), #, click=ns("status_click")
            br(),
            br()
          ),
          tabPanel(
            "Int outlier plot",
            plotlyOutput(ns("plotIntOutlier")),
            br(),
            br()
          ),
          tabPanel(
            "RT outlier plot",
            plotlyOutput(ns("plotRTOutlier")),
            br(),
            br()
          ),
          tabPanel(
            "Height outlier plot",
            plotlyOutput(ns("plotHeightOutlier")),
            br(),
            br()
          ),
          tabPanel(
            "FWHM outlier plot",
            plotlyOutput(ns("plotFWHMOutlier")),
            br(),
            br()
          ),
          tabPanel(
            "TF outlier plot",
            plotlyOutput(ns("plotTFOutlier")),
            br(),
            br()
          ),
          tabPanel(
            "SN outlier plot",
            plotlyOutput(ns("plotSNOutlier")),
            br(),
            br()
          ),
          tabPanel(
            "DataPoints outlier plot",
            plotlyOutput(ns("plotDataPointsOutlier")),
            br(),
            br()
          ),
          tabPanel(
            "Noise outlier plot",
            plotlyOutput(ns("plotNoiseOutlier")),
            br(),
            br()
          )
        )
      )
      ,
      column(
        width=6,
        tabBox(
          width=12,
          height="800px",
          tabPanel(
            "n Peaks plot",
            plotlyOutput(ns("plotPeaks")),
            br(),
            br()
          ),
          tabPanel(
            "IPO plot",
            plotlyOutput(ns("plotIPO")),
            br(),
            br()
          ),
          tabPanel(
            "n Landmarks plot",
            plotlyOutput(ns("plotnLM")),
            br(),
            br()
          ),
          tabPanel(
            "TIC plot",
            plotlyOutput(ns("plotTIC")),
            br(),
            br()
          )
        )
      )
    )
  )

}


#############
###Server####
examineDataServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      graph_colors<-c("green", "red", "darkblue", "orange", "black")

      ####################################
      #######Box UI rendering stuff#######

      ################
      ####Settings####

      output$settings<-renderUI({
        column(
          width=12,

          fluidRow(
            column(
              width=6,

              tags$b("Choose a config file"),
              br(),
              shinyFilesButton(id=ns("configPath"), label="Choose config file", title="", multiple=F)
              # tags$b(r$examineData$preConfig)
            ),
            column(
              width=6,

              selectInput(ns("projName"), "Project", choices=c("All samples", r$examineData$projects), selected=input$projName)
            )
          ),
          fluidRow(
            width=6,

            column(
              width=4,

              selectInput(ns("sampType"), "Injection type", choices=c("sample","sQC"))
            ),
            column(
              width=2,

            ),
            column(
              width=6,

              selectInput(ns("chromPol"), "Chrom & Pol", choices=c("RP","RN","HP","HN"))
            )
          ),
          fluidRow(
            column(
              width=12,

              if(is.null(r$examineData$preConfig)){
                tags$b("No config file loaded.")
              } else {
                tagList(
                  br(),
                  tags$b("Config file: "),
                  r$examineData$preConfig,
                  br()
                )
              }
            )
          )
        )
      })

      #### Status of button-box based on steps taken by user ####
      observe({
        if(is.null(r$examineData$preConfig)){
          r$examineData$boxStatus='danger'
        } else {
          r$examineData$boxStatus='success'
        }
      })

      ####################################
      ####Info boxes output generation####

      #nSamples valueBox
      output$infoSamplesBox<-renderUI({
        if(!is.null(r$examineData$nSamps)){
          tagList(
                 valueBoxOutput(ns("infoSamples"))
          )
        }
      })

      output$infoSamples<-renderInfoBox({
          valueBox(
            r$examineData$nSamps, "Samples", icon=icon("vial"), color="teal"
          )
      })

      #nBatches valueBox
      output$infoBatchesBox<-renderUI({
        if(!is.null(r$examineData$nBatches)){
          tagList(
            valueBoxOutput(ns("infoBatches"))
          )
        }
      })

      output$infoBatches<-renderInfoBox({
        valueBox(
          r$examineData$nBatches, "Batches", icon=icon("vials"), color="orange"
        )
      })




      ###################
      ####Left tabbox####
      ###################
      ####Status plot####
      output$plotStatus<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){
          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          print("Here")

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'status')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)

          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          print("Here2")

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering Status Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      ########################
      ####Int outlier plot####
      output$plotIntOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmIntOutliers')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]

          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering Int outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      #######################
      ####RT outlier plot####
      output$plotRTOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmRTOutliers')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering RT outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      ###########################
      ####Height outlier plot####
      output$plotHeightOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmHeightOutliers')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering Peak height outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      #########################
      ####FWHM outlier plot####
      output$plotFWHMOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmFWHMOutliers')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering FWHM outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      ########################
      ####TF outlier plot####

      output$plotTFOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmTFOutliers')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering Tail. factor outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      ########################
      ####SN outlier plot####

      output$plotSNOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmSNOutliers')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering Sign./Noise outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      ###############################
      ####DataPoints outlier plot####

      output$plotDataPointsOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmDPOutliers')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering Data Point outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })


      ##########################
      ####Noise outlier plot####

      output$plotNoiseOutlier<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)

        s1 <- sprintf("SELECT * FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0", #Changed
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        s2 <- sprintf("SELECT COUNT(*) FROM [%s] q WHERE q.chromPol='%s' AND q.sampleIter>0 AND q.type='%s' AND q.status >= 0",
                      paste0("QTable_",r$examineData$config$sampleMatrix),
                      r$examineData$chromPol,
                      r$examineData$sampType)

        conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)

        if(as.integer(dbGetQuery(conn, s2)) > 0){

          nCols <- max(as.data.table(dbGetQuery(conn,s1))$sampleIter)

          if(nCols > r$examineData$config$nSampsMonitor){
            toRemove <- r$examineData$config$nSampsMonitor
            toShow <- c((nCols-toRemove+3):(nCols+2))
            startNumb <- nCols-(toRemove-1)
          } else {
            toRemove <- 0
            toShow <- c(1:nCols)
            startNumb <- 1
          }

          dcastObj<-dcast(as.data.table(dbGetQuery(conn,s1)),
                          sampleNumber~sampleIter,
                          value.var = 'lmNoise')[c(startNumb:nCols+2), (nCols-ifelse(toRemove==0, (nCols-2), (toRemove-1))):(nCols+1)]
          dbDisconnect(conn)
          if(ncol(dcastObj)==1){
            dcastObj<-cbind(dcastObj,c(NA,NA,NA))
          }

          rownames(dcastObj)<-r$examineData$sampleLevelDT$name[which(r$examineData$sampleLevelDT$sampleNumber %in% toShow)]
          showNotification("Rendering Noise outlier Plot.\n Can take up to 1 minute.")
          heatmaply(dcastObj, dendrogram="none", showticklabels=c(F,F), plotmethod="ggplot")
        } else {
          dbDisconnect(conn)
          NULL
        }
      })



      ###################
      ###Right tab box###
      ###################
      ####nPeaks plot####
      output$plotPeaks<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)
        req(r$examineData$enoughSamples)

        # print(max(r$examineData$sampleLevelDT$sampleNumber))
        # print(r$examineData$sampleLevelDT)
        # print(r$examineData$sampleLevelDT$sampleNumber)

        p <- plot_ly()

        #Setting limits based on chromPol
        if(r$examineData$chromPol=="RP"){
          upper_abline <- r$examineData$config$RPnPeaksSoftLim
          lower_abline <- r$examineData$config$RPnPeaksHardLim
        }
        else if (r$examineData$chromPol=="RN"){
          upper_abline <- r$examineData$config$RNnPeaksSoftLim
          lower_abline <- r$examineData$config$RNnPeaksHardLim
        }
        else if (r$examineData$chromPol=="HP"){
          upper_abline <- r$examineData$config$HPnPeaksSoftLim
          lower_abline <- r$examineData$config$HPnPeaksHardLim
        }
        else if (r$examineData$chromPol=="HN"){
          upper_abline <- r$examineData$config$HNnPeaksSoftLim
          lower_abline <- r$examineData$config$HNnPeaksHardLim
        }

        # If not first batch, printing lines with different colors based on batch belonging
        if (length(r$examineData$batchFreq)!=1){
          sampsSum = 1

          for(i in 1:length(r$examineData$batchFreq)){

            p<-add_trace(p, x=c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i])), y=r$examineData$sampleLevelDT$nPeaks[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))], type="scatter", mode="lines+markers", #-1, -1
                         marker=list(
                           color=graph_colors[i%%5+1]
                         ),
                         line=list(color=graph_colors[i%%5+1]),
                         name=r$examineData$batchFreq[i],
                         text = paste("Batch: ", r$examineData$batchFreq[i], "\n", r$examineData$sampleLevelDT$name[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))]), #-1
                         color = graph_colors[i%%5+1],
                         colors = graph_colors[i%%5+1],
                         connectgaps=TRUE
            )
            sampsSum=sampsSum+r$examineData$sampsInBatch[i]
          }

          # If first batch, different printing settings
        } else {
          p <- add_trace(p, x=c(1:r$examineData$sampsInBatch[1]), y=r$examineData$sampleLevelDT$nPeaks, type="scatter", mode="lines+markers",
                         marker=list(
                           color=graph_colors[1]
                         ),
                         line=list(color=graph_colors[1]),
                         name= "nPeaks",
                         text = ~paste("Batch: ", r$examineData$batchFreq[1], "\n", r$examineData$plotData$sampleNames[c(1:r$examineData$sampsInBatch[1])]),
                         color = graph_colors[1],
                         colors = graph_colors[1]
          )
        }
        # Adding ablines based on chromPol of samples
        if(upper_abline != 0 && lower_abline != 0){
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = upper_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="orange"),
                       name="Soft limit"
          )
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = lower_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="red"),
                       name="Hard limit"
          )
        }
      })


      ################
      ####IPO plot####
      output$plotIPO<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)
        req(r$examineData$enoughSamples)

        p <- plot_ly()

        #Setting limits based on chromPol
        if(r$examineData$chromPol=="RP"){
          upper_abline <- r$examineData$config$RPIPOSoftLim
          lower_abline <- r$examineData$config$RPIPOHardLim
        }
        else if (r$examineData$chromPol=="RN"){
          upper_abline <- r$examineData$config$RNIPOSoftLim
          lower_abline <- r$examineData$config$RNIPOHardLim
        }
        else if (r$examineData$chromPol=="HP"){
          upper_abline <- r$examineData$config$HPIPOSoftLim
          lower_abline <- r$examineData$config$HPIPOHardLim
        }
        else if (r$examineData$chromPol=="HN"){
          upper_abline <- r$examineData$config$HNIPOSoftLim
          lower_abline <- r$examineData$config$HNIPOHardLim
        }

        # If not first batch, printing lines with different colors based on batch belonging
        if(length(r$examineData$batchFreq)!=1){
          sampsSum = 1

          for(i in 1:length(r$examineData$batchFreq)){

            p<-add_trace(p, x=c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i])), y=r$examineData$sampleLevelDT$IPOscore[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))], type="scatter", mode="lines+markers", #-1, -1
                         marker=list(
                           color=graph_colors[i%%5+1]
                         ),
                         line=list(color=graph_colors[i%%5+1]),
                         name=r$examineData$batchFreq[i],
                         text = paste("Batch: ", r$examineData$batchFreq[i], "\n", r$examineData$sampleLevelDT$name[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))]), #-1
                         color = graph_colors[i%%5+1],
                         colors = graph_colors[i%%5+1],
                         connectgaps=TRUE
            )
            sampsSum=sampsSum+r$examineData$sampsInBatch[i]
          }

          # If first batch, different printing settings
        } else {
          p <- add_trace(p, x=c(1:r$examineData$sampsInBatch[1]), y=r$examineData$sampleLevelDT$IPOscore, type="scatter", mode="lines+markers",
                         marker=list(
                           color=graph_colors[1]
                         ),
                         line=list(color=graph_colors[1]),
                         name= "IPO score",
                         text = ~paste("Batch: ", r$examineData$batchFreq[1], "\n", r$examineData$plotData$sampleNames[c(1:r$examineData$sampsInBatch[1])]),
                         color = graph_colors[1],
                         colors = graph_colors[1]
          )
        }
        # Adding ablines based on chromPol of samples
        if(upper_abline != 0 && lower_abline != 0){
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = upper_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="orange"),
                       name="Soft limit"
          )
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = lower_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="red"),
                       name="Hard limit"
          )
        }
      })


      ################
      ####nLM plot####
      output$plotnLM<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)
        req(r$examineData$enoughSamples)

        p <- plot_ly()

        #Setting limits based on chromPol
        if(r$examineData$chromPol=="RP"){
          upper_abline <- r$examineData$config$RPnLMSoftLim
          lower_abline <- r$examineData$config$RPnLMHardLim
        }
        else if (r$examineData$chromPol=="RN"){
          upper_abline <- r$examineData$config$RNnLMSoftLim
          lower_abline <- r$examineData$config$RNnLMHardLim
        }
        else if (r$examineData$chromPol=="HP"){
          upper_abline <- r$examineData$config$HPnLMSoftLim
          lower_abline <- r$examineData$config$HPnLMHardLim
        }
        else if (r$examineData$chromPol=="HN"){
          upper_abline <- r$examineData$config$HNnLMSoftLim
          lower_abline <- r$examineData$config$HNnLMHardLim
        }

        # If not first batch, printing lines with different colors based on batch belonging
        if(length(r$examineData$batchFreq)!=1){
          sampsSum = 1

          for(i in 1:length(r$examineData$batchFreq)){

            p<-add_trace(p, x=c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i])), y=r$examineData$sampleLevelDT$nLMs[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))], type="scatter", mode="lines+markers", #-1, -1
                         marker=list(
                           color=graph_colors[i%%5+1]
                         ),
                         line=list(color=graph_colors[i%%5+1]),
                         name=r$examineData$batchFreq[i],
                         text = paste("Batch: ", r$examineData$batchFreq[i], "\n", r$examineData$sampleLevelDT$name[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))]), #-1
                         color = graph_colors[i%%5+1],
                         colors = graph_colors[i%%5+1],
                         connectgaps=TRUE
            )
            sampsSum=sampsSum+r$examineData$sampsInBatch[i]
          }

          # If first batch, different printing settings
        } else {
          p <- add_trace(p, x=c(1:r$examineData$sampsInBatch[1]), y=r$examineData$sampleLevelDT$nLMs, type="scatter", mode="lines+markers",
                         marker=list(
                           color=graph_colors[1]
                         ),
                         line=list(color=graph_colors[1]),
                         name= "nLaMas",
                         text = ~paste("Batch: ", r$examineData$batchFreq[1], "\n", r$examineData$plotData$sampleNames[c(1:r$examineData$sampsInBatch[1])]),
                         color = graph_colors[1],
                         colors = graph_colors[1]
          )
        }
        # Adding ablines based on chromPol of samples
        if(upper_abline != 0 && lower_abline != 0){
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = upper_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="orange"),
                       name="Soft limit"
          )
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = lower_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="red"),
                       name="Hard limit"
          )
        }
      })


      ################
      ####TIC plot####
      output$plotTIC<-renderPlotly({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(r$examineData$sampType)
        req(r$examineData$chromPol)
        req(r$examineData$enoughSamples)

        p <- plot_ly()

        #Setting limits based on chromPol
        if(r$examineData$chromPol=="RP"){
          upper_abline <- r$examineData$config$RPTICSoftLim
          lower_abline <- r$examineData$config$RPTICHardLim
        }
        else if (r$examineData$chromPol=="RN"){
          upper_abline <- r$examineData$config$RNTICSoftLim
          lower_abline <- r$examineData$config$RNTICHardLim
        }
        else if (r$examineData$chromPol=="HP"){
          upper_abline <- r$examineData$config$HPTICSoftLim
          lower_abline <- r$examineData$config$HPTICHardLim
        }
        else if (r$examineData$chromPol=="HN"){
          upper_abline <- r$examineData$config$HNTICSoftLim
          lower_abline <- r$examineData$config$HNTICHardLim
        }

        # If not first batch, printing lines with different colors based on batch belonging
        if(length(r$examineData$batchFreq)!=1){
          sampsSum = 1

          for(i in 1:length(r$examineData$batchFreq)){

            p<-add_trace(p,
                         x=c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i])),
                         y=r$examineData$sampleLevelDT$TIC[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))],
                         type="scatter",
                         mode="lines+markers", #-1, -1
                         marker=list(
                           color=graph_colors[i%%5+1]
                         ),
                         line=list(color=graph_colors[i%%5+1]),
                         name=r$examineData$batchFreq[i],
                         text = paste("Batch: ", r$examineData$batchFreq[i], "\n", r$examineData$sampleLevelDT$name[c(sampsSum:(sampsSum+r$examineData$sampsInBatch[i]))]), #-1
                         color = graph_colors[i%%5+1],
                         colors = graph_colors[i%%5+1],
                         connectgaps=TRUE
            )
            sampsSum=sampsSum+r$examineData$sampsInBatch[i]
          }

          # If first batch, different printing settings
        } else {
          p <- add_trace(p,
                         x=c(1:r$examineData$sampsInBatch[1]),
                         y=r$examineData$sampleLevelDT$TIC,
                         type="scatter",
                         mode="lines+markers",
                         marker=list(
                           color=graph_colors[1]
                         ),
                         line=list(color=graph_colors[1]),
                         name= "log TIC",
                         text = ~paste("Batch: ", r$examineData$batchFreq[1], "\n", r$examineData$plotData$sampleNames[c(1:r$examineData$sampsInBatch[1])]),
                         color = graph_colors[1],
                         colors = graph_colors[1]
          )
        }
        # Adding ablines based on chromPol of samples
        if(upper_abline != 0 && lower_abline != 0){
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = upper_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="orange"),
                       name="Soft limit"
          )
          p<-add_trace(p, x=c(min(r$examineData$sampleLevelDT$sampleNumber), nrow(r$examineData$sampleLevelDT)), y = lower_abline, type="scatter", mode="lines", #max(r$examineData$sampleLevelDT$sampleNumber)
                       line=list(color ="red"),
                       name="Hard limit"
          )
        }
      })


      ####################################
      #######Server side operations#######

      ####Initialization and storing of sampType, chromPol and getVolumes in r ####
      observe({
        req(r$examineData$config)
        req(input$sampType)
        req(input$chromPol)
        req(input$projName)

        r$examineData$sampType<-input$sampType
        r$examineData$chromPol<-input$chromPol
        r$examineData$projName<-input$projName

        isolate({
          if(file.exists(r$examineData$config$dbName)){

            #Reading LaMa related data
            if(r$examineData$projName != "All samples"){

              s1 <- sprintf("SELECT i.name, i.nLMs, i.nPeaks, i.TIC, i.IPOscore, i.batchWeek, i.sampleNumber FROM [%s] i WHERE i.chromPol='%s' AND i.projName='%s' AND i.type='%s'",
                            paste0("QTableDistinct_",r$examineData$config$sampleMatrix),
                            r$examineData$chromPol,
                            r$examineData$projName,
                            r$examineData$sampType)

              s2 <- sprintf("SELECT count(*) FROM [%s] q WHERE q.chromPol='%s' AND q.projName='%s' AND q.type='%s'",
                            paste0("QTableDistinct_",r$examineData$config$sampleMatrix),
                            r$examineData$chromPol,
                            r$examineData$projName,
                            r$examineData$sampType)
            } else {

              s1 <- sprintf("SELECT i.name, i.nLMs, i.nPeaks, i.TIC, i.IPOscore, i.batchWeek, i.sampleNumber FROM [%s] i WHERE i.chromPol='%s' AND i.type='%s'",
                            paste0("QTableDistinct_",r$examineData$config$sampleMatrix),
                            r$examineData$chromPol,
                            r$examineData$sampType)

              s2 <- sprintf("SELECT count(*) FROM [%s] q WHERE q.chromPol='%s' AND q.type='%s'",
                            paste0("QTableDistinct_",r$examineData$config$sampleMatrix),
                            r$examineData$chromPol,
                            r$examineData$sampType)
            }


            conn <- dbConnect(RSQLite::SQLite(), r$examineData$config$dbName)
            r$examineData$sampleLevelDT<-as.data.table(dbGetQuery(conn, s1))
            r$examineData$nSamps <- as.integer(dbGetQuery(conn, s2))

            dbDisconnect(conn)

            if(r$examineData$nSamps > 2){
              r$examineData$enoughSamples <- 1
              r$examineData$batchFreq <- names(table(r$examineData$sampleLevelDT$batchWeek))
              r$examineData$batchFreq <- r$examineData$batchFreq[order(match(r$examineData$batchFreq, unique(r$examineData$sampleLevelDT$batchWeek)))]
              r$examineData$nBatches <- length(r$examineData$batchFreq)

              r$examineData$sampsInBatch<-rep(0,length(r$examineData$batchFreq))
              for(i in 1:length(r$examineData$batchFreq)){
                r$examineData$sampsInBatch[i]<- as.integer(table(r$examineData$sampleLevelDT$batchWeek)[i])
              }

              names(r$examineData$sampsInBatch) <- names(table(r$examineData$sampleLevelDT$batchWeek))
              r$examineData$sampsInBatch <- r$examineData$sampsInBatch[order(match(names(r$examineData$sampsInBatch), r$examineData$batchFreq))]

            } else {
              # r$examineData$enoughSamples<-0
              # r$examineData$nSamps<-nSamps

              if(r$examineData$nSamps > 0){
                r$examineData$nBatches<-1
              } else {
                r$examineData$nBatches<-0
              }

              showNotification("Not enough samples to plot data.")
            }
          }
        })
      })

      #### Choosing the config file to evaluate ####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$configPath
        },
        handlerExpr={
          shinyFileChoose(input,"configPath", root=c(r$configWiz$roots,wd="."), filetypes=c('','txt'), session=session)

          if(!is.null(input$configPath) && length(grep(".txt",as.character(input$configPath)))>0){
            fileSelMonitor<-parseFilePaths(r$configWiz$roots,input$configPath)
            r$examineData$preConfig<-as.character(fileSelMonitor$datapath)
            r$examineData$config <- readConfigFile(r$examineData$preConfig)
          }
      }
    )
    }
  )
}
