chromatogramUI<-function(id){
  ns<-NS(id)

  tagList(
    fluidRow(
      column(
        width=12,
        uiOutput(ns('file1Box'))
      )
    ),
    fluidRow(
      column(
        width=12,
        uiOutput(ns('file2Box'))
      )
    )
  )

}

chromatogramServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      graph_colors<-c("green", "red", "darkblue", "orange", "black")

      chroms <- reactiveVal()

      ##################################
      #######Rendering stuff############
      #### File 1 render ####
      output$file1Box <- renderUI({

        box(title=r$chroms$box1Title,
            solidHeader=T,
            width=12,
            fluidRow(
              column(
                width=6,
                if(!is.null(r$chroms$LMs)){
                  tagList(
                    selectInput(ns("samp1"), "Select samples to view", choices = r$monitor$plotData$sampleNames, multiple=TRUE, selected=r$monitor$plotData$sampleNames[length(r$monitor$plotData$sampleNames)]), #
                    selectInput(ns("LaMas1"), "Select the LaMa to view", choices = r$chroms$LMs$LMName),
                  )
                }
              ),
              column(
                width=6,
                if(!is.null(r$chroms$LMs)){
                  tagList(
                    sliderInput(ns("y_offset"), "Select offset of chromatograms on y-axis", min=0, max=100000, step=5000, value=20000, ticks=F),
                    sliderInput(ns("x_offset"), "Select offset of chromatograms on x-axis", min=0, max=15, step=1, value=0, ticks=F)
                  )
                }
              )
          ),
          fluidRow(
            column(
              width=6,

            ),
            column(
              width=6,

              actionButton(ns('loadChroms'), "Load\n chromatograms")
            )
          ),
          fluidRow(
            column(
              width=12,
              if(!is.null(chroms())){
                plotlyOutput(ns("file1"))
              }
            )
          )
        )
      })

      #### Rendering file 1 ####
      output$file1 <- renderPlotly({

            chrom_plot <- plot_ly()

            LMToDisplay <- as.double(r$chroms$LMs$LMID[which(r$chroms$LMs$LMName==input$LaMas1)])-min(as.double(r$chroms$LMs$LMID))+1

            for(i in 1:length(r$chroms$loadedFiles)){
              # SampToDisplay <- as.double(which(r$monitor$plotData$sampleNames==input$samp1[i]))
              print(i)
              print(rtime(chroms()[[i]][LMToDisplay, ])+((i-1)*input$x_offset)) #SampToDisplay
              chrom_plot <- add_trace(chrom_plot,
                                      x=rtime(chroms()[[i]][LMToDisplay, ])+((length(r$chroms$loadedFiles)-i)*input$x_offset),
                                      y=intensity(chroms()[[i]][LMToDisplay, ])+((length(r$chroms$loadedFiles)-i)*input$y_offset),
                                      marker=list(color=graph_colors[i %% 5]),
                                      line=list(color=graph_colors[i %% 5]),
                                      type = "scatter",
                                      mode = "markers+lines",
                                      name=r$chroms$loadedFiles[i])

            }

            chrom_plot
      })


      ###################################
      ######## Server side stuff ########

      #### Changing name on box to file name and finding paths to all files to read ####
      # Add if file number changes after initial reading of all files (check monitor, is some stuff there already for report)
      # Keep track of which files have been read and only read new files -> vector with boolean trigger
        #This trigger can be used directly in for-loop as in my R speed presentation

      observe({
        if(!is.null(r$configWiz$config)){
          r$chroms$box1Title<-paste0(r$monitor$projName, " - " , r$monitor$chromPolFormat)
          r$chroms$LMs <- fetchLM(dbName = as.character(r$configWiz$config$dbName), chromPol = as.character(r$monitor$chromPolFormat))

          if(r$monitor$projName == "All samples"){
            s1 <- sprintf("SELECT name FROM [IPOnLMs] i WHERE i.chromPol = '%s' AND type = '%s'",
                          r$monitor$chromPolFormat,
                          r$monitor$sampType
            )
          } else {
            s1 <- sprintf("SELECT name FROM [IPOnLMs] i WHERE i.chromPol = '%s' AND type = '%s' AND projName = '%s'",
                          r$monitor$chromPolFormat,
                          r$monitor$sampType,
                          r$monitor$projName)
          }

          conn <- dbConnect(RSQLite::SQLite(), r$configWiz$config$dbName)
          sqliteSetBusyHandler(conn, 10000)
          r$chroms$sampNames <- dbGetQuery(conn, s1)
          dbDisconnect(conn)

        } else {

          r$chroms$box1Title<-paste0("No config loaded in monitoring")

        }
      })

      #### Load chromatograms ####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$loadChroms
        },
        handlerExpr={
          # condition <- !(r$chroms$fileDirs %in% r$chroms$prevFiles)
          #print("In reactive")

          #chroms <- r$chroms$chroms
          allmzMLFiles <- Sys.glob(paste0(r$configWiz$config$outdir,"/", paste0(strsplit(r$configWiz$config$folderDepth,".d")[1], ".mzML")))
          fileDirs <- allmzMLFiles[which(apply(sapply(input$samp1, grepl, allmzMLFiles), 1, any))]
          r$chroms$loadedFiles <- input$samp1
          chroms_temp <- as.list(chroms())

          if(!is.null(r$chroms$LMs)){

            r$chroms$LMs$LMmz <- as.double(r$chroms$LMs$LMmz)
            r$chroms$LMs$LMRT <- as.double(r$chroms$LMs$LMRT)

            LMformatMZ <- cbind((r$chroms$LMs$LMmz + r$chroms$LMs$LMmz*(30/1000000)), (r$chroms$LMs$LMmz - r$chroms$LMs$LMmz*(30/1000000)))
            LMformatRT <- cbind(((r$chroms$LMs$LMRT - 50)), ((r$chroms$LMs$LMRT + 50)))
            LMformatRT[which(LMformatRT[,1]<0),1] <- 0

            chromLoaderProgress <- AsyncProgress$new(message="Loading chromatograms")

            future_promise({
              for(i in (1:length(fileDirs))){
                chroms_temp[[i]] <- chromatogram(rt=LMformatRT, mz=LMformatMZ, readMSData(fileDirs[i], mode="onDisk"))

                chromLoaderProgress$inc(1/length(fileDirs))
              }

              chromLoaderProgress$close()
              chroms_temp
            }) %...>% chroms()
            NULL

          }


      })
    }
  )
}
