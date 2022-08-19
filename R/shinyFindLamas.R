findLamasUI<-function(id){
  ns<-NS(id)
  tagList(
    #First row of boxes for making a new DB-file and for inputting all settings for optimizLM
    fluidRow(
      box(
        title="Choose / Create database file",
        width=8,
        solidHeader=T,

        fluidRow(
          column(
            width=5,

            textInput(ns('DBName'), label="Choose name for new db file"),
            uiOutput(ns('createDB'))
          ),
          column(
            width=7,

            tags$b("Choose the folder where DB file will be created"),
            br(),
            shinyDirButton(ns('DBDir'), title="", label="Choose folder")
          )
        ),
        fluidRow(
          column(
            width=12,

            div(style="text-align:right;",
                uiOutput(ns('DBFeedback'))
            )
          )
        )
      )
    ),
    #Second row of boxes for inputting all settings for optimizeLM
    fluidRow(
      box(
        title="Input parameters for finding LaMas",
        width=8,
        solidHeader=T,

        fluidRow(
          column(
            width=6,
            uiOutput(ns('rdsDescription')),
            shinyFilesButton(id=ns('preFCPFilepath'), label="Choose .rds object", title="Choose .rds object containing xcms RT adj. object", multiple=F)
          ),
          column(
            width=6,
            textInput(ns('split'), label="Input delimiter for mz & RT (e.g. \'@\')")
          )
        ),
        fluidRow(
          column(
            width=4,
            numericInput(inputId=ns('minRTcheck'), label="The RT (s) from where to start looking for LaMas", value=40, step=1, min=0)
          ),
          column(
            width=4,
            numericInput(inputId=ns('preFilterIntensity'), label="Prefilter intensity used for XCMS processing", value=1500, step=10, min=0)
          ),
          column(
            width=4,
            selectInput(inputId=ns('chromPol'), label="Select the chrom. and pol. of the data", choices=c("RP","RN","HP","HN"))
          )
        ),
        fluidRow(
          column(
            width=4,
            sliderInput(inputId=ns("minMaxLamas"), "Min. and max n LaMas to find",
                        min = 10, max = 300,
                        value = c(100,200))
          ),
          column(
            width=8
          )
        ),
        fluidRow(
          column(
            width=6
          ),
          column(
            width=6,
            div(style="text-align:right;", #!important;
              uiOutput(ns('runButton'))
            )
          )
        )
      )
    ),
    #Third row for looking at plots and determining which LMs to keep and choosing which LaMa-set to use
    fluidRow(
      uiOutput(ns('reviewLamas')),
      box(
        title="Save opt. data and submit LaMas to DB",
        width=4,
        solidHeader=T,

        fluidRow(
          column(
            width=12,
            textInput(ns('saveOptName'), , label="Name for opt. output save file (.rds)")
          )
        ),
        fluidRow(
          column(
            width=12,
            actionButton(ns('saveOpt'), label="Save opt. data"),
            shinyFilesButton(id=ns('loadOpt'), label="Load old .rds object", title="Choose .rds object containing opt. data", multiple=F)
          )
        ),
        tags$br(),
        tags$br(),
        fluidRow(
          #tags$b("Choose .db file to submit LaMas to"),
          column(
            width=12,
            selectInput(ns('lamaSetToSubmit'), label="Select LaMa-set to submit", choices=c(1:10)),
            shinyFilesButton(id=ns('dbSubmitLamasTo'), label="Choose .db file to submit LaMas to", title="Choose .db file to submit LaMas to", multiple=F)
          )
        ),
        fluidRow(
          column(
            width=12,

            uiOutput(ns('submitLamasRender'))
          )
        )
      )
      #uiOutput(ns('saveAndSubmit'))
    )
  )
}

findLamasServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      ###############################
      #### Server side rendering ####
      ###############################
      ####RDS description and feedback####
      output$rdsDescription <- renderUI({
        if(is.null(r$findLamas$preFCPfilepath)){
          tags$b("Choose .rds object containing xcms RT adj. object")
        } else {
          tags$b(paste0("File: ", strsplit(r$findLamas$preFCPfilepath,"/")[[1]][length(strsplit(r$findLamas$preFCPfilepath,"/")[[1]])]))
        }

      })


      ####Button for finding new LaMas####
      output$runButton <- renderUI({
        req(r$findLamas$preFCPfilepath)
        req(r$findLamas$split)

        if(r$findLamas$split !="" && r$findLamas$preFCPfilepath != ""){
          if(is.null(r$findLamas$startOptimize)){
            tagList(
              tags$b("Optimization takes up to 2 hours"),
              actionButton(ns('startOptimize'), "Start optimization")
            )
          }
        }
      })

      #### DB related rendering ####
      #Button for creating new DB file
      output$createDB <- renderUI({
        req(r$findLamas$DBName)
        req(r$findLamas$DBDir)

        fluidRow(
          column(
            width=12,

            if(r$findLamas$DBName != "" && r$findLamas$DBDir != ""){
              actionButton(ns('createDBButton'), "Create new DB file")
            },
            paste0(r$findLamas$DBDir,"/",r$findLamas$DBName)
          )
        )
      })

      #Button for submitting LaMas to db
      output$submitLamasRender <- renderUI({
        req(r$findLamas$dbSubmitLamasTo)

        tagList(
          div(style="text-align:left;",
            tags$br(),
            tags$b(r$findLamas$dbSubmitLamasTo)
          ),

          div(style="text-align:right;",
            if(is.null(r$findLamas$submitLamas) && !is.null(r$findLamas$outObj)){
              actionButton(inputId=ns('submitLamas'), label="Submit LaMas to db")
            }
          )
        )
      })

      output$DBFeedback <- renderUI({
        req(r$findLamas$DBFeedbackMSG)

        tags$b(r$findLamas$DBFeedbackMSG)
      })

      ##### Tabbox for deciding on which LaMas to use #####
      output$reviewLamas <- renderUI({
        tabBox(
          title="Optimization plots",
          width=8,

          tabPanel("LaMa-set #1",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet1'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                       column(
                         width=4,
                         tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[1]][1],0))),
                         tags$br(),
                         tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[1]][2],0))),
                         tags$br(),
                         tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[1]][3],0))),
                         tags$br(),
                         tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[1]][4],0))),
                         tags$br(),
                         tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[1]]))),
                         tags$br(),
                         tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[1]][5]),3)
                       )
                     }
                   )
          ),
          tabPanel("LaMa-set #2",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet2'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[2]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[2]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[2]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[2]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[2]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[2]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #3",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet3'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[3]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[3]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[3]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[3]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[3]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[3]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #4",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet4'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[4]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[4]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[4]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[4]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[4]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[4]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #5",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet5'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[5]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[5]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[5]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[5]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[5]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[5]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #6",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet6'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[6]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[6]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[6]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[6]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[6]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[6]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #7",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet7'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[7]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[7]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[7]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[7]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[7]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[7]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #8",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet8'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[8]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[8]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[8]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[8]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[8]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[8]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #9",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet9'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[9]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[9]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[9]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[9]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[9]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[9]][5]),3)
                     )
                     }
                   )
          ),
          tabPanel("LaMa-set #10",
                   fluidRow(
                     column(
                       width=8,
                       plotlyOutput(ns('lamaSet10'))
                     ),
                     if(!is.null(r$findLamas$outObj)){
                     column(
                       width=4,
                       tags$b(paste0("Min. intensity: ", round(r$findLamas$outObj[[2]][[10]][1],0))),
                       tags$br(),
                       tags$b(paste0("Allowed missingness: ", round(r$findLamas$outObj[[2]][[10]][2],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in mz: ", round(r$findLamas$outObj[[2]][[10]][3],0))),
                       tags$br(),
                       tags$b(paste0("Diff. in rt: ", round(r$findLamas$outObj[[2]][[10]][4],0))),
                       tags$br(),
                       tags$b(paste0("Number of LaMas: ", nrow(r$findLamas$outObj[[1]][[10]]))),
                       tags$br(),
                       tags$b(paste0("RT coverage of LaMas: "), round(r$findLamas$outObj[[2]][[10]][5]),3)
                     )
                     }
                   )
          )
        )
      })

      #### All set plots for the optimizations ####
      #lamaSet1
      output$lamaSet1 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[1]]$Landmarkrts, y=r$findLamas$outObj[[1]][[1]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet2
      output$lamaSet2 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[2]]$Landmarkrts, y=r$findLamas$outObj[[1]][[2]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet3
      output$lamaSet3 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[3]]$Landmarkrts, y=r$findLamas$outObj[[1]][[3]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet4
      output$lamaSet4 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[4]]$Landmarkrts, y=r$findLamas$outObj[[1]][[4]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet5
      output$lamaSet5 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[5]]$Landmarkrts, y=r$findLamas$outObj[[1]][[5]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet6
      output$lamaSet6 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[6]]$Landmarkrts, y=r$findLamas$outObj[[1]][[6]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet7
      output$lamaSet7 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[7]]$Landmarkrts, y=r$findLamas$outObj[[1]][[7]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet8
      output$lamaSet8 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[8]]$Landmarkrts, y=r$findLamas$outObj[[1]][[8]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet9
      output$lamaSet9 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[9]]$Landmarkrts, y=r$findLamas$outObj[[1]][[9]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })
      #lamaSet10
      output$lamaSet10 <- renderPlotly({
        req(r$findLamas$outObj)
        p <- plot_ly()
        p <- add_markers(p, x=r$findLamas$outObj[[1]][[10]]$Landmarkrts, y=r$findLamas$outObj[[1]][[10]]$Landmarkmzs, type="scatter", mode="markers")
        p <- layout(p, showlegend=F, xaxis = list(title = 'LaMa RT (sec)'), yaxis = list(title = 'LaMa m/z'))
      })


      #################################################
      #### Handling all parameter inputs from user ####
      #################################################
      #### Save and submit LaMas to DB ####

      #Format name for .rds-file to save optimization data
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$saveOptName
        },
        handlerExpr={
          if(grepl(".rds", input$saveOptName)==TRUE){
            r$findLamas$saveOptName <- input$saveOptName
          } else {
            r$findLamas$saveOptName <- paste0(input$saveOptName,".rds")
          }
        }
      )

      #Save optimized LaMas to .rda file
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$saveOpt
        },
        handlerExpr={
          req(r$findLamas$saveOptName)
          req(r$findLamas$outObj)

          if(input$saveOptName != ""){
            r$findLamas$outObj$params<-list(split=r$findLamas$split, preFilter=r$findLamas$preFilterIntensity, minRT=r$findLamas$minRTcheck, chromPol=r$findLamas$chromPol)
            saveRDS(r$findLamas$outObj, file=r$findLamas$saveOptName)
            showNotification(".rds object saved")
          }
        }
      )

      #Load .rds-file containing optimization data
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$loadOpt
        },
        handlerExpr={
          shinyFileChoose(input,'loadOpt', roots=c(r$configWiz$roots, wd="."), filetypes=c('','rds'), session=session)

          if(!is.null(input$loadOpt) && length(grep(".rds",as.character(input$loadOpt)))>0){
            fileSelMonitor <- parseFilePaths(r$configWiz$roots,input$loadOpt)
            r$findLamas$loadOpt <- as.character(fileSelMonitor$datapath)
            r$findLamas$outObj <- readRDS(r$findLamas$loadOpt)

            #Updating all settings based on save file, will automatically trigger input into r$findLamas for all of them as well
            updateTextInput(inputId = 'saveOptName', value=strsplit(r$findLamas$loadOpt, "//")[[1]][length(strsplit(r$findLamas$loadOpt, "//")[[1]])])
            updateTextInput(inputId = 'split', value=r$findLamas$outObj$params$split)
            updateNumericInput(inputId = 'preFilterIntensity', value=r$findLamas$outObj$params$preFilter)
            updateSelectInput(inputId = 'chromPol', selected=r$findLamas$outObj$params$chromPol)
            updateNumericInput(inputId = 'minRTcheck', value=r$findLamas$outObj$params$minRT)
          }
        }
      )

      #Choosing a set and submitting it to a .db-file
      observe({
          r$findLamas$lamaSetToSubmit <- as.double(input$lamaSetToSubmit)
      })

      #Choose .db file to upload to
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dbSubmitLamasTo
        },
        handlerExpr = {
          shinyFileChoose(input,'dbSubmitLamasTo', roots=c(r$configWiz$roots, wd="."), filetypes=c('','db'), session=session)

          if(!is.null(input$dbSubmitLamasTo) && grepl(".db",as.character(input$dbSubmitLamasTo))){
            fileSelMonitor <- parseFilePaths(r$configWiz$roots,input$dbSubmitLamasTo)
            r$findLamas$dbSubmitLamasTo <- as.character(fileSelMonitor$datapath)
          }
        }
      )

      #Modal if LaMas already exist in DB to make sure user isn't adding the same ones again
      lamasInDBModal <- function() {
        modalDialog(
          title="LaMas already exist for this chromPol in DB!",
          "LaMas already exist for the selected chromPol in the DB. Are you sure you want to add more?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("addMoreLamas"), "OK")
          )
        )
      }

      #If user is ok with adding more LaMas although they already exist in DB
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$addMoreLamas
        },
        handlerExpr={
          nrowToSubmit <- nrow(r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]])

          landmarks<-data.frame(LMID=integer(nrowToSubmit),
                                LMmz=double(nrowToSubmit),
                                LMRT=double(nrowToSubmit),
                                chromPol=character(nrowToSubmit),
                                LMName=character(nrowToSubmit))

          landmarks[,2] <- r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]][,1]
          landmarks[,3] <- r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]][,2]
          landmarks[,4] <- rep(r$findLamas$outObj[[3]],nrow(r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]]))
          landmarks[,5] <- paste0(landmarks[,2],"_",landmarks[,3])

          print(landmarks)

          submitLMToDB(r$findLamas$dbSubmitLamasTo, landmarks)

          r$findLamas$submitLamas <- 1
          showNotification("LaMas submitted")
          removeModal()
        }
      )

      #Submit to .db
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$submitLamas
        },
        handlerExpr={
          req(input$submitLamas)
          if(grepl(".db",r$findLamas$dbSubmitLamasTo)){
            #Double checking if LaMas already present and asking user to confirm that they want to submit new ones
            if(dim(fetchLM(r$findLamas$dbSubmitLamasTo,r$findLamas$chromPol))[1]>0){
              showModal(lamasInDBModal())
            } else {
              nrowToSubmit <- nrow(r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]])

              landmarks<-data.frame(LMID=integer(nrowToSubmit),
                                    LMmz=double(nrowToSubmit),
                                    LMRT=double(nrowToSubmit),
                                    chromPol=character(nrowToSubmit),
                                    LMName=character(nrowToSubmit))

              landmarks[,2] <- r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]][,1]
              landmarks[,3] <- r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]][,2]
              landmarks[,4] <- rep(r$findLamas$outObj[[3]],nrow(r$findLamas$outObj[[1]][[as.double(r$findLamas$lamaSetToSubmit)]]))
              landmarks[,5] <- paste0(landmarks[,2],"_",landmarks[,3])

              submitLMToDB(r$findLamas$dbSubmitLamasTo, landmarks)

              r$findLamas$submitLamas <- 1
              showNotification("LaMas submitted")
            }
          }
        }
      )

      #### Parameter setting for optimization ####
      #Get filepath to rds for use with optimizeFindLM
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$preFCPFilepath
        },
        handlerExpr={
          shinyFileChoose(input,'preFCPFilepath', roots=c(r$configWiz$roots, wd="."), filetypes=c('','rds'), session=session)

          if(!is.null(input$preFCPFilepath) && length(grep(".rds",as.character(input$preFCPFilepath)))>0){

            fileSelMonitor<-parseFilePaths(r$configWiz$roots,input$preFCPFilepath)
            r$findLamas$preFCPfilepath<-as.character(fileSelMonitor$datapath)

          }
        }
      )

      #Get split delimiter from user
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$split
        },
        handlerExpr={
          r$findLamas$split <- input$split
        }
      )

      #Get minimum RT from where optimizer starts looking
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$minRTcheck
        },
        handlerExpr={
          r$findLamas$minRTcheck <- input$minRTcheck
        }
      )

      #Get minimum RT from where optimizer starts looking
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$preFilterIntensity
        },
        handlerExpr={
          r$findLamas$preFilterIntensity <- input$preFilterIntensity
        }
      )

      #Get chromPol mode from user
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$chromPol
        },
        handlerExpr={
          r$findLamas$chromPol <- input$chromPol
        }
      )

      #Get range of min and max LaMas to use for optimization
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$minMaxLamas
        },
        handlerExpr={
          r$findLamas$minMaxLamas <- input$minMaxLamas
        }
      )

      #Start optimization through use of action button
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$startOptimize
        },
        handlerExpr={
          r$findLamas$startOptimize <- 1

          showNotification("Optimization initiated")
          withProgress(
            value=0,
            message="Optimizing LaMas",
            r$findLamas$outObj <- optimizeFindLM(r$findLamas$preFCPfilepath,
                                                 r$findLamas$split,
                                                 r$findLamas$minRTcheck,
                                                 r$findLamas$preFilterIntensity,
                                                 r$findLamas$chromPol,
                                                 r$findLamas$minMaxLamas[1],
                                                 r$findLamas$minMaxLamas[2])
          )
          showNotification("Optimization of LaMas completed.")
        }
      )

      #### Creating a new DB-file ####
      #Button for creating new DB-file
      observeEvent(
        ignoreNULL = TRUE,
        eventExpr={
          input$createDBButton
        },
        handlerExpr={
          if(!file.exists(paste0(r$findLamas$DBDir,"\\",r$findLamas$DBName))){
            buildDB(r$findLamas$DBName,r$findLamas$DBDir)
            showModal(
              modalDialog(
                title="DB file created",
                paste0("The DB file: ", paste0(r$findLamas$DBDir,"/",r$findLamas$DBName), " has been created."),
                easyClose = TRUE,
                footer = NULL
              )
            )
          } else {
            r$findLamas$DBFeedbackMSG <- "File already exists"
          }
        }
      )

      #Choose dir where DB-file will be created
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$DBDir
        },
        handlerExpr={
          shinyDirChoose(input,"DBDir", roots=c(r$configWiz$roots, wd="."), session=session)
          if(length(input$DBDir) > 0 && ! is.null(input$DBDir)){
            r$findLamas$DBDir <- parseDirPath(r$configWiz$roots, input$DBDir)
          }
        }
      )

      #Storing DB name chosen by user
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$DBName
        },
        handlerExpr={
          req(input$DBName)

          if(grepl(".db",input$DBName)){
            r$findLamas$DBName <- input$DBName
          } else {
            r$findLamas$DBName <- paste0(input$DBName,".db")
          }
        }
      )


    }
  )
}
