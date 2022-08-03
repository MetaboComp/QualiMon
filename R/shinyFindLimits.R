findLimitsUI<-function(id){
  ns<-NS(id)

  tagList(
    fluidRow(
      box(
        title="Choose config to edit",
        width=4,
        solidHeader=T,

        uiOutput(ns('chooseConfigUI'))

      )
    ),
    fluidRow(
      uiOutput(ns('render1')),
      uiOutput(ns('render2')),
      uiOutput(ns('render3')),
      uiOutput(ns('render6'))
    ),
    fluidRow(
      uiOutput(ns('render5')),
      uiOutput(ns('render4')),
      uiOutput(ns('render7')),
      uiOutput(ns('render8'))
    ),
    fluidRow(
      uiOutput(ns('render9')),
      uiOutput(ns('render10')),
      uiOutput(ns('render11')),
      uiOutput(ns('render12'))
    ),
    fluidRow(
      uiOutput(ns('render13'))
    )
  )

}

findLimitsServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      ###############################
      #### Server side rendering ####
      ###############################

      ####Rendering input stuff for editing config####
      output$chooseConfigUI <- renderUI({
        tagList(
          fluidRow(

            column(
              width=6,

              if(is.null(r$findLimits$cfgFilePath)){
                tagList(
                  tags$b("No config file loaded.")
                )
              } else {
                tagList(
                  tags$b("Config file: "),
                  r$findLimits$cfgFilePath
                )
              },
              br(),
              shinyFilesButton(id=ns('cfgFilePath'), "Choose cfg file", title="Choose .txt config file", multiple=F)
            ),
            column(
              width=6,

              tags$b("Choose chromPol to edit:"),
              br(),
              selectInput(ns('chromPolChooser'), NULL, c("RP","RN","HP","HN"), selected=input$chromPolChooser)
            )
          ),
          fluidRow(
            column(
              width=6,

              if(!is.null(r$findLimits$Config) && !is.null(input$StatusLimitInput) && input$StatusLimitInput != 0){
                tagList(
                  tags$b("Submit changes to config-file"),
                  br(),
                  actionButton(ns('submitChanges'),"Submit")
                )
              }
            ),
            column(
              width=6,

              tags$b("Which limit to edit:"),
              br(),
              selectInput(ns('limitChooser'), NULL, c("Soft", "Hard"), selected=input$limitChooser)
            )
          )
        )
      })

      #### Render 1: nLaMas ####
      output$render1 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("nLaMas ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("n LaMas threshold:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('nLaMaInput'), label=NULL, value=ifelse(!is.na(r$findLimits$nLaMa), r$findLimits$nLaMa, 0), step=1)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('nLaMaCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusnLM),FALSE, ifelse(as.integer(r$findLimits$Config$statusnLM), TRUE, FALSE)))
            )
          )
        )
      })

      #### Render 2: nPeaks ####
      output$render2 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("nPeaks ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("n Peaks threshold:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('nPeaksInput'), label=NULL, value=ifelse(!is.na(r$findLimits$nPeaks), r$findLimits$nPeaks, 0), step=10)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('nPeaksCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusNPeaks), FALSE, ifelse(as.integer(r$findLimits$Config$statusNPeaks), TRUE, FALSE)))
            )
          )
        )
      })

      #### Render 3: IPOscore ####
      output$render3 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("IPOscore ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("IPO score threshold:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('IPOInput'), label=NULL, value=ifelse(!is.na(r$findLimits$IPO), r$findLimits$IPO, 0), step=10)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('IPOCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusIPO), FALSE, ifelse(as.integer(r$findLimits$Config$statusIPO), TRUE, FALSE)))
            )
          )
        )
      })

      #### Render 4: prop LMint ####
      output$render4 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Prop. LaMa int ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of LaMa int. outliers:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('LaMaIntInput'), label=NULL, value=ifelse(!is.null(r$findLimits$Int), r$findLimits$Int, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('LaMaIntCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusInt), FALSE, ifelse(as.integer(r$findLimits$Config$statusInt), TRUE, FALSE)))
            )
          )
        )
      })

      #### Render 5: prop LMRT ####
      output$render5 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Prop. LaMa RT ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of LaMa RT outliers:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('LaMaRTInput'), label=NULL, value=ifelse(!is.na(r$findLimits$RT), r$findLimits$RT, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('LaMaRTCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusRT), FALSE, ifelse(as.integer(r$findLimits$Config$statusRT), 1, 0)))
            )
          )

        )
      })

      #### Render 6: TIC ####
      output$render6 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("TIC ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("TIC threshold:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('TICInput'), label=NULL, value=ifelse(!is.na(r$findLimits$TIC), r$findLimits$TIC, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('TICCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusTIC), FALSE, ifelse(as.integer(r$findLimits$Config$statusTIC), 1, 0)))
            )
          )
        )
      })

      #### Render 7: prop Noise ####
      output$render7 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Prop. LaMa noise ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of LaMa noise outliers:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('NoiseInput'), label=NULL, value=ifelse(!is.na(r$findLimits$Noise), r$findLimits$Noise, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('NoiseCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusNoise), FALSE, ifelse(as.integer(r$findLimits$Config$statusNoise), 1, 0)))
            )
          )
        )
      })

      #### Render 8: prop PeakHeight ####
      output$render8 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Prop. LaMa peak height ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of LaMa peak height outliers:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('PeakHeightInput'), label=NULL, value=ifelse(!is.na(r$findLimits$Height), r$findLimits$Height, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('PeakHeightCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusHeight), FALSE, ifelse(as.integer(r$findLimits$Config$statusHeight), 1, 0)))
            )
          )
        )
      })

      #### Render 9: prop TF ####
      output$render9 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Prop. LaMa tail. factor ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of TF outliers:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('TFInput'), label=NULL, value=ifelse(!is.na(r$findLimits$TF), r$findLimits$TF, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('TFCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusTF), FALSE, ifelse(as.integer(r$findLimits$Config$statusTF), 1, 0)))
            )
          )
        )
      })

      #### Render 10: SN ####
      output$render10 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Prop. LaMa signal/noise ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of SN outliers:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('SNInput'), label=NULL, value=ifelse(!is.na(r$findLimits$SN), r$findLimits$SN, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('SNCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusSN), FALSE, ifelse(as.integer(r$findLimits$Config$statusSN), 1, 0)))
            )
          )
        )
      })

      #### Render 11: prop DataPoints ####
      output$render11 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Prop. LaMa DPs/peak ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of DataPoints/peak outliers:"),
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('DataPointsInput'), label=NULL, value=ifelse(!is.na(r$findLimits$DataP), r$findLimits$DataP, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('DataPointsCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusDataPoints), FALSE, ifelse(as.integer(r$findLimits$Config$statusDataPoints), 1, 0)))
            )
          )
        )
      })

      #### Render 12: prop FWHM ####
      output$render12 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("FWHM ", r$findLimits$chromPolChooser, " ", r$findLimits$limitChooser, " limit"),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Prop. of FWHM outliers:")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('FWHMInput'), label=NULL, value=ifelse(!is.na(r$findLimits$Noise), r$findLimits$Noise, 0), step=0.01)
            )
          ),
          fluidRow(
            column(
              width=12,

              awesomeCheckbox(ns('FWHMCheckBox'), "Include in quality check?", value=ifelse(is.na(r$findLimits$Config$statusFWHM), FALSE, ifelse(as.integer(r$findLimits$Config$statusFWHM), 1, 0)))
            )
          )
        )
      })


      #### Render 13: Status Limit ####
      output$render13 <- renderUI({
        req(r$findLimits$Config)

        box(
          width=3,
          title=paste0("Full status limit "),
          status='primary',

          fluidRow(
            column(
              width=12,

              tags$b("Threshold of proportion of failed tests"),
              br(),
              tags$b("for sample to be classified as bad."),
              br(),
              tags$b("Common for all chromPols!")
            )
          ),
          fluidRow(
            column(
              width=6,

              numericInput(ns('StatusLimitInput'), label=NULL, value=ifelse(is.na(r$findLimits$StatusLimitInput), 0, ifelse(as.double(r$findLimits$StatusLimitInput), r$findLimits$StatusLimitInput, 0)), step=0.01)
            )
          )

        )

      })


      #################################################
      #### Handling all parameter inputs from user ####
      #################################################

      #### Choosing config file ####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$cfgFilePath
        },
        handlerExpr={
          shinyFileChoose(input,'cfgFilePath', root=r$configWiz$roots, filetypes=c('','txt'), session=session)

          if(!is.null(input$cfgFilePath) && length(grep(".txt",as.character(input$cfgFilePath)))>0){

            fileSelMonitor<-parseFilePaths(r$configWiz$roots,input$cfgFilePath)
            r$findLimits$cfgFilePath<-as.character(fileSelMonitor$datapath)
            r$findLimits$Config <- readConfigFile(r$findLimits$cfgFilePath)

            # updateAwesomeCheckbox(session, ns('LaMaIntCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('LaMaRTCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('PeakHeightCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('FWHMCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('TFCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('SNCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('NoiseCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('DataPointsCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('IPOCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('nPeaksCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateAwesomeCheckbox(session, ns('nLaMaCheckBox'), value=r$findLimits$Config$statusnLM)
            # updateNumericInput(session=session, inputId=ns('StatusLimInput'), value=r$findLimits$Config$statusnLM)
          }
        }
      )

      #### Reading thresholds based on user settings ####
      observe({
        req(r$findLimits$Config)

        isolate(r$findLimits$limitChooser <- input$limitChooser)
        isolate(r$findLimits$chromPolChooser <- input$chromPolChooser)

        #### RP ####
        if(input$chromPolChooser=="RP"){
          if(input$limitChooser == "Soft"){
            isolate({
              r$findLimits$nLaMa <- r$findLimits$Config$RPnLMSoftLim
              r$findLimits$nPeaks <- r$findLimits$Config$RPnPeaksSoftLim
              r$findLimits$IPO <- r$findLimits$Config$RPIPOSoftLim
              r$findLimits$Int <- r$findLimits$Config$RPIntPropSoftLim
              r$findLimits$RT <- r$findLimits$Config$RPRtPropSoftLim
              r$findLimits$Height <- r$findLimits$Config$RPHeightSoftLim
              r$findLimits$TF <- r$findLimits$Config$RPTFSoftLim
              r$findLimits$SN <- r$findLimits$Config$RPSNSoftLim
              r$findLimits$DataP <- r$findLimits$Config$RPDataPointSoftLim
              r$findLimits$FWHM <- r$findLimits$Config$RPFWHMSoftLim
              r$findLimits$TIC <- r$findLimits$Config$RPTICSoftLim
              r$findLimits$Noise <- r$findLimits$Config$RPNoiseSoftLim
            })
          }
          else{
            isolate({
              r$findLimits$nLaMa <- r$findLimits$Config$RPnLMHardLim
              r$findLimits$nPeaks <- r$findLimits$Config$RPnPeaksHardLim
              r$findLimits$IPO <- r$findLimits$Config$RPIPOHardLim
              r$findLimits$Int <- r$findLimits$Config$RPIntPropHardLim
              r$findLimits$RT <- r$findLimits$Config$RPRtPropHardLim
              r$findLimits$Height <- r$findLimits$Config$RPHeightHardLim
              r$findLimits$TF <- r$findLimits$Config$RPTFHardLim
              r$findLimits$SN <- r$findLimits$Config$RPSNHardLim
              r$findLimits$DataP <- r$findLimits$Config$RPDataPointHardLim
              r$findLimits$FWHM <- r$findLimits$Config$RPFWHMHardLim
              r$findLimits$TIC <- r$findLimits$Config$RPTICHardLim
              r$findLimits$Noise <- r$findLimits$Config$RPNoiseHardLim
            })
          }
        }
        #### RN #####
        else if (input$chromPolChooser=="RN"){
          if(input$limitChooser == "Soft"){
            r$findLimits$nLaMa <- r$findLimits$Config$RNnLMSoftLim
            r$findLimits$nPeaks <- r$findLimits$Config$RNnPeaksSoftLim
            r$findLimits$IPO <- r$findLimits$Config$RNIPOSoftLim
            r$findLimits$Int <- r$findLimits$Config$RNIntPropSoftLim
            r$findLimits$RT <- r$findLimits$Config$RNRtPropSoftLim
            r$findLimits$Height <- r$findLimits$Config$RNHeightSoftLim
            r$findLimits$TF <- r$findLimits$Config$RNTFSoftLim
            r$findLimits$SN <- r$findLimits$Config$RNSNSoftLim
            r$findLimits$DataP <- r$findLimits$Config$RNDataPointSoftLim
            r$findLimits$FWHM <- r$findLimits$Config$RNFWHMSoftLim
            r$findLimits$TIC <- r$findLimits$Config$RNTICSoftLim
            r$findLimits$Noise <- r$findLimits$Config$RNNoiseSoftLim
          }
          else{
            r$findLimits$nLaMa <- r$findLimits$Config$RNnLMHardLim
            r$findLimits$nPeaks <- r$findLimits$Config$RNnPeaksHardLim
            r$findLimits$IPO <- r$findLimits$Config$RNIPOHardLim
            r$findLimits$Int <- r$findLimits$Config$RNIntPropHardLim
            r$findLimits$RT <- r$findLimits$Config$RNRtPropHardLim
            r$findLimits$Height <- r$findLimits$Config$RNHeightHardLim
            r$findLimits$TF <- r$findLimits$Config$RNTFHardLim
            r$findLimits$SN <- r$findLimits$Config$RNSNHardLim
            r$findLimits$DataP <- r$findLimits$Config$RNDataPointHardLim
            r$findLimits$FWHM <- r$findLimits$Config$RNFWHMHardLim
            r$findLimits$TIC <- r$findLimits$Config$RNTICHardLim
            r$findLimits$Noise <- r$findLimits$Config$RNNoiseHardLim
          }
        }
        #### HP ####
        else if (input$chromPolChooser=="HP"){
          if(input$limitChooser == "Soft"){
            r$findLimits$nLaMa <- r$findLimits$Config$HPnLMSoftLim
            r$findLimits$nPeaks <- r$findLimits$Config$HPnPeaksSoftLim
            r$findLimits$IPO <- r$findLimits$Config$HPIPOSoftLim
            r$findLimits$Int <- r$findLimits$Config$HPIntPropSoftLim
            r$findLimits$RT <- r$findLimits$Config$HPRtPropSoftLim
            r$findLimits$Height <- r$findLimits$Config$HPHeightSoftLim
            r$findLimits$TF <- r$findLimits$Config$HPTFSoftLim
            r$findLimits$SN <- r$findLimits$Config$HPSNSoftLim
            r$findLimits$DataP <- r$findLimits$Config$HPDataPointSoftLim
            r$findLimits$FWHM <- r$findLimits$Config$HPFWHMSoftLim
            r$findLimits$TIC <- r$findLimits$Config$HPTICSoftLim
            r$findLimits$Noise <- r$findLimits$Config$HPNoiseSoftLim
          }
          else{
            r$findLimits$nLaMa <- r$findLimits$Config$HPnLMHardLim
            r$findLimits$nPeaks <- r$findLimits$Config$HPnLMHardLim
            r$findLimits$IPO <- r$findLimits$Config$HPIPOHardLim
            r$findLimits$Int <- r$findLimits$Config$HPIntPropHardLim
            r$findLimits$RT <- r$findLimits$Config$HPRtPropHardLim
            r$findLimits$Height <- r$findLimits$Config$HPHeightHardLim
            r$findLimits$TF <- r$findLimits$Config$HPTFHardLim
            r$findLimits$SN <- r$findLimits$Config$HPSNHardLim
            r$findLimits$DataP <- r$findLimits$Config$HPDataPointHardLim
            r$findLimits$FWHM <- r$findLimits$Config$HPFWHMHardLim
            r$findLimits$TIC <- r$findLimits$Config$HPTICHardLim
            r$findLimits$Noise <- r$findLimits$Config$HPNoiseHardLim
          }
        }
        #### HN #####
        else if (input$chromPolChooser=="HN"){
          if(input$limitChooser == "Soft"){
            r$findLimits$nLaMa <- r$findLimits$Config$HNnLMSoftLim
            r$findLimits$nPeaks <- r$findLimits$Config$HNnPeaksSoftLim
            r$findLimits$IPO <- r$findLimits$Config$HNIPOSoftLim
            r$findLimits$Int <- r$findLimits$Config$HNIntPropSoftLim
            r$findLimits$RT <- r$findLimits$Config$HNRtPropSoftLim
            r$findLimits$Height <- r$findLimits$Config$HNHeightSoftLim
            r$findLimits$TF <- r$findLimits$Config$HNTFSoftLim
            r$findLimits$SN <- r$findLimits$Config$HNSNSoftLim
            r$findLimits$DataP <- r$findLimits$Config$HNDataPointSoftLim
            r$findLimits$FWHM <- r$findLimits$Config$HNFWHMSoftLim
            r$findLimits$TIC <- r$findLimits$Config$HNTICSoftLim
            r$findLimits$Noise <- r$findLimits$Config$HNNoiseSoftLim
          }
          else{
            r$findLimits$nLaMa <- r$findLimits$Config$HNnLMHardLim
            r$findLimits$nPeaks <- r$findLimits$Config$HNnPeaksHardLim
            r$findLimits$IPO <- r$findLimits$Config$HNIPOHardLim
            r$findLimits$Int <- r$findLimits$Config$HNIntPropHardLim
            r$findLimits$RT <- r$findLimits$Config$HNRtPropHardLim
            r$findLimits$Height <- r$findLimits$Config$HNHeightHardLim
            r$findLimits$TF <- r$findLimits$Config$HNTFHardLim
            r$findLimits$SN <- r$findLimits$Config$HNSNHardLim
            r$findLimits$DataP <- r$findLimits$Config$HNDataPointHardLim
            r$findLimits$FWHM <- r$findLimits$Config$HNFWHMHardLim
            r$findLimits$TIC <- r$findLimits$Config$HNTICHardLim
            r$findLimits$Noise <- r$findLimits$Config$HNNoiseHardLim
          }
        }





        r$findLimits$StatusLimitInput <- r$findLimits$Config$statusLim
      })

      #### Submit changes to config file ####
      observeEvent(
        ignoreNULL=T,
        eventExpr={
          input$submitChanges
        },
        handlerExpr={
          isolate({
            suppressWarnings(
              for(i in 1:length(r$findLimits$Config)){
                if(is.na(r$findLimits$Config[[i]])){
                  r$findLimits$Config[[i]] <- 0
                }
              }
            )

            r$findLimits$nLaMa <- input$nLaMaInput
            r$findLimits$nPeaks <- input$nPeaksInput
            r$findLimits$IPO <- input$IPOInput
            r$findLimits$Int <- input$LaMaIntInput
            r$findLimits$RT <- input$LaMaRTInput
            r$findLimits$TIC <- input$TICInput
            r$findLimits$Noise <- input$NoiseInput
            r$findLimits$Height <- input$PeakHeightInput
            r$findLimits$TF <- input$TFInput
            r$findLimits$SN <- input$SNInput
            r$findLimits$DataP <- input$DataPointsInput
            r$findLimits$FWHM <- input$FWHMInput

            #### Config stuff ####
            #Folder stuff
            configOutput<-data.frame(matrix(data=NA, nrow=89, ncol=1))
            configOutput[1,1]<-"Filepath to folder that .d files are created in:"
            configOutput[2,1]<-r$findLimits$Config$dir
            configOutput[3,1]<-"Filepath to backup directory"
            configOutput[4,1]<-ifelse(!is.null(r$findLimits$Config$tmp_sink_dir), "", r$findLimits$Config$tmp_sink_dir)
            configOutput[5,1]<-"Filepath to folder that mzMLs are created in:"
            configOutput[6,1]<-r$findLimits$Config$outdir
            configOutput[7,1]<-"Filepath to msconvert:"
            configOutput[8,1]<-r$findLimits$Config$msconvert
            configOutput[9,1]<-"Filepath to database object:"
            configOutput[10,1]<-r$findLimits$Config$dbName
            configOutput[11,1]<-"noCheck vector (list of sample names to not be quality monitored, separated by space):"
            configOutput[12,1]<-paste(r$findLimits$Config$noCheck, sep=" ", collapse=" ")
            configOutput[13,1]<-"instrument:"
            configOutput[14,1]<-r$findLimits$Config$instrument
            configOutput[15,1]<-"--ltQC--"
            configOutput[16,1]<-r$findLimits$Config$ltQCname
            configOutput[17,1]<-"Sample matrix:"
            configOutput[18,1]<-r$findLimits$Config$sampleMatrix
            configOutput[19,1]<-"Accepted ppm mz difference for landmarks:"
            configOutput[20,1]<-r$findLimits$Config$dPPM
            configOutput[21,1]<-"Retention time window for finding landmarks:"
            configOutput[22,1]<-r$findLimits$Config$rtWin
            configOutput[23,1]<-"Alpha value used for statistical tests:"
            configOutput[24,1]<-r$findLimits$Config$alpha
            configOutput[25,1]<-"Sleep time:"
            configOutput[26,1]<-r$findLimits$Config$sleep.time
            configOutput[27,1]<-"--Min file size to avoid copying empty files--"
            configOutput[28,1]<-r$findLimits$Config$minFileSize
            configOutput[29,1]<-"--Folder depth for checking----"
            configOutput[30,1]<-r$findLimits$Config$folderDepth
            configOutput[31,]<-"--FileFormat--"
            configOutput[32,]<-r$findLimits$Config$fileFormat
            configOutput[33,]<-"--SlackChannel hard limit--"
            configOutput[34,]<-ifelse(!is.null(r$findLimits$Config$slackChannelHard), "", r$findLimits$Config$slackChannelHard)
            configOutput[35,]<-"--SlackChannel log--"
            configOutput[36,]<-ifelse(!is.null(r$findLimits$Config$slackChannelLog), "", r$findLimits$Config$slackChannelLog)
            configOutput[37,]<-"--slackToken--"
            configOutput[38,]<-ifelse(!is.null(r$findLimits$Config$slackToken), "", r$findLimits$Config$slackToken)

            #### Soft Limits ####
            #RP Soft
            configOutput[39,]<-"----Softlimits RP----"
            configOutput[40,]<-"Number of landmarks found:"
            configOutput[41,]<-r$findLimits$Config$RPnLMSoftLim
            configOutput[42,]<-"Number of Peaks:"
            configOutput[43,]<-r$findLimits$Config$RPnPeaksSoftLim
            configOutput[44,]<-"IPO score:"
            configOutput[45,]<-r$findLimits$Config$RPIPOSoftLim
            configOutput[46,]<-"Proportion of LM intensity outliers:"
            configOutput[47,]<-r$findLimits$Config$RPIntPropSofttLim
            configOutput[48,]<-"Proportion of LM RT outliers:"
            configOutput[49,]<-r$findLimits$Config$RPRtPropSoftLim
            configOutput[50,]<-"RPTICSoftLim"
            configOutput[51,]<-r$findLimits$Config$RPTICSoftLim
            configOutput[52,]<-"RPNoiseSoftLim"
            configOutput[53,]<-r$findLimits$Config$RPNoiseSoftLim
            configOutput[54,]<-"RPHeightSoftLim"
            configOutput[55,]<-r$findLimits$Config$RPHeightSoftLim
            configOutput[56,]<-"RPTFSoftLim"
            configOutput[57,]<-r$findLimits$Config$RPTFSoftLim
            configOutput[58,]<-"RPSNSoftLim"
            configOutput[59,]<-r$findLimits$Config$RPSNSoftLim
            configOutput[60,]<-"RPDataPointSoftLim"
            configOutput[61,]<-r$findLimits$Config$RPDataPointSoftLim
            configOutput[62,]<-"RPFWHMSoftLim"
            configOutput[63,]<-r$findLimits$Config$RPFWHMSoftLim

            #RN Soft
            configOutput[64,]<-"----Softlimits RN----"
            configOutput[65,]<-"Number of landmarks found:"
            configOutput[66,]<-r$findLimits$Config$RNnLMSoftLim
            configOutput[67,]<-"Number of Peaks:"
            configOutput[68,]<-r$findLimits$Config$RNnPeaksSoftLim
            configOutput[69,]<-"IPO score:"
            configOutput[70,]<-r$findLimits$Config$RNIPOSoftLim
            configOutput[71,]<-"Proportion of LM intensity outliers:"
            configOutput[72,]<-r$findLimits$Config$RNIntPropSofttLim
            configOutput[73,]<-"Proportion of LM RT outliers:"
            configOutput[74,]<-r$findLimits$Config$RNRtPropSoftLim
            configOutput[75,]<-"RNTICSoftLim"
            configOutput[76,]<-r$findLimits$Config$RNTICSoftLim
            configOutput[77,]<-"RNNoiseSoftLim"
            configOutput[78,]<-r$findLimits$Config$RNNoiseSoftLim
            configOutput[79,]<-"RNHeightSoftLim"
            configOutput[80,]<-r$findLimits$Config$RNHeightSoftLim
            configOutput[81,]<-"RNTFSoftLim"
            configOutput[82,]<-r$findLimits$Config$RNTFSoftLim
            configOutput[83,]<-"RNSNSoftLim"
            configOutput[84,]<-r$findLimits$Config$RNSNSoftLim
            configOutput[85,]<-"RNDataPointSoftLim"
            configOutput[86,]<-r$findLimits$Config$RNDataPointSoftLim
            configOutput[87,]<-"RNFWHMSoftLim"
            configOutput[88,]<-r$findLimits$Config$RNFWHMSoftLim

            #HP Soft
            configOutput[89,]<-"----Softlimits HP----"
            configOutput[90,]<-"Number of landmarks found:"
            configOutput[91,]<-r$findLimits$Config$HPnLMSoftLim
            configOutput[92,]<-"Number of Peaks:"
            configOutput[93,]<-r$findLimits$Config$HPnPeaksSoftLim
            configOutput[94,]<-"IPO score:"
            configOutput[95,]<-r$findLimits$Config$HPIPOSoftLim
            configOutput[96,]<-"Proportion of LM intensity outliers:"
            configOutput[97,]<-r$findLimits$Config$HPIntPropSofttLim
            configOutput[98,]<-"Proportion of LM RT outliers:"
            configOutput[99,]<-r$findLimits$Config$HPRtPropSoftLim
            configOutput[100,]<-"HPTICSoftLim"
            configOutput[101,]<-r$findLimits$Config$HPTICSoftLim
            configOutput[102,]<-"HPNoiseSoftLim"
            configOutput[103,]<-r$findLimits$Config$HPNoiseSoftLim
            configOutput[104,]<-"HPHeightSoftLim"
            configOutput[105,]<-r$findLimits$Config$HPHeightSoftLim
            configOutput[106,]<-"HPTFSoftLim"
            configOutput[107,]<-r$findLimits$Config$HPTFSoftLim
            configOutput[108,]<-"HPSNSoftLim"
            configOutput[109,]<-r$findLimits$Config$HPSNSoftLim
            configOutput[110,]<-"HPDataPointSoftLim"
            configOutput[111,]<-r$findLimits$Config$HPDataPointSoftLim
            configOutput[112,]<-"HPFWHMSoftLim"
            configOutput[113,]<-r$findLimits$Config$HPFWHMSoftLim

            #HN Soft
            configOutput[114,]<-"----Softlimits HN----"
            configOutput[115,]<-"Number of landmarks found:"
            configOutput[116,]<-r$findLimits$Config$HNnLMSoftLim
            configOutput[117,]<-"Number of Peaks:"
            configOutput[118,]<-r$findLimits$Config$HNnPeaksSoftLim
            configOutput[119,]<-"IPO score:"
            configOutput[120,]<-r$findLimits$Config$HNIPOSoftLim
            configOutput[121,]<-"Proportion of LM intensity outliers:"
            configOutput[122,]<-r$findLimits$Config$HNIntPropSofttLim
            configOutput[123,]<-"Proportion of LM RT outliers:"
            configOutput[124,]<-r$findLimits$Config$HNRtPropSoftLim
            configOutput[125,]<-"HNTICSoftLim"
            configOutput[126,]<-r$findLimits$Config$HNTICSoftLim
            configOutput[127,]<-"HNNoiseSoftLim"
            configOutput[128,]<-r$findLimits$Config$HNNoiseSoftLim
            configOutput[129,]<-"HNHeightSoftLim"
            configOutput[130,]<-r$findLimits$Config$HNHeightSoftLim
            configOutput[131,]<-"HNTFSoftLim"
            configOutput[132,]<-r$findLimits$Config$HNTFSoftLim
            configOutput[133,]<-"HNSNSoftLim"
            configOutput[134,]<-r$findLimits$Config$HNSNSoftLim
            configOutput[135,]<-"HNDataPointSoftLim"
            configOutput[136,]<-r$findLimits$Config$HNDataPointSoftLim
            configOutput[137,]<-"HNFWHMSoftLim"
            configOutput[138,]<-r$findLimits$Config$HNFWHMSoftLim
            #### Hard limits ####
            #RP hard
            configOutput[139,]<-"--Hardlimits RP--"
            configOutput[140,]<-"Number of landmarks found:"
            configOutput[141,]<-r$findLimits$Config$RPnLMHardLim
            configOutput[142,]<-"Number of Peaks:"
            configOutput[143,]<-r$findLimits$Config$RPnPeaksHardLim
            configOutput[144,]<-"IPO score:"
            configOutput[145,]<-r$findLimits$Config$RPIPOHardLim
            configOutput[146,]<-"Proportion of LM intensity outliers"
            configOutput[147,]<-r$findLimits$Config$RPIntPropHardtLim
            configOutput[148,]<-"Proportion of LM RT outliers:"
            configOutput[149,]<-r$findLimits$Config$RPRtPropHardLim
            configOutput[150,]<-"RPTICHardLim"
            configOutput[151,]<-r$findLimits$Config$RPTICHardLim
            configOutput[152,]<-"RPNoiseHardLim"
            configOutput[153,]<-r$findLimits$Config$RPNoiseHardLim
            configOutput[154,]<-"RPHeightHardLim"
            configOutput[155,]<-r$findLimits$Config$RPHeightHardLim
            configOutput[156,]<-"RPTFHardLim"
            configOutput[157,]<-r$findLimits$Config$RPTFHardLim
            configOutput[158,]<-"RPSNHardLim"
            configOutput[159,]<-r$findLimits$Config$RPSNHardLim
            configOutput[160,]<-"RPDataPointHardLim"
            configOutput[161,]<-r$findLimits$Config$RPDataPointHardLim
            configOutput[162,]<-"RPFWHMHardLim"
            configOutput[163,]<-r$findLimits$Config$RPFWHMHardLim

            #RN hard
            configOutput[164,]<-"--Hardlimits RN--"
            configOutput[165,]<-"Number of landmarks found:"
            configOutput[166,]<-r$findLimits$Config$RNnLMHardLim
            configOutput[167,]<-"Number of Peaks:"
            configOutput[168,]<-r$findLimits$Config$RNnPeaksHardLim
            configOutput[169,]<-"IPO score:"
            configOutput[170,]<-r$findLimits$Config$RNIPOHardLim
            configOutput[171,]<-"Proportion of LM intensity outliers"
            configOutput[172,]<-r$findLimits$Config$RNIntPropHardtLim
            configOutput[173,]<-"Proportion of LM RT outliers:"
            configOutput[174,]<-r$findLimits$Config$RNRtPropHardLim
            configOutput[175,]<-"RNTICHardLim"
            configOutput[176,]<-r$findLimits$Config$RNTICHardLim
            configOutput[177,]<-"RNNoiseHardLim"
            configOutput[178,]<-r$findLimits$Config$RNNoiseHardLim
            configOutput[179,]<-"RNHeightHardLim"
            configOutput[180,]<-r$findLimits$Config$RNHeightHardLim
            configOutput[181,]<-"RNTFHardLim"
            configOutput[182,]<-r$findLimits$Config$RNTFHardLim
            configOutput[183,]<-"RNSNHardLim"
            configOutput[184,]<-r$findLimits$Config$RNSNHardLim
            configOutput[185,]<-"RNDataPointHardLim"
            configOutput[186,]<-r$findLimits$Config$RNDataPointHardLim
            configOutput[187,]<-"RNFWHMHardLim"
            configOutput[188,]<-r$findLimits$Config$RNFWHMHardLim

            #HP hard
            configOutput[189,]<-"--Hardlimits HP--"
            configOutput[190,]<-"Number of landmarks found:"
            configOutput[191,]<-r$findLimits$Config$HPnLMHardLim
            configOutput[192,]<-"Number of Peaks:"
            configOutput[193,]<-r$findLimits$Config$HPnPeaksHardLim
            configOutput[194,]<-"IPO score:"
            configOutput[195,]<-r$findLimits$Config$HPIPOHardLim
            configOutput[196,]<-"Proportion of LM intensity outliers"
            configOutput[197,]<-r$findLimits$Config$HPIntPropHardtLim
            configOutput[198,]<-"Proportion of LM RT outliers:"
            configOutput[199,]<-r$findLimits$Config$HPRtPropHardLim
            configOutput[200,]<-"HPTICHardLim"
            configOutput[201,]<-r$findLimits$Config$HPTICHardLim
            configOutput[202,]<-"HPNoiseHardLim"
            configOutput[203,]<-r$findLimits$Config$HPNoiseHardLim
            configOutput[204,]<-"HPHeightHardLim"
            configOutput[205,]<-r$findLimits$Config$HPHeightHardLim
            configOutput[206,]<-"HPTFHardLim"
            configOutput[207,]<-r$findLimits$Config$HPTFHardLim
            configOutput[208,]<-"HPSNHardLim"
            configOutput[209,]<-r$findLimits$Config$HPSNHardLim
            configOutput[210,]<-"HPDataPointHardLim"
            configOutput[211,]<-r$findLimits$Config$HPDataPointHardLim
            configOutput[212,]<-"HPFWHMHardLim"
            configOutput[213,]<-r$findLimits$Config$HPFWHMHardLim

            #HN hard
            configOutput[214,]<-"--Hardlimits HN--"
            configOutput[215,]<-"Number of landmarks found:"
            configOutput[216,]<-r$findLimits$Config$HNnLMHardLim
            configOutput[217,]<-"Number of Peaks:"
            configOutput[218,]<-r$findLimits$Config$HNnPeaksHardLim
            configOutput[219,]<-"IPO score:"
            configOutput[220,]<-r$findLimits$Config$HNIPOHardLim
            configOutput[221,]<-"Proportion of LM intensity outliers"
            configOutput[222,]<-r$findLimits$Config$HNIntPropHardtLim
            configOutput[223,]<-"Proportion of LM RT outliers:"
            configOutput[224,]<-r$findLimits$Config$HNRtPropHardLim
            configOutput[225,]<-"HNTICHardLim"
            configOutput[226,]<-r$findLimits$Config$HNTICHardLim
            configOutput[227,]<-"HNNoiseHardLim"
            configOutput[228,]<-r$findLimits$Config$HNNoiseHardLim
            configOutput[229,]<-"HNHeightHardLim"
            configOutput[230,]<-r$findLimits$Config$HNHeightHardLim
            configOutput[231,]<-"HNTFHardLim"
            configOutput[232,]<-r$findLimits$Config$HNTFHardLim
            configOutput[233,]<-"HNSNHardLim"
            configOutput[234,]<-r$findLimits$Config$HNSNHardLim
            configOutput[235,]<-"HNDataPointHardLim"
            configOutput[236,]<-r$findLimits$Config$HNDataPointHardLim
            configOutput[237,]<-"HNFWHMHardLim"
            configOutput[238,]<-r$findLimits$Config$HNFWHMHardLim

            #### Status stuff ####
            configOutput[239,]<-"----statusInt----"
            configOutput[240,]<-ifelse(input$LaMaIntCheckBox, 1, 0)
            configOutput[241,]<-"--statusRT--"
            configOutput[242,]<-ifelse(input$LaMaRTCheckBox, 1, 0)
            configOutput[243,]<-"--statusHeight--"
            configOutput[244,]<-ifelse(input$PeakHeightCheckBox, 1, 0)
            configOutput[245,]<-"--statusFWHM--"
            configOutput[246,]<-ifelse(input$FWHMCheckBox, 1, 0)
            configOutput[247,]<-"--statusTF--"
            configOutput[248,]<-ifelse(input$TFCheckBox, 1, 0)
            configOutput[249,]<-"--statusSN--"
            configOutput[250,]<-ifelse(input$SNCheckBox, 1, 0)
            configOutput[251,]<-"--statusNoise--"
            configOutput[252,]<-ifelse(input$NoiseCheckBox, 1, 0)
            configOutput[253,]<-"--statusDataPoints--"
            configOutput[254,]<-ifelse(input$DataPointsCheckBox, 1, 0)
            configOutput[255,]<-"--statusIPO--"
            configOutput[256,]<-ifelse(input$IPOCheckBox, 1, 0)
            configOutput[257,]<-"--statusNPeaks--"
            configOutput[258,]<-ifelse(input$nPeaksCheckBox, 1, 0)
            configOutput[259,]<-"--statusTIC--"
            configOutput[260,]<-ifelse(input$TICCheckBox, 1, 0)
            configOutput[261,]<-"--statusnLM--"
            configOutput[262,]<-ifelse(input$nLaMaCheckBox, 1, 0)
            configOutput[263,]<-"--StatusLim--"
            configOutput[264,]<-input$StatusLimitInput
            configOutput[265,]<-"" #For readLine not to freak out :)

            r$findLimits$StatusLimitInput <- input$StatusLimitInput

            #### Submitting the correct data to the config-file ####
            #### RP ####
            if(input$chromPolChooser=="RP"){
              if(input$limitChooser == "Soft"){
                configOutput[41,] <- r$findLimits$nLaMa
                configOutput[43,] <- r$findLimits$nPeaks
                configOutput[45,] <- r$findLimits$IPO
                configOutput[47,] <- r$findLimits$Int
                configOutput[49,] <- r$findLimits$RT
                configOutput[51,] <- r$findLimits$TIC
                configOutput[53,] <- r$findLimits$Noise
                configOutput[55,] <- r$findLimits$Height
                configOutput[57,] <- r$findLimits$TF
                configOutput[59,] <- r$findLimits$SN
                configOutput[61,] <- r$findLimits$DataP
                configOutput[63,] <- r$findLimits$FWHM
              }
              else{
                configOutput[141,] <- r$findLimits$nLaMa
                configOutput[143,] <- r$findLimits$nPeaks
                configOutput[145,] <- r$findLimits$IPO
                configOutput[147,] <- r$findLimits$Int
                configOutput[149,] <- r$findLimits$RT
                configOutput[151,] <- r$findLimits$TIC
                configOutput[153,] <- r$findLimits$Noise
                configOutput[155,] <- r$findLimits$Height
                configOutput[157,] <- r$findLimits$TF
                configOutput[159,] <- r$findLimits$SN
                configOutput[161,] <- r$findLimits$DataP
                configOutput[163,] <- r$findLimits$FWHM
              }
            }
            #### RN #####
            else if (input$chromPolChooser=="RN"){
              if(input$limitChooser == "Soft"){
                configOutput[66,] <- r$findLimits$nLaMa
                configOutput[68,] <- r$findLimits$nPeaks
                configOutput[70,] <- r$findLimits$IPO
                configOutput[72,] <- r$findLimits$Int
                configOutput[74,] <- r$findLimits$RT
                configOutput[76,] <- r$findLimits$TIC
                configOutput[78,] <- r$findLimits$Noise
                configOutput[80,] <- r$findLimits$Height
                configOutput[82,] <- r$findLimits$TF
                configOutput[84,] <- r$findLimits$SN
                configOutput[86,] <- r$findLimits$DataP
                configOutput[88,] <- r$findLimits$FWHM
              }
              else{
                configOutput[166,] <- r$findLimits$nLaMa
                configOutput[168,] <- r$findLimits$nPeaks
                configOutput[170,] <- r$findLimits$IPO
                configOutput[172,] <- r$findLimits$Int
                configOutput[174,] <- r$findLimits$RT
                configOutput[176,] <- r$findLimits$TIC
                configOutput[178,] <- r$findLimits$Noise
                configOutput[180,] <- r$findLimits$Height
                configOutput[182,] <- r$findLimits$TF
                configOutput[184,] <- r$findLimits$SN
                configOutput[186,] <- r$findLimits$DataP
                configOutput[188,] <- r$findLimits$FWHM
              }
            }
            #### HP ####
            else if (input$chromPolChooser=="HP"){
              if(input$limitChooser == "Soft"){
                configOutput[91,] <- r$findLimits$nLaMa
                configOutput[93,] <- r$findLimits$nPeaks
                configOutput[95,] <- r$findLimits$IPO
                configOutput[97,] <- r$findLimits$Int
                configOutput[99,] <- r$findLimits$RT
                configOutput[101,] <- r$findLimits$TIC
                configOutput[103,] <- r$findLimits$Noise
                configOutput[105,] <- r$findLimits$Height
                configOutput[107,] <- r$findLimits$TF
                configOutput[109,] <- r$findLimits$SN
                configOutput[111,] <- r$findLimits$DataP
                configOutput[113,] <- r$findLimits$FWHM
              }
              else{
                configOutput[191,] <- r$findLimits$nLaMa
                configOutput[193,] <- r$findLimits$nPeaks
                configOutput[195,] <- r$findLimits$IPO
                configOutput[197,] <- r$findLimits$Int
                configOutput[199,] <- r$findLimits$RT
                configOutput[201,] <- r$findLimits$TIC
                configOutput[203,] <- r$findLimits$Noise
                configOutput[205,] <- r$findLimits$Height
                configOutput[207,] <- r$findLimits$TF
                configOutput[209,] <- r$findLimits$SN
                configOutput[211,] <- r$findLimits$DataP
                configOutput[213,] <- r$findLimits$FWHM
              }
            }
            #### HN #####
            else if (input$chromPolChooser=="HN"){
              if(input$limitChooser == "Soft"){
                configOutput[116,] <- r$findLimits$nLaMa
                configOutput[118,] <- r$findLimits$nPeaks
                configOutput[120,] <- r$findLimits$IPO
                configOutput[122,] <- r$findLimits$Int
                configOutput[124,] <- r$findLimits$RT
                configOutput[126,] <- r$findLimits$TIC
                configOutput[128,] <- r$findLimits$Noise
                configOutput[130,] <- r$findLimits$Height
                configOutput[132,] <- r$findLimits$TF
                configOutput[134,] <- r$findLimits$SN
                configOutput[136,] <- r$findLimits$DataP
                configOutput[138,] <- r$findLimits$FWHM
              }
              else{
                configOutput[216,] <- r$findLimits$nLaMa
                configOutput[218,] <- r$findLimits$nPeaks
                configOutput[220,] <- r$findLimits$IPO
                configOutput[222,] <- r$findLimits$Int
                configOutput[224,] <- r$findLimits$RT
                configOutput[226,] <- r$findLimits$TIC
                configOutput[228,] <- r$findLimits$Noise
                configOutput[230,] <- r$findLimits$Height
                configOutput[232,] <- r$findLimits$TF
                configOutput[234,] <- r$findLimits$SN
                configOutput[236,] <- r$findLimits$DataP
                configOutput[238,] <- r$findLimits$FWHM
              }
            }


            # updatePickerInput()

            #### Write new config file ####
            write.table(configOutput,file=r$findLimits$cfgFilePath, row.names=FALSE, col.names=FALSE, quote = FALSE, append=FALSE)
            r$findLimits$Config <- readConfigFile(r$findLimits$cfgFilePath)
          })
        }
      )
    }
  )
}
