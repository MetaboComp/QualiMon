configWizUI<-function(id){
  ns<-NS(id)

  tagList(
    fluidRow(
      uiOutput(ns('render0')),
      uiOutput(ns('render1')),
      uiOutput(ns('render2')),
      uiOutput(ns('render3'))
    ),
    fluidRow(
      uiOutput(ns('render4')),
      uiOutput(ns('render5')),
      uiOutput(ns('render6')),
      uiOutput(ns('render7'))
    ),
    fluidRow(
      uiOutput(ns('render8')),
      uiOutput(ns('render9')),
      uiOutput(ns('render10')),
      uiOutput(ns('render11'))
    ),
    fluidRow(
      uiOutput(ns('render12'))
    )
  )
}

configWizServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      #######################
      ####Rendering stuff####
      #######################
      ####Render 0####
      output$render0<-renderUI({
          box(
            width=3,
            title="Step 1/13",

            div(style = "height:110px;",
              tags$b("Choose the folder where the instrument will generate raw files:"),
              tags$br(),
              shinyDirButton(ns('dir'), "Choose folder", FALSE),
              tags$br(),
              tags$b(r$configWiz$injPath)
            )
          )
      })


      ####Render 1####
      output$render1<-renderUI({
        req(r$configWiz$injPath)

        box(width=3,
            title="Step 2/13",

            div(style = "height:110px;",
              tags$b("If wanted, a folder whereto raw files will be backed up:"),
              tags$br(),
              fluidRow(
                column(
                  width=6,

                  div(style="display: inline-block;vertical-align:top;",shinyDirButton(ns('dir2'), "Choose folder", "Select a folder", FALSE))
                ),
                column(
                  width=6,

                  div(style="display: inline-block;vertical-align:top;",awesomeCheckbox(ns('checkBackup'), "Skip backup", value=FALSE))
                )
              ),
              tags$b(r$configWiz$path2)
              )
        )
      })

      ####Render 2####
      output$render2<-renderUI({
        if(!is.null(input$checkBackup)){
          if(length(r$configWiz$path2)>0 || input$checkBackup==TRUE){
              box(
                width=3,
                title="Step 3/13",

                div(style = "height:110px;",
                  tags$b("3. A folder in which mzML files will be created:"),
                  tags$br(),
                  tags$br(),
                  shinyDirButton(ns('dir3'), "Choose folder", "Select a folder", FALSE),
                  tags$br(),
                  tags$b(r$configWiz$path3)
                )
              )
          }
        }
      })

      ####Render 3####
      output$render3<-renderUI({
        req(r$configWiz$path3)

        box(
          width=3,
          title="Step 4/13",
          div(style = "height:110px;",
            tags$b("Select msconvert.exe:"),
            tags$br(),
            tags$br(),
            shinyFilesButton(ns('dir4'), "Choose msconvert.exe", "Select msconvert.exe", multiple=FALSE),
            tags$br(),
            tags$b(r$configWiz$path4)
          )
        )
      })

      ####Render 4####
      output$render4<-renderUI({
        req(r$configWiz$path4)

        box(
          width=3,
          title="Step 5/13",

          div(style = "height:140px;",
            tags$b("The DB to use for the analysis:"),
            tags$br(),
            shinyFilesButton(id=ns('dir5'), label="Choose DB file", title="Choose the DB file to use for analysis", multiple=F),
            tags$br(),
            tags$b(r$configWiz$path5)
          )
        )
      })

      ####Render 5####

      output$render5<-renderUI({
        req(r$configWiz$path5)

        box(
          width=3,
          title="Step 6/13",

          div(style = "height:140px;",
            tags$b("6. Type in parts of filenames of files to be excluded from analysis."),
            tags$br(),
            tags$b("Make sure to separate every word by spaces (e.g. \"blank cond\"):"),
            br(),
            textInput(ns('noCheck'), label=NULL)
          )
        )
      })

      ####Render 6####
      output$render6<-renderUI({
        req(r$configWiz$noCheck)

        box(
          width=3,
          title="Step 7/13",
          div(style = "height:140px;",
            tags$b("Specify instrument type:"),
            br(),
            selectizeInput(ns('instrument'),
                           label=NULL,
                           multiple=FALSE,
                           selected=NULL,
                           choices=c("QTOF", "Orbitrap"),
                           options=list(placeholder='Select an instrument',onInitialize = I('function() { this.setValue(""); }'))
                           )
          )
        )
      })
      ####Render 7####
      output$render7<-renderUI({
        req(r$configWiz$instrument)

        box(
          width=3,
          title="Step 8/13",
          div(style = "height:140px;",
            tags$b("8. Specify sample matrix (e.g. \"blood\"):"),
            br(),
            textInput(ns('sampMatrix'), label=NULL
            )
          )
        )
      })

      ####Render 8####
      output$render8<-renderUI({
        req(r$configWiz$sampMatrix)

        box(
          width=3,
          title="Step 9/13",

          div(style = "height:140px;",
            tags$b("9a. Input accepted PPM and RT deviation for landmarks:"),
            br(),
            column(width=4, numericInput(ns('dPPM'), label="Set dPPM", 5, max=100)),
            column(width=8, numericInput(ns('dRT'), label="Set RT window (s)", 30, max=120)),
            br(),
            tags$b("9b. Input number injections to monitor at most:"),
            column(width=12, numericInput(ns('nSampMonitor'), label="", value=500))
          )
        )
      })

      ####Render 9####
      output$render9<-renderUI({
        req(r$configWiz$dRT)

        box(
          width=3,
          title="Step 10/13",
          div(style = "height:140px;",
            tags$b("Input stat alpha value, interval of file size increase and min. file size check:"),
            br(),
            column(
              width=6,
              numericInput(ns('alpha'), label="Set alpha value", 0.01)
            ),
            column(
              width=6,
              numericInput(ns('sleep'), label="Set sleep time", 30)
            ),
            column(
              width=12,
              numericInput(ns('minFileSize'), label="Set min. file size (mb)", 20)
            )
          )
        )
      })

      ####Render 10####
      output$render10<-renderUI({
        req(r$configWiz$minFileSize)

        box(
          width=3,
          title="Step 11/13",
          div(style = "height:140px;",
            tags$b("Folder depth for monitoring projects and minimum file size:"),
            br(),
            column(
              width=6,
              numericInput(ns('folderDepth'), label="Set folder depth", 2)
            ),
            column(
              width=6,
              textInput(ns('fileFormat'), label="File format", NULL)
            ),
            column(
              width=6,
              textInput(ns('ltQCName'), label="Name of ltQC", NULL)
            )
          )
        )
      })

      ####Render 12####
      output$render12<-renderUI({
        req(r$configWiz$fileFormat)

        box(
          width=6,
          title="Step 12/13",

          div(style = "height:130px;",
              tags$b("Slack settings:"),
              awesomeCheckbox(ns('checkSlack'), label="", value=FALSE),
              uiOutput(ns('slackChoices'))
          )
        )
      })

      ####Render 11####
      output$render11<-renderUI({
        req(r$configWiz$fileFormat)

        box(
          width=3,
          title="Step 13/13",

          div(style = "height:130px;",
              column(
                width=12,

                tags$b("Choose name and folder for config file:")
              ),
              column(
                width=12,

                textInput(ns('configName'), label="Name of config file")
              ),
              uiOutput(ns('configChoices'))
          )
        )
      })

      ####Render slack stuff####
      output$slackChoices<-renderUI({
        if(!is.null(input$checkSlack)){
          if(input$checkSlack==TRUE){
            column(
              width=12,

              tags$b("Format for channel input: #channelName"),

              column(
                width=6,

                textInput(ns('hardLimChannel1'), label="Channel 1 name (bad sample notif.):", value=NULL)
              ),
              column(
                width=6,

                textInput(ns('hardLimChannel2'), label="Channel 2 name (optional, full log):", value=NULL)
              ),
              column(
                width=12,

                textInput(ns('slackToken'), label="Slack token:", value=NULL)
              )
            )
          }
        }
      })
      ####Render config stuff####
      output$configChoices <- renderUI({
        if(!is.null(r$configWiz$configName)){
          tagList(
            column(
              width=6,

              shinyDirButton(ns('createDir'), "Choose folder", FALSE)
            ),
            column(
              width=6,

              actionButton(ns('create'),label="Create file")
            ),
            column(
              width=12,

              if(!is.null(r$configWiz$createDir)){
                tags$b(paste0(r$configWiz$createDir,"/",r$configWiz$configName))
              }
            )
          )
        }
      })
      #############################
      ####Server side functions####
      #############################

      ####Raw data filepath (dir)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dir
        },
        handlerExpr={
          req(input$dir)

          # if(input$dir > 0){
            shinyDirChoose(input, 'dir', roots=c(r$configWiz$roots, getwd()), session=session)

            if(!is.null(input$dir) && length(input$dir)>1){
              r$configWiz$injPath <- parseDirPath(r$configWiz$roots, input$dir)
            }
          # }
        }
      )

      ####Backup1 filepath (dir2)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dir2
        },
        handlerExpr={
          req(input$dir2)

          shinyDirChoose(input, 'dir2', roots=c(r$configWiz$roots, getwd()), session=session)

          if(!is.null(input$dir2)){
            r$configWiz$path2 <- parseDirPath(r$configWiz$roots, input$dir2)
          }
        }
      )


      ####mzML filepath (dir3)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dir3
        },
        handlerExpr={
          req(input$dir3)

          shinyDirChoose(input, 'dir3', roots=c(r$configWiz$roots, getwd()), session=session)

          if(!is.null(input$dir3)){
            r$configWiz$path3 <- parseDirPath(r$configWiz$roots, input$dir3)
          }
        }
      )

      ####msconvert.exe filepath (dir4)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dir4
        },
        handlerExpr={
          # req(input$dir4)

          shinyFileChoose(input, 'dir4', root=c(r$configWiz$roots,wd="."), filetypes=c('', 'exe'), session=session)

          # if(!is.null(input$dir4)){
          if(!is.null(input$dir4) && any(grepl(".exe",as.character(input$dir4)))){
              r$configWiz$path4 <- as.character(parseFilePaths(r$configWiz$roots,input$dir4)$datapath)
          }
        }
      )

      ####DB filepath (dir5)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dir5
        },
        handlerExpr={
          shinyFileChoose(input,'dir5', root=c(r$configWiz$roots,wd="."), filetypes=c('','db','sqlite'), session=session)

          if(!is.null(input$dir5)){
            fileSel<-parseFilePaths(r$configWiz$roots,input$dir5)
            r$configWiz$path5<-as.character(fileSel$datapath)
          }
        }
      )

      ####noCheck input (noCheck)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$noCheck
        },
        handlerExpr={
          if(input$noCheck!=""){
            r$configWiz$noCheck <- input$noCheck
          }
        }
      )

      ####instrument input (instrument)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$instrument
        },
        handlerExpr={
          if(input$instrument!=''){
            r$configWiz$instrument <- input$instrument
          }
        }
      )

      ####matrix of samples (sampMatrix)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$sampMatrix
        },
        handlerExpr={
          if(input$sampMatrix != ''){
            r$configWiz$sampMatrix <- input$sampMatrix
          }
        }
      )

      ####Delta RT and delta PPM for LaMas (dRT)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dRT
        },
        handlerExpr={
          r$configWiz$dPPM <- input$dPPM
          r$configWiz$dRT  <- input$dRT
          r$configWiz$nSampMonitor <- input$nSampMonitor
        }
      )

      ####Alpha value, sleep time and min file size (minFileSize)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$minFileSize
        },
        handlerExpr={
          r$configWiz$alpha <- input$alpha
          r$configWiz$sleep <- input$sleep
          r$configWiz$minFileSize <- input$minFileSize
        }
      )

      ####Folder depth, file format and minimum file size (fileFormat)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$fileFormat
        },
        handlerExpr={
          r$configWiz$folderDepth<-input$folderDepth
          r$configWiz$fileFormat<-input$fileFormat
          r$configWiz$ltQCName<-input$ltQCName
        }
      )

      ####Slack settings (slackToken)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$slackToken
        },
        handlerExpr={
          r$configWiz$slackToken <- input$slackToken
        }
      )

      ####Checking if config name has ".txt" at end (configName)####
      observeEvent(
        ignoreNULL=T,
        eventExpr={
          input$configName
        },
        handlerExpr={
          if(grepl(".txt", input$configName)){
            r$configWiz$configName <- input$configName
          } else
            r$configWiz$configName <- paste0(input$configName,".txt")
        }
      )
      ####Choose folder to create config (createDir)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$createDir
        },
        handlerExpr={
          req(input$createDir)
          req(r$configWiz$configName)

          shinyDirChoose(input, 'createDir', roots=c(r$configWiz$roots, getwd()), session=session)

          if(!is.null(input$createDir) && length(input$createDir)>1){
            if(!file.exists(paste0(parseDirPath(r$configWiz$roots, input$createDir),r$configWiz$configName,".txt"))){
              r$configWiz$createDir <- parseDirPath(r$configWiz$roots, input$createDir)
            } else {
              showModal(
                modalDialog(
                  title="File already exists",
                  easyClose=TRUE,
                  paste0(paste0(parseDirPath(r$configWiz$roots, input$createDir),r$configWiz$configName,".txt"), " already exists. Choose another directory or name.")
                )
              )
            }

          }
        }
      )


      ####Create button (create)####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$create
        },
        handlerExpr={
          req(r$configWiz$createDir)
          req(r$configWiz$configName)

          isolate({
            #Consider making a function which takes r$configWiz as an argument and extracts all the information from within
            configOutput<-data.frame(matrix(data=NA, nrow=89, ncol=1))
            configOutput[1,1]<-"Filepath to folder that .d files are created in:"
            configOutput[2,1]<-as.character(r$configWiz$injPath)
            configOutput[3,1]<-"Filepath to backup directory"
            configOutput[4,1]<-ifelse(input$checkBackup==FALSE, ifelse(is.null(r$configWiz$path2), "", as.character(r$configWiz$path2)), "")
            configOutput[5,1]<-"Filepath to folder that mzMLs are created in:"
            configOutput[6,1]<-as.character(r$configWiz$path3)
            configOutput[7,1]<-"Filepath to msconvert:"
            configOutput[8,1]<-as.character(r$configWiz$path4)
            configOutput[9,1]<-"Filepath to database object:"
            configOutput[10,1]<-as.character(r$configWiz$path5)
            configOutput[11,1]<-"noCheck vector (list of sample names to not be quality monitored, separated by space):"
            configOutput[12,1]<-as.character(input$noCheck)
            configOutput[13,1]<-"instrument:"
            configOutput[14,1]<-as.character(input$instrument)
            configOutput[15,1]<-"--ltQC--"
            configOutput[16,1]<-ifelse(is.null(input$ltQCName), "", as.character(input$ltQCName))
            configOutput[17,1]<-"Sample matrix:"
            configOutput[18,1]<-as.character(input$sampMatrix)
            configOutput[19,1]<-"Accepted ppm mz difference for landmarks:"
            configOutput[20,1]<-as.character(input$dPPM)
            configOutput[21,1]<-"Retention time window for finding landmarks:"
            configOutput[22,1]<-as.character(input$dRT)
            configOutput[23,1]<-"Alpha value used for statistical tests:"
            configOutput[24,1]<-as.character(input$alpha)
            configOutput[25,1]<-"Sleep time:"
            configOutput[26,1]<-as.character(input$sleep)
            configOutput[27,1]<-"--Min file size to avoid copying empty files--"
            configOutput[28,1]<-as.character(input$minFileSize)
            configOutput[29,1]<-"--Folder depth for checking----"
            configOutput[30,1]<-paste0(paste(rep("*/",input$folderDepth),collapse=""),"*",input$fileFormat)
            configOutput[31,]<-"--FileFormat--"
            configOutput[32,]<-as.character(input$fileFormat)
            configOutput[33,]<-"--SlackChannel hard limit--"
            configOutput[34,]<-ifelse(is.null(input$hardLimChannel1), "", as.character(input$hardLimChannel1))
            configOutput[35,]<-"--SlackChannel log--"
            configOutput[36,]<-ifelse(is.null(input$hardLimChannel2), "", as.character(input$hardLimChannel2))
            configOutput[37,]<-"--slackToken--"
            configOutput[38,]<-ifelse(is.null(r$configWiz$slackToken), "", as.character(r$configWiz$slackToken))

            #RP Soft
            configOutput[39,]<-"----Softlimits RP----"
            configOutput[40,]<-"Number of landmarks found:"
            configOutput[41,]<-"0"
            configOutput[42,]<-"Number of Peaks:"
            configOutput[43,]<-"0"
            configOutput[44,]<-"IPO score:"
            configOutput[45,]<-"0"
            configOutput[46,]<-"Proportion of LM intensity outliers:"
            configOutput[47,]<-"0"
            configOutput[48,]<-"Proportion of LM RT outliers:"
            configOutput[49,]<-"0"
            configOutput[50,]<-"RPTICSoftLim"
            configOutput[51,]<-"0"
            configOutput[52,]<-"RPNoiseSoftLim"
            configOutput[53,]<-"0"
            configOutput[54,]<-"RPHeightSoftLim"
            configOutput[55,]<-"0"
            configOutput[56,]<-"RPTFSoftLim"
            configOutput[57,]<-"0"
            configOutput[58,]<-"RPSNSoftLim"
            configOutput[59,]<-"0"
            configOutput[60,]<-"RPDataPointSoftLim"
            configOutput[61,]<-"0"
            configOutput[62,]<-"RPFWHMSoftLim"
            configOutput[63,]<-"0"

            #RN Soft
            configOutput[64,]<-"----Softlimits RN----"
            configOutput[65,]<-"Number of landmarks found:"
            configOutput[66,]<-"0"
            configOutput[67,]<-"Number of Peaks:"
            configOutput[68,]<-"0"
            configOutput[69,]<-"IPO score:"
            configOutput[70,]<-"0"
            configOutput[71,]<-"Proportion of LM intensity outliers:"
            configOutput[72,]<-"0"
            configOutput[73,]<-"Proportion of LM RT outliers:"
            configOutput[74,]<-"0"
            configOutput[75,]<-"RNTICSoftLim"
            configOutput[76,]<-"0"
            configOutput[77,]<-"RNNoiseSoftLim"
            configOutput[78,]<-"0"
            configOutput[79,]<-"RNHeightSoftLim"
            configOutput[80,]<-"0"
            configOutput[81,]<-"RNTFSoftLim"
            configOutput[82,]<-"0"
            configOutput[83,]<-"RNSNSoftLim"
            configOutput[84,]<-"0"
            configOutput[85,]<-"RNDataPointSoftLim"
            configOutput[86,]<-"0"
            configOutput[87,]<-"RNFWHMSoftLim"
            configOutput[88,]<-"0"

            #HP Soft
            configOutput[89,]<-"----Softlimits HP----"
            configOutput[90,]<-"Number of landmarks found:"
            configOutput[91,]<-"0"
            configOutput[92,]<-"Number of Peaks:"
            configOutput[93,]<-"0"
            configOutput[94,]<-"IPO score:"
            configOutput[95,]<-"0"
            configOutput[96,]<-"Proportion of LM intensity outliers:"
            configOutput[97,]<-"0"
            configOutput[98,]<-"Proportion of LM RT outliers:"
            configOutput[99,]<-"0"
            configOutput[100,]<-"HPTICSoftLim"
            configOutput[101,]<-"0"
            configOutput[102,]<-"HPNoiseSoftLim"
            configOutput[103,]<-"0"
            configOutput[104,]<-"HPHeightSoftLim"
            configOutput[105,]<-"0"
            configOutput[106,]<-"HPTFSoftLim"
            configOutput[107,]<-"0"
            configOutput[108,]<-"HPSNSoftLim"
            configOutput[109,]<-"0"
            configOutput[110,]<-"HPDataPointSoftLim"
            configOutput[111,]<-"0"
            configOutput[112,]<-"HPFWHMSoftLim"
            configOutput[113,]<-"0"

            #HN Soft
            configOutput[114,]<-"----Softlimits HN----"
            configOutput[115,]<-"Number of landmarks found:"
            configOutput[116,]<-"0"
            configOutput[117,]<-"Number of Peaks:"
            configOutput[118,]<-"0"
            configOutput[119,]<-"IPO score:"
            configOutput[120,]<-"0"
            configOutput[121,]<-"Proportion of LM intensity outliers:"
            configOutput[122,]<-"0"
            configOutput[123,]<-"Proportion of LM RT outliers:"
            configOutput[124,]<-"0"
            configOutput[125,]<-"HNTICSoftLim"
            configOutput[126,]<-"0"
            configOutput[127,]<-"HNNoiseSoftLim"
            configOutput[128,]<-"0"
            configOutput[129,]<-"HNHeightSoftLim"
            configOutput[130,]<-"0"
            configOutput[131,]<-"HNTFSoftLim"
            configOutput[132,]<-"0"
            configOutput[133,]<-"HNSNSoftLim"
            configOutput[134,]<-"0"
            configOutput[135,]<-"HNDataPointSoftLim"
            configOutput[136,]<-"0"
            configOutput[137,]<-"HNFWHMSoftLim"
            configOutput[138,]<-"0"

            #Hard limits
            #RP hard
            configOutput[139,]<-"--Hardlimits RP--"
            configOutput[140,]<-"Number of landmarks found:"
            configOutput[141,]<-"0"
            configOutput[142,]<-"Number of Peaks:"
            configOutput[143,]<-"0"
            configOutput[144,]<-"IPO score:"
            configOutput[145,]<-"0"
            configOutput[146,]<-"Proportion of LM intensity outliers"
            configOutput[147,]<-"0"
            configOutput[148,]<-"Proportion of LM RT outliers:"
            configOutput[149,]<-"0"
            configOutput[150,]<-"RPTICHardLim"
            configOutput[151,]<-"0"
            configOutput[152,]<-"RPNoiseHardLim"
            configOutput[153,]<-"0"
            configOutput[154,]<-"RPHeightHardLim"
            configOutput[155,]<-"0"
            configOutput[156,]<-"RPTFHardLim"
            configOutput[157,]<-"0"
            configOutput[158,]<-"RPSNHardLim"
            configOutput[159,]<-"0"
            configOutput[160,]<-"RPDataPointHardLim"
            configOutput[161,]<-"0"
            configOutput[162,]<-"RPFWHMHardLim"
            configOutput[163,]<-"0"

            #RN hard
            configOutput[164,]<-"--Hardlimits RN--"
            configOutput[165,]<-"Number of landmarks found:"
            configOutput[166,]<-"0"
            configOutput[167,]<-"Number of Peaks:"
            configOutput[168,]<-"0"
            configOutput[169,]<-"IPO score:"
            configOutput[170,]<-"0"
            configOutput[171,]<-"Proportion of LM intensity outliers"
            configOutput[172,]<-"0"
            configOutput[173,]<-"Proportion of LM RT outliers:"
            configOutput[174,]<-"0"
            configOutput[175,]<-"RNTICHardLim"
            configOutput[176,]<-"0"
            configOutput[177,]<-"RNNoiseHardLim"
            configOutput[178,]<-"0"
            configOutput[179,]<-"RNHeightHardLim"
            configOutput[180,]<-"0"
            configOutput[181,]<-"RNTFHardLim"
            configOutput[182,]<-"0"
            configOutput[183,]<-"RNSNHardLim"
            configOutput[184,]<-"0"
            configOutput[185,]<-"RNDataPointHardLim"
            configOutput[186,]<-"0"
            configOutput[187,]<-"RNFWHMHardLim"
            configOutput[188,]<-"0"

            #HP hard
            configOutput[189,]<-"--Hardlimits HP--"
            configOutput[190,]<-"Number of landmarks found:"
            configOutput[191,]<-"0"
            configOutput[192,]<-"Number of Peaks:"
            configOutput[193,]<-"0"
            configOutput[194,]<-"IPO score:"
            configOutput[195,]<-"0"
            configOutput[196,]<-"Proportion of LM intensity outliers"
            configOutput[197,]<-"0"
            configOutput[198,]<-"Proportion of LM RT outliers:"
            configOutput[199,]<-"0"
            configOutput[200,]<-"HPTICHardLim"
            configOutput[201,]<-"0"
            configOutput[202,]<-"HPNoiseHardLim"
            configOutput[203,]<-"0"
            configOutput[204,]<-"HPHeightHardLim"
            configOutput[205,]<-"0"
            configOutput[206,]<-"HPTFHardLim"
            configOutput[207,]<-"0"
            configOutput[208,]<-"HPSNHardLim"
            configOutput[209,]<-"0"
            configOutput[210,]<-"HPDataPointHardLim"
            configOutput[211,]<-"0"
            configOutput[212,]<-"HPFWHMHardLim"
            configOutput[213,]<-"0"

            #HN hard
            configOutput[214,]<-"--Hardlimits HN--"
            configOutput[215,]<-"Number of landmarks found:"
            configOutput[216,]<-"0"
            configOutput[217,]<-"Number of Peaks:"
            configOutput[218,]<-"0"
            configOutput[219,]<-"IPO score:"
            configOutput[220,]<-"0"
            configOutput[221,]<-"Proportion of LM intensity outliers"
            configOutput[222,]<-"0"
            configOutput[223,]<-"Proportion of LM RT outliers:"
            configOutput[224,]<-"0"
            configOutput[225,]<-"HNTICHardLim"
            configOutput[226,]<-"0"
            configOutput[227,]<-"HNNoiseHardLim"
            configOutput[228,]<-"0"
            configOutput[229,]<-"HNHeightHardLim"
            configOutput[230,]<-"0"
            configOutput[231,]<-"HNTFHardLim"
            configOutput[232,]<-"0"
            configOutput[233,]<-"HNSNHardLim"
            configOutput[234,]<-"0"
            configOutput[235,]<-"HNDataPointHardLim"
            configOutput[236,]<-"0"
            configOutput[237,]<-"HNFWHMHardLim"
            configOutput[238,]<-"0"

            #Status stuff
            configOutput[239,]<-"----statusInt----"
            configOutput[240,]<-"1"
            configOutput[241,]<-"--statusRT--"
            configOutput[242,]<-"1"
            configOutput[243,]<-"--statusHeight--"
            configOutput[244,]<-"1"
            configOutput[245,]<-"--statusFWHM--"
            configOutput[246,]<-"1"
            configOutput[247,]<-"--statusTF--"
            configOutput[248,]<-"1"
            configOutput[249,]<-"--statusSN--"
            configOutput[250,]<-"1"
            configOutput[251,]<-"--statusNoise--"
            configOutput[252,]<-"1"
            configOutput[253,]<-"--statusDataPoints--"
            configOutput[254,]<-"1"
            configOutput[255,]<-"--statusIPO--"
            configOutput[256,]<-"1"
            configOutput[257,]<-"--statusNPeaks--"
            configOutput[258,]<-"1"
            configOutput[259,]<-"--statusTIC--"
            configOutput[260,]<-"1"
            configOutput[261,]<-"--statusnLM--"
            configOutput[262,]<-"1"
            configOutput[263,]<-"--StatusLim--"
            configOutput[264,]<-"0.2"
            configOutput[265,]<-"--nSampMonitor--"
            configOutput[266,]<-as.character(input$nSampsMonitor)
            configOutput[267,]<-"" #For readLine not to freak out :)
          })


          #Write .txt config file
          write.table(configOutput,file=paste0(r$configWiz$createDir,"/",r$configWiz$configName), row.names=FALSE, col.names=FALSE, quote = FALSE)
          isolate(r$configWiz$preConfig<-paste0(r$configWiz$createDir,"/",r$configWiz$configName))

          #Save the config file location to r$sidebar$preConfig
          showNotification("Config file created.")
        }
      )


    }
  )
}
