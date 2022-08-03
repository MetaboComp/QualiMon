runBatchUI<-function(id){
  ns<-NS(id)

  tagList(
    fluidRow(
      box(
        title="Setup batch run",
        width=8,
        solidHeader=T,

        uiOutput(ns('chooseConfigUI'))

      ),


    )
  )

}

runBatchServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns
      processReady <- reactiveVal(1)

      ###############################
      #### Server side rendering ####
      ###############################

      ####Rendering input stuff for running batches####
      output$chooseConfigUI <- renderUI({
        tagList(
          fluidRow(

            column(
              width=4,

              if(is.null(r$runBatch$cfgFilePath)){
                tagList(
                  tags$b("No config file loaded.")
                )
              } else {
                tagList(
                  tags$b("Config file: "),
                  r$runBatch$cfgFilePath
                )
              },
              br(),
              shinyFilesButton(id=ns('cfgFilePath'), "Choose cfg file", title="Choose .txt config file", multiple=F),
              br(),
              br(),
              tags$b("Choose the folder with batch folders inside"),
              br(),
              shinyDirButton(ns('dirChooser'), "Choose folder", title="Choose folder with folders to be run within", multiple=F)
            ),

            column(
              width=4,

              pickerInput(
                inputId = ns("subDirChooser"),
                label="Choose batch-folders to run",
                choices=r$runBatch$subDirs,
                # selected=input$subDirChooser,
                options=list(
                  'actions-box' = TRUE,
                  size=10,
                  'selected-text-format'="count>3"
                ),
                multiple=TRUE
              ),
              br()
            ),

            column(
              width=4,

              textInput(
                inputId = ns("projName"),
                label="Name of batch project",
                # value=input$projName
              ),
              br(),
              uiOutput(ns('processButtonUI'))
            )
          )
        )
      })

      ####Render process-button when all prerequisites are present####
      output$processButtonUI <- renderUI({
        req(r$runBatch$cfgFilePath)
        req(r$runBatch$subPaths)
        req(input$subDirChooser)
        req(processReady())

        if(input$projName != ""){
            actionButton(ns('processButton'), "Process batches")
        }
      })


      #################################################
      #### Handling all parameter inputs from user ####
      #################################################
      ####Input which config file to use####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$cfgFilePath
        },
        handlerExpr={
          shinyFileChoose(input,'cfgFilePath', root=r$configWiz$roots, filetypes=c('','txt'), session=session)

          if(!is.null(input$cfgFilePath) && length(grep(".txt",as.character(input$cfgFilePath)))>0){

            fileSelMonitor<-parseFilePaths(r$configWiz$roots,input$cfgFilePath)
            r$runBatch$cfgFilePath<-as.character(fileSelMonitor$datapath)
          }

          updateTextInput(session=session, 'projName', value=input$projName)
          updatePickerInput(session=session, 'subDirChooser', selected=input$subDirChooser)
        }
      )

      ####Input which folder which houses all the subfolders which are batch-folders####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$dirChooser
        },
        handlerExpr={
          req(input$dirChooser)

          shinyDirChoose(input,'dirChooser', roots=c(r$configWiz$roots, getwd()), session=session)

          if(!is.null(input$dirChooser) && length(input$dirChooser)>1){
            isolate({
              r$runBatch$path <- parseDirPath(r$configWiz$roots,input$dirChooser)
              r$runBatch$subDirs <- list.dirs(r$runBatch$path, full.names=FALSE, recursive=FALSE)
            })

            r$runBatch$subPaths <- list.dirs(r$runBatch$path, full.names=TRUE, recursive=FALSE)
          }

          updateTextInput(session=session, 'projName', value=input$projName)
          updatePickerInput(session=session, 'subDirChooser', selected=input$subDirChooser)
        }
      )

      ####Process batch####
      observeEvent(
        ignoreNULL=TRUE,
        eventExpr={
          input$processButton
        },
        handlerExpr={
          ####Transfer all r variables to local variables
          choosenDirs <- r$runBatch$subPaths[which(r$runBatch$subDirs%in%input$subDirChooser)]
          projName <- input$projName
          cfgFilePath <- r$runBatch$cfgFilePath

          #Checking that there are mzML files in all batch folders
          nFiles <- vector(length=length(choosenDirs))
          sumFiles <- 0

          for(i in 1:length(choosenDirs)){
            if(length(list.files(choosenDirs[i], ".mzML")) <1){
              nFiles[i] = TRUE
            }
            sumFiles <- sumFiles + length(list.files(choosenDirs[i]))
          }

          #If any folder missing .mzML user informed, otherwise starts running batches
          if(any(nFiles==TRUE)){
            showModal(
              modalDialog(
                title="Missing .mzML files",
                paste0("No .mzML files found in folders:"),
                br(),
                paste(choosenDirs[nFiles], sep="\n"),
                easyClose=TRUE
              )
            )
          }
          else{
            #Running all samples in all folders selected by user
            progressBatchRun <- AsyncProgress$new(message="Batch processing old files\n")
            # processReady(NULL)

            future_promise(seed=NULL,
              {
                for(i in 1:length(choosenDirs)){
                  progressBatchRun$set(detail=paste0("\n",basename(choosenDirs[i])))
                  checkLMBatch(choosenDirs[i], projName, cwp=NULL, readConfigFile(cfgFilePath), sumFiles, progressBatchRun)
                }
                progressBatchRun$close()
              }) %>% then(
                onRejected= function(err){
                  progressBatchRun$close()
                  showModal(
                    modalDialog(
                      title="!!Error!!",
                      paste0("An error occured: ",err),
                      easyClose=F
                    )
                  )
                }
              )
            NULL
          }
        }
      )
    }
  )
}
