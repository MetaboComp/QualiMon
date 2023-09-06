#' checkLMBatch - A function for performing checkLM on a batch of samples
#'
#' @param fileDir A filepath to the directory where the .mzML files which are to be checkLM analyzed are located
#' @param projName If the user has supplied a projName it will be submitted to the DB
#' @param cwp Option to specify custom centwave-parameters (see XCMS package). Otherwise uses default (View(check_LM))
#' @param Config Config object from readConfigFile function
#' @param sumFiles Tracking progress of checkLMBatch
#' @param progressBatchRun The dPPM window for comparing peaks in sample against landmarks in DB
#'
#' @return Nothing


checkLMBatch<-function(fileDir, projName, cwp=NULL, Config, sumFiles, progressBatchRun, progressMonitor=function(i) cat(".")){

  #Checking if project name is present in DB
  conn <- dbConnect(RSQLite::SQLite(),Config$dbName)
  projectName<-dbGetQuery(conn, sprintf("SELECT projectID FROM projects WHERE projName='%s'",
                                        projName))
  # dbDisconnect(conn)
  if(dim(projectName)[1]==0){
    # print("Project does not exist. Adding as a new project.")
    addProject(Config$dbName, projName)
  }

  #Checking if ltQC name present in DB
  # conn <- dbConnect(RSQLite::SQLite(),dbName)
  projectName<-dbGetQuery(conn, sprintf("SELECT projectID FROM projects WHERE projName='%s'",
                                        Config$ltQCname))
  dbDisconnect(conn)

  if(dim(projectName)[1]==0){
    # print("ltQC project does not exist. Adding as a new project.")
    addProject(Config$dbName, Config$ltQCname)
  }

  # toFetch <- list(type="sample", chromPol="RN", projName=projName)

  files <- list.files(path=fileDir)
  files <- files[order(str_split_fixed(files, "_", 6)[,6])]

  #Looping through all samples on list / in filepath and performing checkLM() on the$
  for(i in 1:length(files)){
    file<-files[i]

    #ltQC functionality for having a separate ltQC project running
    if(length(grep(Config$ltQCname,file)>0)!=0){
      if(grep(Config$ltQCname,file)>0){ #MIGHT BE UNNECCESSARY
        projSubmit <- Config$ltQCname
      }
    } else {
      projSubmit <- projName
    }

    checkLM(filePath=paste0(fileDir,"\\",file),
            dbName=Config$dbName,
            projName=projSubmit,
            sampleMatrix=Config$sampleMatrix,
            instrument=Config$instrument,
            dPPM=Config$dPPM,
            rtWin=Config$rtWin,
            alpha=Config$alpha,
            noCheck=Config$noCheck,
            Config=Config,
            slackOn=F,
            batch=T)
    #Async progression update
    progressBatchRun$inc(1/sumFiles)
  }


  # writeLines(paste("checkLM performed for all",i,"files."))
}
