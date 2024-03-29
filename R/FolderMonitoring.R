#' Function to start monitoring .d files from LC/MS
#'
#' @param configFilePath File path to config file to use for monitoring
#' @param availableChromPols All chromPols for which LaMas are available
#'
#' @return Messages regarding the sample quality

initFolderMonitoring <- function(configFilePath, availableChromPols, progressMonitor=function(i) cat(".")){
  #Setting up parameters from config file
  Config <- readConfigFile(configFilePath)
  dir <- Config$dir
  splitlengthdir <- length(strsplit(dir, split="/")[[1]])
  tmp_sink_dir <- Config$tmp_sink_dir
  outdir <- Config$outdir
  msconvert <- Config$msconvert
  DB <- Config$dbName
  noCheck <- Config$noCheck
  projectID <- Config$projectID
  instrument <- Config$instrument
  sampleMatrix <- Config$sampleMatrix
  dPPM <- Config$dPPM
  rtWin <- Config$rtWin
  alpha <- Config$alpha
  sleep.time <- Config$sleep.time
  folderDepth<-Config$folderDepth
  slackChannelHard<-Config$slackChannelHard
  slackChannelLog<-Config$slackChannelLog
  minFileSize<-Config$minFileSize
  ltQCname<-Config$ltQCname
  fileFormat<-Config$fileFormat
  # projToCheck <- Config$doCheckProj

  #Check if raw file folder exists, otherwise exit function
  if (!dir.exists(dir)){
    if(!dir.exists(paste0(dir,"/"))){
      print('Warning, file directory where raw files are generated does not exist!')
      return()
    }
  } else {
    if(str_sub(dir,str_length(dir),str_length(dir)) != "/"){
      dir <- paste0(dir,"/")
    }
  }

  if (!file.exists(outdir)){
    suppressWarnings(dir.create(outdir, recursive=T))
  }
  # if (!file.exists(tmp_sink_dir)){
  #   suppressWarnings(dir.create(tmp_sink_dir, recursive=T))
  # }
  if (!file.exists(msconvert)){
    print('Warning, filepath to msconvert.exe does not exist, please ensure you have entered the correct path')
    return()
  }

  #Setting up starting conditions, such as time that the script was started etc.
  runTime <- 0
  startTime <- proc.time()[3]
  dFiles <- Sys.glob(paste0(dir, folderDepth))
  start.state <- length(dFiles) #Number of files in folder supplied by user
  change.state <- 0
  done_samples <- Sys.glob(paste0(dir, folderDepth))
  done_samples <- sub(paste0('\\',fileFormat,'$'), '', basename(done_samples))

  #Main loop for monitoring new raw files
  while(TRUE){
    runTime <- proc.time()[3] - startTime
    change.state <- length(Sys.glob(paste0(dir, folderDepth))) #Checking how many files are in folder

    if(start.state < change.state) { #If number of files in folder are more than when script started, then proceed with conversions etc.
      start.state <- change.state


      #Checking when files were created, ordering the files by that and choosing the youngest file to track
      Files <- Sys.glob(paste0(dir, folderDepth))
      time.created <- file.info(Files)$ctime
      Files <- Files[order(time.created)]
      FileForConversion <- Files[length(Files)]
      Filesize <- sum(file.info(list.files(FileForConversion, recursive = T, all.files=T, include.dirs = T, full.names=T))$size)

      #ltQC functionality for having a separate ltQC project running
      if(length(grep(ltQCname,FileForConversion)>0)!=0){
        if(grep(ltQCname,FileForConversion)>0){
          projectID <- ltQCname
        }
      } else {
        projectID <- strsplit(FileForConversion, split="/")[[1]][splitlengthdir+1] #+1 to grab the correct name for the project
      }

      #Test if projectName already in DB, otherwise add it
      conn1 <- dbConnect(RSQLite::SQLite(),DB)
      sqliteSetBusyHandler(conn1, 40000)
      projectName<-dbGetQuery(conn1, sprintf("SELECT projectID FROM projects WHERE projName='%s'",
                                            projectID))
      dbDisconnect(conn1)
      if(dim(projectName)[1]==0){
        addProject(DB, projectID)
      }

      print(projectName)
      print(projectID)


      #Sub-loop checking the youngest file which has been identified as new is still growing in size until it's not growing anymore. minFileSize makes sure that the script doesn't copy empty files which causes error
      Sys.sleep(sleep.time)
      while(sum(file.info(list.files(FileForConversion, recursive = T, all.files=T, include.dirs = T, full.names=T))$size) > Filesize || Filesize < minFileSize){
        Filesize <- sum(file.info(list.files(FileForConversion, recursive = T, all.files=T, include.dirs = T, full.names=T))$size)
        Sys.sleep(sleep.time)
      }

      ## Replacing spaces with - in raw file to avoid errors
      ## HAS TO BE FIXED
      # if(length(grep(" ",basename(FileForConversion)))>0){
      #   NewName<-gsub(" ","-",basename(FileForConversion))
      #   file.rename(paste0(Backupdir,"/",basename(FileForConversion)), paste0(Backupdir,"/",NewName))
      #   FileForConversion<-NewName
      # }

      #Checking if user want backup: If yes, checking if backup folder exists, creating if none present
      if(tmp_sink_dir!=""){
        Filelocation <- strsplit(FileForConversion, split="/")[[1]][c(-1:-splitlengthdir)]
        Folderlocation <- paste0(Filelocation[-length(Filelocation)], collapse="/")
        ConvDir <- paste0(tmp_sink_dir, "/",Folderlocation)
        if(!file.exists(ConvDir)){
          suppressWarnings(dir.create(paste0(ConvDir), recursive = T))
        }

        #Copying file to backup computer and checking for " " naming error
        file.copy(FileForConversion, ConvDir, recursive=TRUE)
      } else {
        Filelocation <- strsplit(FileForConversion, split="/")[[1]][c(-1:-splitlengthdir)]
        ConvDir <- paste0(Filelocation[-length(Filelocation)], collapse="/")
      }

      # Checking if file is a rerun, and if so, renames it
      if(length(grep("rerun",basename(FileForConversion)))>0){
        NewName<-gsub("-rerun","",basename(FileForConversion))
        strVec<-unlist(strsplit(NewName,"_"))
        NewName<-paste(strVec[1],strVec[2],strVec[3],strVec[4],paste0(strVec[5],"-rerun"),strVec[6],sep="_")
        file.rename(paste0(ConvDir,"/",basename(FileForConversion)), paste0(ConvDir,"/",NewName))
        FileForConversion<-NewName
      }

      #Creating a .mzML file from original/backup folder, depending on user specification
      FileBaseName <- basename(FileForConversion)
      mzMLdir <- paste0(outdir, "/",Folderlocation)
      if(!file.exists(mzMLdir)){
        suppressWarnings(dir.create(paste0(mzMLdir), recursive = T))
      }
      cmd <- paste("cd /d", paste0("\"",ConvDir,"\""),"&&" ,paste0("\"",msconvert,"\"") , "--64 --zlib --mzML -o",paste0("\"",mzMLdir,"\""), FileBaseName)
      shell(cmd)

      #Collect filepath and filename of files, file.info(...)$ctime used to find the most recent file
      fullname <- list.files(path=mzMLdir, pattern="\\.mzML$", full.names=T)
      filenames <- basename(fullname)
      Samples <- data.frame(fullname, filenames)
      Samples$filenames <- as.character(Samples$filenames)
      mzMLcreated <- file.info(list.files(path=mzMLdir, pattern="\\.mzML$", full.names=T))$ctime
      Samples<- Samples[order(mzMLcreated),]
      Sample <- Samples[nrow(Samples),]


      #Calling "checkLM" to compare landmarks in the last .mzML file to be created with the landmark-information in the DB
      #Checking if filename conforms to file naming standard
      if(length(strsplit(Sample$filenames, split='_')[[1]])==6){ #&& (projectID %in% projToCheck)
        #Checking if chromPol of sample is available in DB
        currChromPol <- paste0(strsplit(Sample$filenames, split='_')[[1]][3], "_", strsplit(Sample$filenames, split='_')[[1]][4])
        convChromPol <- switch(currChromPol, "RP_POS"="RP", "RP_NEG"="RN", "HILIC_POS"="HP", "HILIC_NEG"="HN")
        print("First if after copy + mzML conversion")

        if(convChromPol %in% availableChromPols){
          Sample$sample <- strsplit(Sample$filenames, split='_')[[1]][5]
          print("Just prior to checkLM")

          ####CheckLM####
          checkLM(filePath=paste0(mzMLdir, "/", Sample$filenames), dbName=DB, instrument = instrument, projName=projectID, sampleMatrix=sampleMatrix, Config=Config, dPPM = dPPM, rtWin=rtWin, noCheck = noCheck, alpha = alpha, slackOn = T)
        } else {
          # cat('\nNo LaMas for the chromPol of the current sample. Sample not submitted to DB or monitored.')
        }
      } else {
        # cat('\nFilename does not conform to naming strategy, file will not be checked.')
      }
    } else {
    }
    Sys.sleep(sleep.time)
  }
}
