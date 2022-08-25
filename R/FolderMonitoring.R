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

  ##make initCheck for folders. (apply to shiny interface?)
  if (!file.exists(dir)){
    if(!file.exists(paste0(dir,"/"))){
      print('Warning, file directory where raw files are generated does not exist!')
      return()
    }
  } else {
    if(str_sub(dir,str_length(dir),str_length(dir)) != "/"){
      dir <- paste0(dir,"/")
    }
  }
  # if (!file.exists(rep.path)){
  #   suppressWarnings(dir.create(rep.path, recursive=T))
  # }
  if (!file.exists(outdir)){
    suppressWarnings(dir.create(outdir, recursive=T))
  }
  if (!file.exists(tmp_sink_dir)){
    suppressWarnings(dir.create(tmp_sink_dir, recursive=T))
  }
  if (!file.exists(msconvert)){
    print('Warning, filepath to msconvert.exe does not exist, please ensure you have entered the correct path')
    return()
  }

  # if (!file.exists(paste0(rep.path,reportName))){
  #   temp = matrix(nrow = 1, ncol = 18)
  #     colnames(temp) = c("FilePath","LM_Int_outliers_num","LM_Int_outlier_p",
  #                        "LM_rt_outliers_num", "LM_rt_outlier_p",
  #                        "LM_numbers", "LM_n_p",
  #                        "LM_IPO_score", "LM_IPO_score_p", "Peak_number",
  #                        "Peak_number_p", "TIC", "TIC_p", "noise", "noise_p","Status","Sample_number", "Iter")
  #     fwrite(temp,paste0(rep.path,reportName), sep=";", row.names=F)
  # }

  #Setting up starting conditions, such as time that the script was started etc.
  runTime <- 0
  startTime <- proc.time()[3]
  dFiles <- Sys.glob(paste0(dir, folderDepth))
  start.state <- length(dFiles) #Number of files in folder supplied by user
  change.state <- 0
  done_samples <- Sys.glob(paste0(dir, folderDepth))
  done_samples <- sub(paste0('\\',fileFormat,'$'), '', basename(done_samples))

  while(TRUE){
    write.table("Awaiting new file...", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
    runTime <- proc.time()[3] - startTime
    change.state <- length(Sys.glob(paste0(dir, folderDepth))) #Checking how many files are in folder and storing in "change.state"

    if(start.state < change.state) { #If number of files in folder are more than when script started, then proceed with conversions etc.
      start.state <- change.state


      #Checking when files were created, ordering the files by that and choosing the youngest file to track
      Files <- Sys.glob(paste0(dir, folderDepth))
      time.created <- file.info(Files)$ctime
      Files <- Files[order(time.created)]
      FileForConversion <- Files[length(Files)]
      Filesize <- sum(file.info(list.files(FileForConversion, recursive = T, all.files=T, include.dirs = T, full.names=T))$size)

      write.table(Files, "data/status/log.txt", sep=";", row.names = FALSE, col.names = FALSE)

      #ltQC functionality for having a separate ltQC project running
      if(length(grep(ltQCname,FileForConversion)>0)!=0){
        if(grep(ltQCname,FileForConversion)>0){
          projectID <- ltQCname
        }
      } else {
        projectID <- strsplit(FileForConversion, split="/")[[1]][splitlengthdir+1] #+1 to grab the correct name for the project
      }

      #Test if projectName already in DB, otherwise ask user to add new project
      conn <- dbConnect(RSQLite::SQLite(),DB)
      projectName<-dbGetQuery(conn, sprintf("SELECT projectID FROM projects WHERE projName='%s'",
                                            projectID))
      dbDisconnect(conn)
      if(dim(projectName)[1]==0){
        print("Project does not exist. Adding as a new project.")
        addProject(DB, projectID)
      }

      Sys.sleep(sleep.time)
      #Sub-loop checking the youngest file which has been identified as new is still growing in size until it's not growing anymore. minFileSize makes sure that the script doesn't copy empty files which causes error
      while(sum(file.info(list.files(FileForConversion, recursive = T, all.files=T, include.dirs = T, full.names=T))$size) > Filesize || Filesize < minFileSize){
        write.table("New file found. Monitoring file size", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
        print(paste0(sum(file.info(list.files(FileForConversion, recursive = T, all.files=T, include.dirs = T, full.names=T))$size)))
        Filesize <- sum(file.info(list.files(FileForConversion, recursive = T, all.files=T, include.dirs = T, full.names=T))$size)
        Sys.sleep(sleep.time)
      }

      #Calling msconvert through commando line
      # TODO: doesn't work on network mapped folders
      #shell(cmd=paste("cd", paste0("\"",dir,"\""),"&&" ,paste0("\"",msconvert,"\"") , "--64 --zlib --mzML -o",paste0("\"",outdir,"\""), FileForConversion))

      # Workaround was to use a sink
      #dir.create(tmp_sink_dir)
      Filelocation <- strsplit(FileForConversion, split="/")[[1]][c(-1:-splitlengthdir)]
      Folderlocation <- paste0(Filelocation[-length(Filelocation)], collapse="/")

      write.table(cbind(Filelocation, Folderlocation), "data/status/log.txt", sep=";", row.names = FALSE, col.names = FALSE, append=TRUE)

      write.table("Making backup of raw file", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
      Backupdir <- paste0(tmp_sink_dir, "/",Folderlocation)
      if(!file.exists(Backupdir)){
        suppressWarnings(dir.create(paste0(Backupdir), recursive = T))
      }

      #Copying file to backup computer and checking for " " naming error
      file.copy(FileForConversion, Backupdir, recursive=TRUE)
      if(length(grep(" ",basename(FileForConversion)))>0){
        NewName<-gsub(" ","-",basename(FileForConversion))
        file.rename(paste0(Backupdir,"/",basename(FileForConversion)), paste0(Backupdir,"/",NewName))
        FileForConversion<-NewName
      }

      #Checking if file is a rerun, and if so, renames it
      if(length(grep("rerun",basename(FileForConversion)))>0){
        NewName<-gsub("-rerun","",basename(FileForConversion))
        strVec<-unlist(strsplit(NewName,"_"))
        NewName<-paste(strVec[1],strVec[2],strVec[3],strVec[4],paste0(strVec[5],"-rerun"),strVec[6],sep="_")
        file.rename(paste0(Backupdir,"/",basename(FileForConversion)), paste0(Backupdir,"/",NewName))
        FileForConversion<-NewName
      }

      FileBaseName <- basename(FileForConversion)

      mzMLdir <- paste0(outdir, "/",Folderlocation)
      if(!file.exists(mzMLdir)){
        suppressWarnings(dir.create(paste0(mzMLdir), recursive = T))
      }

      write.table("Converting to .mzML format", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
      cmd <- paste("cd", paste0("\"",Backupdir,"\""),"&&" ,paste0("\"",msconvert,"\"") , "--64 --zlib --mzML -o",paste0("\"",mzMLdir,"\""), FileBaseName)
      #print(cmd)
      shell(cmd)
      #unlink(tmp_sink_dir, recursive=TRUE, force=TRUE)
      #Collect filepath and filename of files, file.info(...)$ctime used to find the most recent file
      fullname <- list.files(path=mzMLdir, pattern="\\.mzML$", full.names=T)
      filenames <- basename(fullname)
      Samples <- data.frame(fullname, filenames)
      Samples$filenames <- as.character(Samples$filenames)
      #behöver: filepath, filename samt sample type ()
      mzMLcreated <- file.info(list.files(path=mzMLdir, pattern="\\.mzML$", full.names=T))$ctime
      Samples<- Samples[order(mzMLcreated),]
      Sample <- Samples[nrow(Samples),]


      #Calling "checkLM" to compare landmarks in the last .mzML file to be created with the landmark-information in the DB
      #Checking if filename conforms to file naming standard
      if(length(strsplit(Sample$filenames, split='_')[[1]])==6){

        #Checking if chromPol of sample is available in DB
        currChromPol <- paste0(strsplit(Sample$filenames, split='_')[[1]][3], "_", strsplit(Sample$filenames, split='_')[[1]][4])
        convChromPol <- switch(currChromPol, "RP_POS"="RP", "RP_NEG"="RN", "HILIC_POS"="HP", "HILIC_NEG"="HN")

        if(convChromPol %in% availableChromPols){
          Sample$sample <- strsplit(Sample$filenames, split='_')[[1]][5]
          print("check LM for:")
          print(paste0(mzMLdir, "/", Sample$filenames))

          #Check if slack on from main app



          ####CheckLM####
          checkLM(filePath=paste0(mzMLdir, "/", Sample$filenames), dbName=DB, instrument = instrument, projName=projectID, sampleMatrix=sampleMatrix, Config=Config, dPPM = dPPM, rtWin=rtWin, noCheck = noCheck, alpha = alpha, slackOn = F)
        } else {
          cat('\nNo LaMas for the chromPol of the current sample. Sample not submitted to DB or monitored.')
        }

      } else {
        cat('\nFilename does not conform to naming strategy, file will not be checked.')
        #Building the report for the latest file to add it to the report file
        # check_LM_info = data.frame(matrix(nrow = 1, ncol = 28))
        # colnames(check_LM_info) = c("FilePath","LM_Int_outliers_num","LM_Int_outlier_p",
        #                             "LM_rt_outliers_num", "LM_rt_outlier_p",
        #                             "LM_Height_outliers_num", "LM_Height_outlier_p",
        #                             "LM_FWHM_outliers_num", "LM_FWHM_outlier_p",
        #                             "LM_TF_outliers_num", "LM_TF_outlier_p",
        #                             "LM_SN_outliers_num", "LM_SN_outlier_p",
        #                             "LM_DataPoints_outliers_num", "LM_DataPoints_outlier_p",
        #                             "LM_numbers", "LM_n_p",
        #                             "LM_IPO_score", "LM_IPO_score_p", "Peak_number",
        #                             "Peak_number_p", "TIC", "TIC_p", "LM_Noise_outliers_num", "noise_p", "Status", "Sample_number", "Iter")
        # check_LM_info[,1] = filePath
        # check_LM_info[,5] = LM_numbers
        # check_LM_info[,7] = LM_IPO_score
        # check_LM_info[,9] = nrow(chromPeaks(xdata_cwp))
        # check_LM_info[,26] = "Faulty filename"
        #
        # write.table(check_LM_info,paste0(rep.path,"/",reportName), append=TRUE, col.names=F, row.names=testInj$filePath, sep=";", quote=FALSE, row.names = FALSE, col.names = FALSE)
        # write.csv("Filename does not conform to naming strategy, please make sure that files are properly named", file = paste0(rep.path, "/Abnormalty report of ", Sample$filenames,".csv"))
      }
    } else {
      print(paste0("Nothing new. ", Sys.time())) #Used for functionality testing
    }
    Sys.sleep(sleep.time)
  }
  print('Monitoring finished')
}
