#' submitBatchToDB - A function for submitting injections to DB
#'
#' @param dir A diretion containing only .mzML files which are to be submitted to the DB
#' @param projName The project ID which will be assigned to all the entries into the DB
#' @param sampMatrix The sample matrix of all the samples that will be assigned to all the entries into the DB
#' @param dbName A list of names of the same length as the number of files in the direction 'dir'
#' @param instrument Description of instrument used to analyze the samples
#' @param cwp Option to specify custom centwave-parameters, see XCMS package.
#'
#' @return Nothing

submitBatchToDB<-function(dir, projName, sampMatrix, dbName="NameOfDB.db", instrument="QTOF", cwp=NULL){

  #Setting up data frames for interactions with DB
  fileList<-list.files(dir)
  fileList<-fileList[grep("mzML",fileList)]
  if(length(fileList)<1){
    writeLines("The directory does not contain any .mzML files to submit to the DB")
    return()
  }

  samples<-data.frame(sampID=integer(),
                      projName=integer(),
                      type=character(),
                      matrix=character(),
                      name=character())

  injections<-data.frame(injID=integer(),
                         sampID=integer(),
                         date=character(),
                         batchWeek=character(),
                         seqInj=integer(),
                         chromPol=character(),
                         instrument=character(),
                         MSMode=character(),
                         IPOscore=double(),
                         nLMs=integer())

  lmPeaks<-data.frame(peakIDLM=integer(),
                      injID=integer(),
                      mz=double(),
                      RT=double(),
                      int=double(),
                      LMID=integer())

  testInj<-list()
  LMPeaksToSub<-list()

  # Default centwave-parameters for xcms
  if(is.null(cwp)) {
    cwp <- CentWaveParam(peakwidth = c(8, 50),# Peak picking parameters for XCMS using centwave
                         noise = 1000,
                         ppm = 20,
                         mzdiff = 0.003,
                         prefilter = c(3,1000),
                         integrate = 1,
                         snthresh = 6)
  }

  #Loop to submit sample, injection and LMPeak information to DB
  for(i in 1:length(fileList)){

    filePath<-fileList[i]

    #Checking if file path adheres to naming strategy or not
    if(length(strsplit(filePath,"_")[[1]])!=6 || strsplit(filePath,"\\.")[[1]][2]!="mzML"){
      writeLines(paste("Filepath faulty, not performing checkLM of file:",filePath))
      next
    }

    #Checking if sample or sQC
    if(strsplit(filePath,"_")[[1]][5]=="sQC"){
      type<-"sQC"
    } else {
      type<-"sample"
    }

    #Setting up injection information to send to DB
    testInj$date[i]<-strsplit(filePath,"_")[[1]][1]
    testInj$batchWeek[i]<-strsplit(filePath,"_")[[1]][2]
    testInj$type[i]<-type
    testInj$seqInj[i]<-as.integer(strsplit((strsplit(filePath,"_")[[1]][6]),"\\.")[[1]][1])
    testInj$filePath[i]<-filePath
    testInj$MSMode[i]<-"MS1"
    testInj$instrument[i]<-instrument
    testInj$matrix[i]<-sampMatrix
    testInj$projName[i]<-projName
    # if(!is.null(sampNames)){
    #   testInj$sampName[i]<-sampNames[i]
    # } else {
      testInj$sampName[i]<-paste(strsplit(filePath,"_")[[1]][1],"_",strsplit(filePath,"_")[[1]][5],sep="")
    # }

    #Combining chrom and pol into one statement
    if(strsplit(filePath,"_")[[1]][3]=="RP" && strsplit(filePath,"_")[[1]][4]=="POS"){
      testInj$chromPol[i]<-"RP"
    } else if(strsplit(filePath,"_")[[1]][3]=="RP" && strsplit(filePath,"_")[[1]][4]=="NEG"){
      testInj$chromPol[i]<-"RN"
    } else if(strsplit(filePath,"_")[[1]][3]=="HILIC" && strsplit(filePath,"_")[[1]][4]=="POS"){
      testInj$chromPol[i]<-"HP"
    } else if(strsplit(filePath,"_")[[1]][3]=="HILIC" && strsplit(filePath,"_")[[1]][4]=="NEG"){
      testInj$chromPol[i]<-"HN"
    }

    #Performing peakpicking for each file and calculates IPO-score
    raw_data <- readMSData(files = paste(dir,"\\",filePath,sep="") , mode = "onDisk") # Read in file -> MS
    xdata_cwp <- findChromPeaks(raw_data, param = cwp) # Actual peak picking
    PeakInfo <- chromPeaks(xdata_cwp) %>% as.data.frame() # Extract picked peaks into data frame
    LM_IPO_score <- IPOscore(xdata_cwp, isotopeIdentification ="IPO")[5] # Calculate IPO score



    # Fetch landmark identifiers from DB for comparison
    landmarks <- fetchLM(dbName=dbName, chromPol=testInj$chromPol[i]) # Change to dynamic generation
    landmarks[,2]<-as.double(landmarks[,2])
    landmarks[,3]<-as.double(landmarks[,3])

    # Allocate object
    position_in_sample <-NULL # Position of landmark features in the sample peak list (from XCMS)
    position_in_LMDF <- NULL # Position of landmark features in the landmark reference DF

    #Loop through all landmarks to try and match to features in sample
    for (j in 1:nrow(landmarks)){

      # Landmark is found if < 5 ppm diff between landmark and sample m/z AND < 15 sec diff between RTs
      absDiff_ppm <- abs((landmarks$LMmz[j]-PeakInfo$mz)/(PeakInfo$mz[j])*1000000)
      absDiff_rt <- abs(landmarks$LMRT[j]-PeakInfo$rt)

      # Find potential matches with landmarks
      whichMatch <- which((absDiff_ppm < 5) & (absDiff_rt < 15))
      if (length(whichMatch) > 0) { # Is any feature within diff for both ppm and rt?

        #Collect position of landmarks in the sample and store them
        whichMatch <- whichMatch[(PeakInfo[whichMatch,]$rt - landmarks$LMRT[j]) %>% abs() %>% which.min()]
        position_in_sample <- c(position_in_sample, whichMatch)
        position_in_LMDF <- c(position_in_LMDF, j)
      }
    }

    # Setting up vectors for LM_Int and LM_RT from samples
    LM_Int <- rep(NA, nrow(landmarks))
    LM_rt  <- rep(NA, nrow(landmarks))
    if(any(is.na(landmarks$LMName))){
      whichNA<-is.na(landmarks$LMName)
      landmarks$LMName[whichNA]<-paste(landmarks$LMmz[whichNA], landmarks$LMRT[whichNA], landmarks$chromPol[whichNA])
    }
    names(LM_Int) <- landmarks$LMName

    # Allocate matrix for LM features (rows) and mz, rt & intensity (columns)
    LM_in_sample <- matrix(nrow = nrow(landmarks), ncol = 3)

    #Collect info from LMs found in sample, match up all data frames and input int & rt information
    LM_in_sample <-  PeakInfo[position_in_sample, c(1, 4, 7)]
    LM_in_sample[,4]<-landmarks$LMID[position_in_LMDF]
    rownames(LM_in_sample) <-  landmarks$LMName[position_in_LMDF]
    position_in_Int <-  match(rownames(LM_in_sample), names(LM_Int))
    LM_Int[position_in_Int] <-  LM_in_sample$into %>% log()
    LM_rt[position_in_Int] <-  LM_in_sample$rt
    LM_numbers <- nrow(LM_in_sample)

    colnames(LM_in_sample)<-c("mz","RT","int","LMID")
    testInj$LMtoSub[[i]] <- LM_in_sample

    #Adding IPO and LM_numbers into injection information
    testInj$IPOScore[i]<-LM_IPO_score
    testInj$nLMs[i]<-LM_numbers

    cat(paste("Peakpicking performed for", filePath, "...",testInj$nLMs[i]," LMs found, IPO score:",testInj$IPOScore[i]))
  }

  submitSampToDB(dbName=dbName, injToSub=testInj)
  submitInjToDB(dbName=dbName, injToSub=testInj)
  submitLMPeaksToDB(dbName=dbName, injToSub=testInj)

}
