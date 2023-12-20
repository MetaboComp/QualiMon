#' check_LM - A function for determining whether an injection is an outlier
#'
#' @param filePath A filepath to the file to check
#' @param dbName Name of the DB to submit to
#' @param instrument Name of the type of instrument used for analysis (default is "QTOF)
#' @param projName If the user has supplied a projName it will be submitted to the DB
#' @param sampleMatrix If the user has supplied a sample matrix it will be submitted to the DB
#' @param dPPM The dPPM window for comparing peaks in sample against landmarks in DB
#' @param rtWin The RT window for comparing peaks in sample against landmarks in DB
#' @param alpha Specifies significance wanted for outlier detection
#' @param noCheck Part of sample names not to be compared to landmark database
#' @param cwp Option to specify custom centwave-parameters (see XCMS package). Otherwise uses default (View(check_LM))
#' @param Config Config object loaded using function readConfigFile
#' @param slackOn Boolean stating whether bad samples should be reported to slack or not
#'
#' @return Nothing

#The specific instrument could be logged if inputed by user!
checkLM <- function(filePath, dbName="NameOfDB.db", instrument="QTOF", projName="", sampleMatrix="", dPPM=10, rtWin=30, alpha = 0.01, noCheck=c("blank", "cond", "CP"), Config, slackOn=F, batch=F){

  #Extracting the file name from the filePath
  fileName<-basename(filePath)

  #Checking if file is already in DB, exiting if yes
  conn <- dbConnect(RSQLite::SQLite(),dbName)
  sqliteSetBusyHandler(conn, 10000)
  fileInDB <- dbGetQuery(conn, sprintf("SELECT name FROM injections i WHERE name='%s'",
                                       fileName))
  dbDisconnect(conn)
  if(dim(fileInDB)[1]!=0){
    return()
  }

  #Setting up injection information to send to DB
  testInj<-list()
  testInj$date<-strsplit(fileName,"_")[[1]][1]
  testInj$batchWeek<-strsplit(fileName,"_")[[1]][2]
  testInj$seqInj<-as.integer(strsplit((strsplit(fileName,"_")[[1]][6]),"\\.")[[1]][1])
  testInj$filePath<-fileName
  testInj$MSMode<-"MS1"
  testInj$instrument<-instrument
  testInj$projName<-projName
  testInj$matrix<-sampleMatrix
  # cwp<-NULL

  #Checking if a sample name has been submitted
  testInj$sampName<-paste(strsplit(fileName,"_")[[1]][1],"_",strsplit(fileName,"_")[[1]][5],sep="")

  #Combining chrom and pol into one statement
  if(grepl("RP", strsplit(fileName,"_")[[1]][3], ignore.case=T) && grepl("POS",strsplit(fileName,"_")[[1]][4], ignore.case=T)){
    testInj$chromPol<-"RP"
  } else if(grepl("RP", strsplit(fileName,"_")[[1]][3], ignore.case=T) && grepl("NEG",strsplit(fileName,"_")[[1]][4], ignore.case=T)){
    testInj$chromPol<-"RN"
  } else if(grepl("HILIC", strsplit(fileName,"_")[[1]][3], ignore.case=T) && grepl("POS",strsplit(fileName,"_")[[1]][4], ignore.case=T)){
    testInj$chromPol<-"HP"
  } else if(grepl("HILIC", strsplit(fileName,"_")[[1]][3], ignore.case=T) && grepl("NEG",strsplit(fileName,"_")[[1]][4], ignore.case=T)){
    testInj$chromPol<-"HN"
  }

  #Checking if sample or sQC
  if(sum(str_detect(fileName, regex(noCheck, ignore_case=T)))){
    cat(paste("Injection type on exclusion list. No data quality check for ", testInj$filePath,"\n",sep=""))
    return()
  }else if(length(grep("sQC",strsplit(fileName,"_")[[1]][5], ignore.case = T))>0){
    type<-"sQC"
    #type<-strsplit(fileName,"_")[[1]][5]
  } else {
    type<-"sample"
  }
  testInj$type<-type

  #Double checking that file is not already in DB to avoid errors
  # conn <- dbConnect(RSQLite::SQLite(),dbName)
  # s1 <- sprintf("SELECT * FROM injections WHERE injections.name = %s",
  #               fileName)
  # if(length(dbGetQuery(conn, s1)) > 0){
  #   return()
  # }

  #Extracting information from config-file
  ##setting up soft and hard limits depending on chromatography¨(in future) and polarity
  if(testInj$chromPol=="RP"){
    SoftLimnLMs <- Config$RPnLMSoftLim
    SoftLimnPeaks <- Config$RPnPeaksSoftLim
    SoftLimIPO <- Config$RPIPOSoftLim
    SoftLimIntProp <- Config$RPIntPropSofttLim
    SoftLimRtProp <- Config$RPRtPropSoftLim
    SoftLimTIC <- Config$RPTICSoftLim
    SoftLimNoiseProp <- Config$RPNoiseSoftLim
    SoftLimHeightProp <- Config$RPHeightSoftLim
    SoftLimFWHMProp <- Config$RPFWHMSoftLim
    SoftLimTFProp <- Config$RPTFSoftLim
    SoftLimSNProp <- Config$RPSNSoftLim
    SoftLimDataPointsProp <- Config$RPDataPointSoftLim
    HardLimnLMs <- Config$RPnLMHardLim
    HardLimnPeaks <- Config$RPnPeaksHardLim
    HardLimIPO <- Config$RPIPOHardLim
    HardLimIntProp <- Config$RPIntPropHardtLim
    HardLimRtProp <- Config$RPRtPropHardLim
    HardLimTIC <- Config$RPTICHardLim
    HardLimNoiseProp <- Config$RPNoiseHardLim
    HardLimHeightProp <- Config$RPHeightHardLim
    HardLimFWHMProp <- Config$RPFWHMHardLim
    HardLimTFProp <- Config$RPTFHardLim
    HardLimSNProp <- Config$RPSNHardLim
    HardLimDataPointsProp <- Config$RPDataPointHardLim
  } else if (testInj$chromPol=="RN"){
    SoftLimnLMs <- Config$RNnLMSoftLim
    SoftLimnPeaks <- Config$RNnPeaksSoftLim
    SoftLimIPO <- Config$RNIPOSoftLim
    SoftLimIntProp <- Config$RNIntPropSofttLim
    SoftLimRtProp <- Config$RNRtPropSoftLim
    SoftLimTIC <- Config$RNTICSoftLim
    SoftLimNoiseProp <- Config$RNNoiseSoftLim
    SoftLimHeightProp <- Config$RNHeightSoftLim
    SoftLimFWHMProp <- Config$RNFWHMSoftLim
    SoftLimTFProp <- Config$RNTFSoftLim
    SoftLimSNProp <- Config$RNSNSoftLim
    HardLimnLMs <- Config$RNnLMHardLim
    HardLimnPeaks <- Config$RNnPeaksHardLim
    HardLimIPO <- Config$RNIPOHardLim
    HardLimIntProp <- Config$RNIntPropHardtLim
    HardLimRtProp <- Config$RNRtPropHardLim
    HardLimTIC <- Config$RNTICHardLim
    HardLimNoiseProp <- Config$RNNoiseHardLim
    HardLimHeightProp <- Config$RNHeightHardLim
    HardLimFWHMProp <- Config$RNFWHMHardLim
    HardLimTFProp <- Config$RNTFHardLim
    HardLimSNProp <- Config$RNSNHardLim
  } else {
    print('Sample does not have any supported polarity, please make sure naming of file is correct')
    return()
  }

  # Default centwave-parameters for xcms
  if(is.null(Config$cwp_peakwidthL)) {
    cwp <- CentWaveParam(peakwidth = c(8, 50),# Peak picking parameters for XCMS using centwave
                         noise = 1000,
                         ppm = 20,
                         mzdiff = -0.003,
                         prefilter = c(3,1000),
                         integrate = 1,
                         snthresh = 10)

  } else {
    cwp <- CentWaveParam(peakwidth = c(Config$cwp_peakwidthL, Config$cwp_peakwidthR),# Peak picking parameters for XCMS using centwave
                         noise = Config$cwp_noise,
                         ppm = Config$cwp_ppm,
                         mzdiff = Config$cwp_mzdiff,
                         prefilter = c(Config$cwp_prefilterL, Config$cwp_prefilterR),
                         integrate = Config$cwp_integrate,
                         snthresh = Config$cwp_snthresh)
  }


  #######################################################
  #### Pick peaks using XCMS and calculate IPO score ####
  #### Ultimately stored in "PeakInfo"               ####
  #Trying to avoid XCMS bug
  #gc (reset = TRUE)
  if(!batch){
    # write.table("Running xcms & cpc on file...", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
  }

  #tryCatch to avoid whole app shutting down when one sample is somehow shitty / mzML conversion fails
  resultTryCatch <- tryCatch({
    invisible({
      raw_data <- readMSData(files = filePath, mode = "onDisk") # Read in file -> MS
      
      if(length(unique(raw_data@featureData@data$msLevel))==1 && unique(raw_data@featureData@data$msLevel)==2 && batch){
        # write.table("File only containing MS2 data, performing no quality check", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
        return(NA)
      }
      
      gc (reset = TRUE)
      xdata_cwp <- findChromPeaks(raw_data, param = cwp) # Actual peak picking
      gc (reset = TRUE)
      PeakInfo <- as.data.frame(chromPeaks(xdata_cwp)) # Extract picked peaks into data frame
      gc (reset = TRUE)
      LM_IPO_score <- IPOscore(xdata_cwp, isotopeIdentification ="IPO")[5] # Calculate IPO score
      LM_TIC <- round(log(sum(tic(xdata_cwp))),3)
      "No error"
    })
  }, error = function(err) {
    print(paste0("Couldn't read sample: ", fileName))
    print("properly. Skipping it and continuing file monitoring.")
    return(NA)
  })
  
  if(is.na(resultTryCatch)){
    return()
  }

  #Spec <- spectra(raw_data)
  #Noises=sapply(Spec, function (x) specNoise(spec = cbind(mz=x@mz, intensity=x@intensity))) #
  #LM_Noise <- mean(Noises)


  #################################################
  #### Match detected peaks with the database  ####

  # Fetch landmark identifiers from DB for comparison
  landmarks <- fetchLM(dbName=dbName, chromPol=testInj$chromPol) # Change to dynamic generation
  landmarks[,2]<-as.double(landmarks[,2])
  landmarks[,3]<-as.double(landmarks[,3])

  # Allocate object
  position_in_sample <-NULL # Position of landmark features in the sample peak list (from XCMS)
  position_in_LMDF <- NULL # Position of landmark features in the landmark reference DF
  LMCandidates <- c()

  #Loop through all landmarks to try and match to features in sample
  for (j in 1:nrow(landmarks)){

    # Landmark is found if < 5 ppm diff between landmark and sample m/z AND < 15 sec diff between RTs
    absDiff_ppm <- abs((landmarks$LMmz[j]-PeakInfo$mz)/(PeakInfo$mz)*1000000)
    absDiff_rt <- abs(landmarks$LMRT[j]-PeakInfo$rt)

    # Find potential matches with landmarks
    whichMatch <- which((absDiff_ppm < (dPPM/2)) & (absDiff_rt < (rtWin/2)))
    if (length(whichMatch) > 0) { # Is any feature within diff for both ppm and rt?
      LMCandidates <- c(LMCandidates,length(whichMatch))
      #Collect position of landmarks in the sample and store them
      whichMatch <- whichMatch[which.min(abs((PeakInfo[whichMatch,]$rt - landmarks$LMRT[j])))]
      position_in_sample <- c(position_in_sample, whichMatch)
      position_in_LMDF <- c(position_in_LMDF, j)
    }
  }

  #### If no LaMas found whatsoever, enter NA into all things? ####

  #################################
  #### CPC processing of peaks ####
  #Attempt to hard integrate peaks into CPC object

  # #Checking if no LMs were detected or not and picking which peaks to hard-integrate based on this
  # LMs_not_found <- which(!(c(1:length(landmarks$LMID)) %in% position_in_LMDF))
  #
  sink("NUL")
    cpcObj<-characterize_xcms_peaklist(xdata_cwp, param=cpcProcParam(sel_peaks = c(position_in_sample)))
    cptObj<-cpt(cpcObj)[,c(1,2,15,16,14,19,20,27,26)]
  sink()

  # cpc<-parsePeaklist(cpcObj)
  #
  # #Taking the last line of the CPC object and creating new lines
  # for(i in 1: length(LMs_not_found)){
  #   newline <- cpc@pt[nrow(cpc@pt), , drop = F]
  #   oldid <- sub(dimnames(newline)[[1]], pattern = "CP", replacement = "")
  #   newid <- as.numeric(oldid)+1
  #   newidC <- paste0("CP", newid)
  #   dimnames(newline)[[1]] <- newidC
  #   newline$id <- newid
  #   newline$mz <- landmarks$LMmz[LMs_not_found[i]]
  #   newline$rt <- landmarks$LMRT[LMs_not_found[i]]
  #   newline$sample <- 1
  #
  #   cpc@pt <- rbind(cpc@pt, newline)
  # }
  # setParam(cpc) <- list(sel_peaks = c(getParam(cpc, "sel_peaks"), nrow(cpc@pt)))


  
  cptObj<-cbind(cptObj,(cptObj$sn*2/cptObj$height), (cptObj$tpkb-cptObj$fpkb+1))
  cptObj<-cptObj[,-c(8,9)]
  names(cptObj)<-c("ID","RT","Int","Height","FWHM","TF","SN","Noise","DataPoints")

  # Setting up vectors for LM_Int and LM_RT from samples
  LM_Int <- rep(NA, nrow(landmarks))
  LM_rt  <- rep(NA, nrow(landmarks))
  if(any(is.na(landmarks$LMName))){
    whichNA<-is.na(landmarks$LMName)
    landmarks$LMName[whichNA]<-paste(landmarks$LMmz[whichNA], landmarks$LMRT[whichNA], landmarks$chromPol[whichNA])
  }
  names(LM_Int) <- landmarks$LMName

  # Allocate matrix for LM features (rows) and mz, rt & intensity (columns)
  LM_in_sample <- matrix(nrow = nrow(landmarks), ncol = 11)

  #Collect info from LMs found in sample, match up all data frames and input int & rt information
  for(i in 1:length(position_in_LMDF)){
    LM_in_sample[position_in_LMDF[i],1] <-  as.double(PeakInfo[position_in_sample[i], c(1)]) #mz
    LM_in_sample[position_in_LMDF[i],2] <- cptObj[i,2] #RT
    LM_in_sample[position_in_LMDF[i],3] <- cptObj[i,3] #Int
    LM_in_sample[position_in_LMDF[i],4] <- LMCandidates[i] #Number of candidates for LM in sample
    LM_in_sample[position_in_LMDF[i],5] <- as.double(landmarks$LMID[position_in_LMDF[i]]) #LMID from DB
    LM_in_sample[position_in_LMDF[i],(6)] <- cptObj[i,4] #Height
    LM_in_sample[position_in_LMDF[i],(7)] <- cptObj[i,5] #FWHM
    LM_in_sample[position_in_LMDF[i],(8)] <- cptObj[i,6] #TF
    LM_in_sample[position_in_LMDF[i],(9)] <- cptObj[i,7] #SN
    LM_in_sample[position_in_LMDF[i],(10)] <- cptObj[i,8] #Noise
    LM_in_sample[position_in_LMDF[i],(11)] <- cptObj[i,9] #DataPoints
  }

  LM_in_sample[,5]<-as.integer(landmarks$LMID)
  rownames(LM_in_sample) <-  landmarks$LMName
  LM_rt <-  as.double(LM_in_sample[,2])
  LM_Int <-  as.double(LM_in_sample[,3])
  LM_height <-LM_in_sample[,6]
  LM_fwhm <-LM_in_sample[,7]
  LM_tf <-LM_in_sample[,8]
  LM_sn <-LM_in_sample[,9]
  LM_noise <-LM_in_sample[,10]
  LM_dataPoints <-LM_in_sample[,11]
  LM_numbers <- length(position_in_LMDF)

  #############################################################
  ####Collecting DB information depending on injection type####
  #Checking if injection name corresponds to elements in the noCheck argument in which cases it exits the function
  #Otherwise fetches sQC or sample information from DB depending on sample name

  H0Object<-list()
  toFetch<-list()
  Status<-0 #For slack purposes

  if (testInj$type %in% "sQC"){
    toFetch$type<-"sQC"
    toFetch$batchWeek<-testInj$batchWeek
    toFetch$chromPol<-testInj$chromPol
    toFetch$projName<-projName
    toFetch$nSampsMonitor<-Config$nSampsMonitor
    H0Object$LMPeaks <- fetchLMRefDF(dbName=dbName, toFetch=toFetch)
    H0Object$IPOnLM <- fetchIPOnLM(dbName=dbName, toFetch=toFetch)
  } else {
    toFetch$type<-"sample"
    toFetch$batchWeek<-testInj$batchWeek
    toFetch$chromPol<-testInj$chromPol
    toFetch$projName<-projName
    toFetch$nSampsMonitor<-Config$nSampsMonitor
    H0Object$LMPeaks <- fetchLMRefDF(dbName=dbName, toFetch=toFetch)
    H0Object$IPOnLM <- fetchIPOnLM(dbName=dbName, toFetch=toFetch)
  }

  check_LM_info = data.frame(matrix(nrow = 1, ncol = 28))
  colnames(check_LM_info) = c("FilePath","lmIntOutliers","lmIntOutliers_p",
                              "lmRTOutliers", "lmRTOutliers_p",
                              "lmHeightOutliers", "lmHeightOutliers_p",
                              "lmFWHMOutliers", "lmFWHMOutliers_p",
                              "lmTFOutliers", "lmTFOutliers_p",
                              "lmSNOutliers", "lmSNOutliers_p",
                              "lmDPOutliers", "lmDPOutliers_p",
                              "lmN", "lmN_p",
                              "LM_IPO_score", "LM_IPO_score_p", "Peak_number",
                              "Peak_number_p", "TIC", "TIC_p", "lmNoise", "lmNoise_p", "status", "sampleNumber", "sampleIter")


  ###################################################################################
  #### Checking that there are enough samples /§ sQCs to perform the calculations ####

  if(dim(H0Object$IPOnLM)[1]>1 && dim(H0Object$LMPeaks)[1]>1){
    ##############################################
    #### Building a 2D DF of landmark results ####
    if(!batch){
      # write.table("Performing quality tests...", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
    }

    #Setting up LM_Ref
    LM_Ref<-list()
    LM_Ref$intensity<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))
    LM_Ref$RT<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))
    LM_Ref$height<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))
    LM_Ref$fwhm<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))
    LM_Ref$tf<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))
    LM_Ref$sn<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))
    LM_Ref$noise<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))
    LM_Ref$dataPoints<-matrix(NA,nrow=length(unique(H0Object$LMPeaks$injID)),ncol=nrow(landmarks))

    LM_Ref$IPO<-as.double(H0Object$IPOnLM[,2])
    LM_Ref$n<-as.integer(H0Object$IPOnLM[,3])
    LM_Ref$nPeaks<-as.integer(H0Object$IPOnLM[,4])
    #LM_Ref$injID<-as.integer(H0Object$IPOnLM[,1])
    LM_Ref$name<-as.character(H0Object$IPOnLM[,6])
    LM_Ref$TIC <- as.double(H0Object$IPOnLM[,5])

    #Naming all the rows and columns in matrices
    for(i in 1:8){
      colnames(LM_Ref[[i]])<-landmarks$LMID
      rownames(LM_Ref[[i]])<-unique(H0Object$LMPeaks$injID)
    }

    #Filling in intensity & RT tables
    for(i in 1:length(LM_Ref$intensity[,1])){
      LM_Ref$intensity[i,]<-as.double(H0Object$LMPeaks$int[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
      LM_Ref$RT[i,]<-as.double(H0Object$LMPeaks$RT[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
      LM_Ref$height[i,]<-as.double(H0Object$LMPeaks$height[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
      LM_Ref$fwhm[i,]<-as.double(H0Object$LMPeaks$fwhm[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
      LM_Ref$tf[i,]<-as.double(H0Object$LMPeaks$tf[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
      LM_Ref$sn[i,]<-as.double(H0Object$LMPeaks$sn[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
      LM_Ref$noise[i,]<-as.double(H0Object$LMPeaks$noise[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
      LM_Ref$dataPoints[i,]<-as.double(H0Object$LMPeaks$dataPoints[which(H0Object$LMPeaks$injID==rownames(LM_Ref$intensity)[i])])
    }

    #Adding the sample to LM_Ref to be able to calculate all samples again each time from the same DF
    names(LM_Int)<-LM_in_sample[,5]
    names(LM_rt)<-LM_in_sample[,5]
    names(LM_height)<-LM_in_sample[,5]
    names(LM_fwhm)<-LM_in_sample[,5]
    names(LM_tf)<-LM_in_sample[,5]
    names(LM_sn)<-LM_in_sample[,5]
    names(LM_noise)<-LM_in_sample[,5]
    names(LM_dataPoints)<-LM_in_sample[,5]

    LM_Ref$intensity<-rbind(LM_Ref$intensity,rep(NA,ncol(LM_Ref$intensity)))
    LM_Ref$RT<-rbind(LM_Ref$RT,rep(NA,ncol(LM_Ref$RT)))
    LM_Ref$height<-rbind(LM_Ref$height,rep(NA,ncol(LM_Ref$height)))
    LM_Ref$fwhm<-rbind(LM_Ref$fwhm,rep(NA,ncol(LM_Ref$fwhm)))
    LM_Ref$tf<-rbind(LM_Ref$tf,rep(NA,ncol(LM_Ref$tf)))
    LM_Ref$sn<-rbind(LM_Ref$sn,rep(NA,ncol(LM_Ref$sn)))
    LM_Ref$noise<-rbind(LM_Ref$noise,rep(NA,ncol(LM_Ref$noise)))
    LM_Ref$dataPoints<-rbind(LM_Ref$dataPoints,rep(NA,ncol(LM_Ref$dataPoints)))

    for(l in 1:length(LM_Int)){
      LM_Ref$intensity[nrow(LM_Ref$intensity),which(colnames(LM_Ref$intensity)==names(LM_Int)[l])]<-LM_Int[l]
      LM_Ref$RT[nrow(LM_Ref$RT),which(colnames(LM_Ref$RT)==names(LM_rt)[l])]<-LM_rt[l]
      LM_Ref$height[nrow(LM_Ref$height),which(colnames(LM_Ref$height)==names(LM_height)[l])]<-LM_height[l]
      LM_Ref$fwhm[nrow(LM_Ref$fwhm),which(colnames(LM_Ref$fwhm)==names(LM_fwhm)[l])]<-LM_fwhm[l]
      LM_Ref$tf[nrow(LM_Ref$tf),which(colnames(LM_Ref$tf)==names(LM_tf)[l])]<-LM_tf[l]
      LM_Ref$sn[nrow(LM_Ref$sn),which(colnames(LM_Ref$sn)==names(LM_sn)[l])]<-LM_sn[l]
      LM_Ref$noise[nrow(LM_Ref$noise),which(colnames(LM_Ref$noise)==names(LM_noise)[l])]<-LM_noise[l]
      LM_Ref$dataPoints[nrow(LM_Ref$dataPoints),which(colnames(LM_Ref$dataPoints)==names(LM_dataPoints)[l])]<-LM_dataPoints[l]
    }
    LM_Ref$IPO<-c(LM_Ref$IPO,LM_IPO_score)
    LM_Ref$n<-c(LM_Ref$n, LM_numbers)
    LM_Ref$nPeaks<-c(LM_Ref$nPeaks,nrow(PeakInfo))
    LM_Ref$name<-c(LM_Ref$name, testInj$filePath)
    LM_Ref$TIC <- c(LM_Ref$TIC, LM_TIC)

    #Log calculating all intensities to get normal distribution of values
    #LM_Ref$intensity <- apply(LM_Ref$intensity, 2, log)

    #############################################################################
    ###Loop for checking all previous samples every time a new sample is added###

    #Calculating number of dfs for reference data
    df_ref<-length(LM_Ref$IPO[-i])-1

    #Initializing z-crit values instead of testing within loop (saving in on computations)
    t_Crit2Tail<-qt(1-alpha/2,df_ref)
    t_Crit1Tail<-qt(1-alpha, df_ref)
    sqrtSamps<-sqrt(length(LM_Ref$IPO[-i])-1)
    # check_LM_info = data.frame(matrix(nrow = 1, ncol = 28))

    for(i in 1:(nrow(LM_Ref$intensity))){
      if(!batch){
        # write.table(paste0("Testing file ", LM_Ref$name[i],"..."), "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
      }

      #Setting up a new
      LM_Int <- LM_Ref$intensity[i,]
      LM_rt <-  LM_Ref$RT[i,]
      LM_height <-  LM_Ref$height[i,]
      LM_fwhm <-  LM_Ref$fwhm[i,]
      LM_tf <-  LM_Ref$tf[i,]
      LM_sn <-  LM_Ref$sn[i,]
      LM_noise <-  LM_Ref$noise[i,]
      LM_dataPoints <-  LM_Ref$dataPoints[i,]

      LM_numbers <- ncol(LM_Ref$intensity)-sum(is.na(LM_Ref$intensity[i,]))
      LM_IPO_score_ref<-LM_Ref$IPO[i]
      LM_nPeaks_ref<-LM_Ref$nPeaks[i]
      LM_TIC_ref <- LM_Ref$TIC[i]
      LM_noise_ref <- LM_Ref$noise[i]
      Status <- 0
      testsMade <- 0

      ################################################
      #### Extract actual data from REF landmarks ####

      ###### LM level tests ######

      #### For Int ####
      LM_Int_mean <- colMeans(LM_Ref$intensity[-i,], na.rm = TRUE)
      LM_Int_sd <- apply(LM_Ref$intensity[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_Int_Ref_t <- apply(LM_Ref$intensity[-i,], 1, function(x) abs((x - LM_Int_mean) / (LM_Int_sd)))
      LM_Int_Ref_outlier_n <- colSums(LM_Int_Ref_t > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_Int_Ref_outlier_n_div_nLM <- LM_Int_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:  #Number of outliers likely not normally distributed?
      LM_Int_z <-  abs(LM_Int-LM_Int_mean)/(LM_Int_sd) # Generate z-score for intensity
      LM_Int_outlier_n <- sum(LM_Int_z > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_Int_outlier_n_div_nLM <- LM_Int_outlier_n/LM_numbers
      LM_Int_outlier_t = (LM_Int_outlier_n_div_nLM - mean(LM_Int_Ref_outlier_n_div_nLM, na.rm=TRUE)) / (sd(LM_Int_Ref_outlier_n_div_nLM, na.rm=TRUE)) # calculate z-value for number of outliers
      LM_Int_outlier_p = (LM_Int_outlier_t>t_Crit1Tail) # calculate p-value for number of outliers (one-sided; only higher is relevant)

      if(is.na(LM_Int_outlier_p)){
        LM_Int_outlier_p<-F
      }
      #Checking if proportion of intensity outliers are higher than set limits
      if (Config$statusInt==T){
        testsMade = testsMade+1

        if(SoftLimIntProp!=0){
          testsMade = testsMade+3

          if (LM_Int_outlier_n_div_nLM > SoftLimIntProp || is.nan(LM_Int_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_Int_outlier_n_div_nLM > HardLimIntProp || is.nan(LM_Int_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }
      }

      #### For retention time ####
      LM_rt_mean <- colMeans(LM_Ref$RT[-i,], na.rm = TRUE)
      LM_rt_sd <- apply(LM_Ref$RT[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_rt_Ref_z <- apply(LM_Ref$RT[-i,], 1, function(x) (abs(x - LM_rt_mean) / (LM_rt_sd)))
      LM_rt_Ref_outlier_n <- colSums((LM_rt_Ref_z>t_Crit2Tail), na.rm = TRUE)
      LM_rt_Ref_outlier_n_div_nLM <- LM_rt_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:
      LM_rt_z <-  abs(LM_rt-LM_rt_mean)/(LM_rt_sd)
      LM_rt_p <-  (LM_rt_z>t_Crit2Tail)
      LM_rt_outlier_n <- sum(LM_rt_p, na.rm = TRUE)

      #Checking if the proportion of outliers in the refpop is significant compared to sample
      LM_rt_outlier_n_div_nLM <- LM_rt_outlier_n/LM_numbers
      LM_rt_outlier_z = (LM_rt_outlier_n_div_nLM - mean(LM_rt_Ref_outlier_n_div_nLM, na.rm=TRUE)) / sd(LM_rt_Ref_outlier_n_div_nLM, na.rm=TRUE)# calculate z-value for number of outliers
      LM_rt_outlier_p = (LM_rt_outlier_z>t_Crit1Tail)

      #If the proportion of outliers are zero LM_rt_outlier_p will become NaN which is problematic, so we replace it with 1
      if(is.na(LM_rt_outlier_p)==TRUE){
        LM_rt_outlier_p<-F
      }
      #Checking if proportion of retention time outliers are higher than set limits
      if (Config$statusRT==T){
        testsMade = testsMade+1

        if(SoftLimRtProp!=0){
          testsMade = testsMade+3

          if (LM_rt_outlier_n_div_nLM > SoftLimRtProp || is.nan(LM_rt_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_rt_outlier_n_div_nLM > HardLimRtProp || is.nan(LM_rt_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }


      }

      #### For height ####
      LM_Height_mean <- colMeans(LM_Ref$height[-i,], na.rm = TRUE)
      LM_Height_sd <- apply(LM_Ref$height[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_Height_Ref_t <- apply(LM_Ref$height[-i,], 1, function(x) abs((x - LM_Height_mean) / (LM_Height_sd)))
      LM_Height_Ref_outlier_n <- colSums(LM_Height_Ref_t > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_Height_Ref_outlier_n_div_nLM <- LM_Height_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:  #Number of outliers likely not normally distributed?
      LM_Height_z <-  abs(LM_height-LM_Height_mean)/(LM_Height_sd) # Generate z-score for intensity
      LM_Height_outlier_n <- sum(LM_Height_z > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_Height_outlier_n_div_nLM <- LM_Height_outlier_n/LM_numbers
      LM_Height_outlier_t = (LM_Height_outlier_n_div_nLM - mean(LM_Height_Ref_outlier_n_div_nLM, na.rm=TRUE)) / (sd(LM_Height_Ref_outlier_n_div_nLM, na.rm=TRUE)) # calculate z-value for number of outliers
      LM_Height_outlier_p = (LM_Height_outlier_t>t_Crit1Tail) # calculate p-value for number of outliers (one-sided; only higher is relevant)

      if(is.na(LM_Height_outlier_p)){
        LM_Height_outlier_p<-F
      }
      #Checking if proportion of intensity outliers are higher than set limits
      if (Config$statusHeight==T){
        testsMade = testsMade+1

        if(SoftLimHeightProp != 0){
          testsMade = testsMade+3

          if (LM_Height_outlier_n_div_nLM > SoftLimHeightProp || is.nan(LM_Height_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_Height_outlier_n_div_nLM > HardLimHeightProp || is.nan(LM_Height_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }


      }

      #### For fwhm ####
      LM_FWHM_mean <- colMeans(LM_Ref$fwhm[-i,], na.rm = TRUE)
      LM_FWHM_sd <- apply(LM_Ref$fwhm[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_FWHM_Ref_t <- apply(LM_Ref$fwhm[-i,], 1, function(x) abs((x - LM_FWHM_mean) / (LM_FWHM_sd)))
      LM_FWHM_Ref_outlier_n <- colSums(LM_FWHM_Ref_t > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_FWHM_Ref_outlier_n_div_nLM <- LM_FWHM_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:  #Number of outliers likely not normally distributed?
      LM_FWHM_z <-  abs(LM_fwhm-LM_FWHM_mean)/(LM_FWHM_sd) # Generate z-score for intensity
      LM_FWHM_outlier_n <- sum(LM_FWHM_z > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_FWHM_outlier_n_div_nLM <- LM_FWHM_outlier_n/LM_numbers
      LM_FWHM_outlier_t = (LM_FWHM_outlier_n_div_nLM - mean(LM_FWHM_Ref_outlier_n_div_nLM, na.rm=TRUE)) / (sd(LM_FWHM_Ref_outlier_n_div_nLM, na.rm=TRUE)) # calculate z-value for number of outliers
      LM_FWHM_outlier_p = (LM_FWHM_outlier_t>t_Crit1Tail) # calculate p-value for number of outliers (one-sided; only higher is relevant)

      if(is.na(LM_FWHM_outlier_p)){
        LM_FWHM_outlier_p<-F
      }
      #Checking if proportion of intensity outliers are higher than set limits
      if (Config$statusFWHM==T){
        testsMade = testsMade+1

        if(SoftLimFWHMProp!=0){
          testsMade = testsMade+3

          if (LM_FWHM_outlier_n_div_nLM > SoftLimFWHMProp || is.nan(LM_FWHM_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_FWHM_outlier_n_div_nLM > HardLimFWHMProp || is.nan(LM_FWHM_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }
      }

      #### For tf ####
      LM_TF_mean <- colMeans(LM_Ref$tf[-i,], na.rm = TRUE)
      LM_TF_sd <- apply(LM_Ref$tf[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_TF_Ref_t <- apply(LM_Ref$tf[-i,], 1, function(x) abs((x - LM_TF_mean) / (LM_TF_sd)))
      LM_TF_Ref_outlier_n <- colSums(LM_TF_Ref_t > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_TF_Ref_outlier_n_div_nLM <- LM_TF_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:  #Number of outliers likely not normally distributed?
      LM_TF_z <-  abs(LM_tf-LM_TF_mean)/(LM_TF_sd) # Generate z-score for intensity
      LM_TF_outlier_n <- sum(LM_TF_z > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_TF_outlier_n_div_nLM <- LM_TF_outlier_n/LM_numbers
      LM_TF_outlier_t = (LM_TF_outlier_n_div_nLM - mean(LM_TF_Ref_outlier_n_div_nLM, na.rm=TRUE)) / (sd(LM_TF_Ref_outlier_n_div_nLM, na.rm=TRUE)) # calculate z-value for number of outliers
      LM_TF_outlier_p = (LM_TF_outlier_t>t_Crit1Tail) # calculate p-value for number of outliers (one-sided; only higher is relevant)

      if(is.na(LM_TF_outlier_p)){
        LM_TF_outlier_p<-F
      }
      #Checking if proportion of intensity outliers are higher than set limits
      if (Config$statusTF==T){
        testsMade = testsMade+1

        if(SoftLimTFProp!=0){
          testsMade = testsMade+3

          if(LM_TF_outlier_n_div_nLM > SoftLimTFProp || is.nan(LM_TF_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_TF_outlier_n_div_nLM > HardLimTFProp || is.nan(LM_TF_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }
      }

      #### For sn ####
      LM_SN_mean <- colMeans(LM_Ref$sn[-i,], na.rm = TRUE)
      LM_SN_sd <- apply(LM_Ref$sn[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_SN_Ref_t <- apply(LM_Ref$sn[-i,], 1, function(x) abs((x - LM_SN_mean) / (LM_SN_sd)))
      LM_SN_Ref_outlier_n <- colSums(LM_SN_Ref_t > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_SN_Ref_outlier_n_div_nLM <- LM_SN_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:  #Number of outliers likely not normally distributed?
      LM_SN_z <-  abs(LM_sn-LM_SN_mean)/(LM_SN_sd) # Generate z-score for intensity
      LM_SN_outlier_n <- sum(LM_SN_z > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_SN_outlier_n_div_nLM <- LM_SN_outlier_n/LM_numbers
      LM_SN_outlier_t = (LM_SN_outlier_n_div_nLM - mean(LM_SN_Ref_outlier_n_div_nLM, na.rm=TRUE)) / (sd(LM_SN_Ref_outlier_n_div_nLM, na.rm=TRUE)) # calculate z-value for number of outliers
      LM_SN_outlier_p = (LM_SN_outlier_t>t_Crit1Tail) # calculate p-value for number of outliers (one-sided; only higher is relevant)

      if(is.na(LM_SN_outlier_p)){
        LM_SN_outlier_p<-F
      }
      #Checking if proportion of intensity outliers are higher than set limits
      if (Config$statusSN==T){
        testsMade = testsMade+1

        if(SoftLimSNProp!=0){
          testsMade = testsMade+3

          if (LM_SN_outlier_n_div_nLM > SoftLimSNProp || is.nan(LM_SN_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_SN_outlier_n_div_nLM > HardLimSNProp || is.nan(LM_SN_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }
      }

      #### For noise ####
      LM_NOISE_mean <- colMeans(LM_Ref$noise[-i,], na.rm = TRUE)
      LM_NOISE_sd <- apply(LM_Ref$noise[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_NOISE_Ref_t <- apply(LM_Ref$noise[-i,], 1, function(x) abs((x - LM_NOISE_mean) / (LM_NOISE_sd)))
      LM_NOISE_Ref_outlier_n <- colSums(LM_NOISE_Ref_t > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_NOISE_Ref_outlier_n_div_nLM <- LM_NOISE_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:  #Number of outliers likely not normally distributed?
      LM_NOISE_z <-  abs(LM_noise-LM_NOISE_mean)/(LM_NOISE_sd) # Generate z-score for intensity
      LM_NOISE_outlier_n <- sum(LM_NOISE_z > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_NOISE_outlier_n_div_nLM <- LM_NOISE_outlier_n/LM_numbers
      LM_NOISE_outlier_t = (LM_NOISE_outlier_n_div_nLM - mean(LM_NOISE_Ref_outlier_n_div_nLM, na.rm=TRUE)) / (sd(LM_NOISE_Ref_outlier_n_div_nLM, na.rm=TRUE)) # calculate z-value for number of outliers
      LM_NOISE_outlier_p = (LM_NOISE_outlier_t>t_Crit1Tail) # calculate p-value for number of outliers (one-sided; only higher is relevant)

      if(is.na(LM_NOISE_outlier_p)){
        LM_NOISE_outlier_p<-F
      }
      #Checking if proportion of intensity outliers are higher than set limits
      if (Config$statusNoise==T){
        testsMade = testsMade+1

        if(SoftLimNoiseProp!=0){
          testsMade = testsMade+3

          if (LM_NOISE_outlier_n_div_nLM > SoftLimNoiseProp || is.nan(LM_NOISE_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_NOISE_outlier_n_div_nLM > HardLimNoiseProp || is.nan(LM_NOISE_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }
      }

      #### For dataPoints ####
      LM_DataPoints_mean <- colMeans(LM_Ref$dataPoints[-i,], na.rm = TRUE)
      LM_DataPoints_sd <- apply(LM_Ref$dataPoints[-i,], 2, function(x) sd(x, na.rm = TRUE))

      # For reference population:
      LM_DataPoints_Ref_t <- apply(LM_Ref$dataPoints[-i,], 1, function(x) abs((x - LM_DataPoints_mean) / (LM_DataPoints_sd)))
      LM_DataPoints_Ref_outlier_n <- colSums(LM_DataPoints_Ref_t > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_DataPoints_Ref_outlier_n_div_nLM <- LM_DataPoints_Ref_outlier_n/LM_Ref$n[-i]
      # For test sample:  #Number of outliers likely not normally distributed?
      LM_DataPoints_z <-  abs(LM_dataPoints-LM_DataPoints_mean)/(LM_DataPoints_sd) # Generate z-score for intensity
      LM_DataPoints_outlier_n <- sum(LM_DataPoints_z > t_Crit2Tail, na.rm = TRUE) # calculate number of intensities "significantly" outside the range (p<alpha)
      LM_DataPoints_outlier_n_div_nLM <- LM_DataPoints_outlier_n/LM_numbers
      LM_DataPoints_outlier_t = (LM_DataPoints_outlier_n_div_nLM - mean(LM_DataPoints_Ref_outlier_n_div_nLM, na.rm=TRUE)) / (sd(LM_DataPoints_Ref_outlier_n_div_nLM, na.rm=TRUE)) # calculate z-value for number of outliers
      LM_DataPoints_outlier_p = (LM_DataPoints_outlier_t>t_Crit1Tail) # calculate p-value for number of outliers (one-sided; only higher is relevant)

      if(is.na(LM_DataPoints_outlier_p)){
        LM_DataPoints_outlier_p<-F
      }
      #Checking if proportion of intensity outliers are higher than set limits
      if (Config$statusDataPoints==T){
        testsMade = testsMade+1

        if(SoftLimIntProp!=0){
          testsMade = testsMade+3

          if(LM_DataPoints_outlier_n_div_nLM > SoftLimIntProp || is.nan(LM_DataPoints_outlier_n_div_nLM)){
            Status <- Status + 1

            if (LM_DataPoints_outlier_n_div_nLM > HardLimIntProp || is.nan(LM_DataPoints_outlier_n_div_nLM)){
              Status <- Status + 2
            }
          }
        }
      }


      ###### Sample level tests ######

      #### For number of landmarks ####
      LM_n_mean<-mean(as.integer(LM_Ref$n[-i]), na.rm=TRUE)
      LM_n_sd<-sd(as.integer(LM_Ref$n[-i]), na.rm=TRUE)
      LM_n_z = -((LM_numbers-LM_n_mean)/(LM_n_sd))
      LM_n_p = (LM_n_z>t_Crit1Tail)
      #Checking if proportion of retention time outliers are higher than set limits
      if (Config$statusnLM==T){
        testsMade = testsMade+1

        if(SoftLimnLMs!=0){
          testsMade = testsMade+3

          if (LM_numbers < SoftLimnLMs){
            Status <- Status + 1

            if (LM_numbers < HardLimnLMs){
              Status <- Status + 2
            }
          }
        }
      }


      #### For IPO score ####
      LM_IPO_mean<-mean(as.double(LM_Ref$IPO[-i]), na.rm=TRUE)
      LM_IPO_sd<-sd(as.double(LM_Ref$IPO[-i]), na.rm=TRUE)
      LM_IPO_z = abs((LM_IPO_score_ref - LM_IPO_mean)/(LM_IPO_sd))
      LM_IPO_p = (LM_IPO_z>t_Crit2Tail)
      #Checking if proportion of retention time outliers are higher than set limits
      if (Config$statusIPO==T){
        testsMade = testsMade+1

        if(SoftLimIPO!=0){
          testsMade = testsMade+3

          if(LM_IPO_score_ref < SoftLimIPO){
            Status <- Status + 1

            if (LM_IPO_score_ref < HardLimIPO){
              Status <- Status + 2
            }
          }
        }
      }

      #### For nPeaks ####
      LM_nPeaks_mean <- mean(as.double(LM_Ref$nPeaks[-i]), na.rm=TRUE)
      LM_nPeaks_sd <- sd(LM_Ref$nPeaks[-i], na.rm=TRUE)
      Peak_number_z = abs((LM_nPeaks_ref-LM_nPeaks_mean)/(LM_nPeaks_sd))
      Peak_number_p = (Peak_number_z>t_Crit2Tail)

      if (Config$statusNPeaks==T){
        testsMade = testsMade+1

        if(SoftLimnPeaks!=0){
          testsMade = testsMade+3

          if(LM_nPeaks_ref < SoftLimnPeaks){
            Status <- Status + 1

            if (LM_nPeaks_ref < HardLimnPeaks){
              Status <- Status + 2
            }
          }
        }
      }

      #### For TIC ####
      LM_TIC_mean<-mean(as.double(LM_Ref$TIC[-i]), na.rm=TRUE)
      LM_TIC_sd<-sd(as.double(LM_Ref$TIC[-i]), na.rm=TRUE)
      LM_TIC_z = abs((LM_TIC_ref - LM_TIC_mean)/(LM_TIC_sd))
      LM_TIC_p = (LM_TIC_z>t_Crit2Tail)
      #Checking if proportion of retention time outliers are higher than set limits
      if (Config$statusTIC==T){
        testsMade = testsMade+1

        if(SoftLimTIC!=0){
          testsMade = testsMade+3

          if(LM_TIC_ref < SoftLimTIC){
            Status <- Status + 1

            if (LM_TIC_ref < HardLimTIC){
              Status <- Status + 2
            }
          }
        }
      }

      #### Summing up all the statuses ####
      Status<-sum(Status,
                  ifelse(Config$statusInt==1, LM_Int_outlier_p, 0),
                  ifelse(Config$statusRT==1, LM_rt_outlier_p, 0),
                  ifelse(Config$statusHeight==1, LM_Height_outlier_p, 0),
                  ifelse(Config$statusFWHM==1, LM_FWHM_outlier_p, 0),
                  ifelse(Config$statusTF==1, LM_TF_outlier_p, 0),
                  ifelse(Config$statusSN==1, LM_SN_outlier_p, 0),
                  ifelse(Config$statusNoise==1, LM_NOISE_outlier_p, 0),
                  ifelse(Config$statusDataPoints==1, LM_DataPoints_outlier_p, 0),
                  ifelse(Config$statusnLM==1, LM_n_p, 0),
                  ifelse(Config$statusIPO==1, LM_IPO_p, 0),
                  ifelse(Config$statusNPeaks==1, Peak_number_p, 0),
                  ifelse(Config$statusTIC==1, LM_TIC_p, 0),
                  na.rm=TRUE) / testsMade

      ######################################################################
      ##### Report generation and abnormal/normal determination section ####
      check_LM_info[i,1] = LM_Ref$name[i]
      check_LM_info[i,2] = LM_Int_outlier_n
      check_LM_info[i,3] = LM_Int_outlier_p
      check_LM_info[i,4] = LM_rt_outlier_n
      check_LM_info[i,5] = LM_rt_outlier_p
      check_LM_info[i,6] = LM_Height_outlier_n
      check_LM_info[i,7] = LM_Height_outlier_p
      check_LM_info[i,8] = LM_FWHM_outlier_n
      check_LM_info[i,9] = LM_FWHM_outlier_p
      check_LM_info[i,10] = LM_TF_outlier_n
      check_LM_info[i,11] = LM_TF_outlier_p
      check_LM_info[i,12] = LM_SN_outlier_n
      check_LM_info[i,13] = LM_SN_outlier_p
      check_LM_info[i,14] = LM_DataPoints_outlier_n
      check_LM_info[i,15] = LM_DataPoints_outlier_p
      check_LM_info[i,16] = LM_numbers
      check_LM_info[i,17] = LM_n_p
      check_LM_info[i,18] = round(LM_Ref$IPO[i],0)
      check_LM_info[i,19] = LM_IPO_p
      check_LM_info[i,20] = LM_Ref$nPeaks[i] #Change to taking DB value
      check_LM_info[i,21] = Peak_number_p
      check_LM_info[i,22] = round(LM_Ref$TIC[i],3)
      check_LM_info[i,23] = LM_TIC_p
      check_LM_info[i,24] = LM_NOISE_outlier_n
      check_LM_info[i,25] = LM_NOISE_outlier_p
      check_LM_info[i,27] = i
      check_LM_info[i,28] = df_ref

      #Evaluating the results to determine if file is abnormal or normal
      if(Status <= Config$statusLim){
        # writeLines(paste(LM_Ref$name[i], "is likely normal"))
        check_LM_info[i,26] = Status

      } else {
        # writeLines(paste("Warning!!",LM_Ref$name[i]  ,"is likely abnormal!!"))
        check_LM_info[i,26] = Status
      }


      #### PUT SUBMIT LMQUALITY HERE ####
      # fwrite(check_LM_info, paste0(reportPath,reportName), append=TRUE, col.names=F, row.names=F, sep=";")
    }
    if(!batch){
      # write.table("Finished analysis", "data/status/status.txt", sep=";", row.names = FALSE, col.names = FALSE)
    }


    #Else statement referring back to whether there are enough samples in DB to even test sample
  } else {

    check_LM_info[1,1] = fileName
    check_LM_info[1,2] = -1
    check_LM_info[1,3] = -1
    check_LM_info[1,4] = -1
    check_LM_info[1,5] = -1
    check_LM_info[1,6] = -1
    check_LM_info[1,7] = -1
    check_LM_info[1,8] = -1
    check_LM_info[1,9] = -1
    check_LM_info[1,10] = -1
    check_LM_info[1,11] = -1
    check_LM_info[1,12] = -1
    check_LM_info[1,13] = -1
    check_LM_info[1,14] = -1
    check_LM_info[1,15] = -1
    check_LM_info[1,16] = LM_numbers
    check_LM_info[1,17] = -1
    check_LM_info[1,18] = round(LM_IPO_score,0)
    check_LM_info[1,19] = -1
    check_LM_info[1,20] = nrow(chromPeaks(xdata_cwp))
    check_LM_info[1,21] = -1
    check_LM_info[1,22] = LM_TIC
    check_LM_info[1,23] = -1
    check_LM_info[1,24] = -1
    check_LM_info[1,25] = -1
    check_LM_info[1,26] = -1
    check_LM_info[1,27] = -1
    check_LM_info[1,28] = -1

  }

  #Updating testInj with IPO_score & nLMs
  testInj$IPOScore<-round(LM_IPO_score,0)
  testInj$TIC <- round(LM_TIC, 3)
  testInj$nLMs<-LM_numbers
  colnames(LM_in_sample)<-c("mz","RT","int", "nLMMatch","LMID", "height", "fwhm", "tf", "sn", "noise","dataPoints")
  testInj$LMtoSub <- list()
  testInj$LMtoSub[[1]] <- as.data.frame(LM_in_sample)
  testInj$nPeaks <- nrow(chromPeaks(xdata_cwp))
  testInj$nSampsMonitor <- Config$nSampsMonitor

  ####################################
  ##Slack message for latest sample###
  if(slackOn==TRUE && Config$slackToken != "" && Config$slackChannelHard != ""){

    slackChannelHard<-Config$slackChannelHard
    slackChannelLog<-Config$slackChannelLog

    slackMSG<-paste("Sample: ", testInj$filePath,"\n",
                    "\t IntOutlier\t RTOutlier\t nLMs\t IPO\t nPeaks\n",
                    "Number\t",check_LM_info[nrow(check_LM_info),1],"\t",check_LM_info[nrow(check_LM_info),3],"\t",check_LM_info[nrow(check_LM_info),5],"\t",check_LM_info[nrow(check_LM_info),7],"\t",check_LM_info[nrow(check_LM_info),9],"\n",
                    "Sign.\t",check_LM_info[nrow(check_LM_info),2],"\t",check_LM_info[nrow(check_LM_info),4],"\t",check_LM_info[nrow(check_LM_info),6],"\t",check_LM_info[nrow(check_LM_info),8],"\t",check_LM_info[nrow(check_LM_info),10],"\n")
    slackr_msg(txt=slackMSG, channel=slackChannelLog, username="LM_Bot", token=Config$slackToken)


    ##Look over if status should be percent of total score-based
    ##Look over if should have other slackMSGs based on other parameters
    if(Status>Config$statusLim || testInj$nLMs < HardLimnLMs){
      slackMSG<-paste("Warning!!",testInj$filePath  ,"is likely abnormal!!")
      slackr_msg(txt=slackMSG, channel=slackChannelHard, username="LM_Bot", token=Config$slackToken)
    }
  }

  #Submitting sample, injection and LMPeak to DB
  submitSampToDB(dbName=dbName, injToSub=testInj)
  injAlreadyInDB <- submitInjToDB(dbName=dbName, injToSub=testInj)
  if(injAlreadyInDB == 0){
    submitLMPeaksToDB(dbName=dbName, injToSub=testInj)
  } #else {
    # print("Injection already in DB, no LM peak submission.")
  # }


  #### MAKE ALL CHECK_LM_INFO COLUMNS INTO NUMBER ####
  # colnames(check_LM_info) = c("FilePath","lmIntOutliers","lmIntOutliers_p",
  #                             "lmRTOutliers", "lmRTOutliers_p",
  #                             "lmHeightOutliers", "lmHeightOutliers_p",
  #                             "lmFWHMOutliers", "lmFWHMOutliers_p",
  #                             "lmTFOutliers", "lmTFOutliers_p",
  #                             "lmSNOutliers", "lmSNOutliers_p",
  #                             "lmDPOutliers", "lmDPOutliers_p",
  #                             "lmN", "lmN_p",
  #                             "LM_IPO_score", "LM_IPO_score_p", "Peak_number",
  #                             "Peak_number_p", "TIC", "TIC_p", "lmNoise", "lmNoise_p", "status", "sampleNumber", "sampleIter")
  #BROKE HERE
  check_LM_info[,c(2,4,6,8,10,12,14,16,18,20,27,28)] <- sapply(check_LM_info[,c(2,4,6,8,10,12,14,16,18,20,27,28)], as.integer) #as.data.frame(as.integer(check_LM_info[,c(2,4,6,8,10,12,14,16,18,20,26,27,28)]))
  # check_LM_info[,c(22)] <- sapply(check_LM_info[,c(22)], as.double)#as.data.frame(as.double(check_LM_info[,c(22,24)]))
  check_LM_info[check_LM_info=="TRUE"] <- "*"
  check_LM_info[check_LM_info=="FALSE"] <- ""


  submitLMQualityToDB(testInj=testInj, dbName=dbName, qualityTable=check_LM_info)
}
