#' readConfigFile - A function for reading config files
#'
#' @param configFilePath Filepath to config.txt file
#'
#' @return Config file object

readConfigFile <- function(configFilePath){
  txtobj <- readLines(configFilePath)
  dir <- txtobj[2]
  tmp_sink_dir <- txtobj[4]
  outdir <- txtobj[6]
  msconvert <- txtobj[8]
  dbName <- txtobj[10]
  noCheck <- unlist(strsplit(txtobj[12], split=" "))
  instrument <- txtobj[14]
  ltQCname<-as.character(txtobj[16])
  sampleMatrix <- txtobj[18]
  dPPM <- as.numeric(txtobj[20])
  rtWin <- as.numeric(txtobj[22])
  alpha <- as.numeric(txtobj[24])
  sleep.time <- as.numeric(txtobj[26])
  minFileSize<-as.double(txtobj[28])
  folderDepth<- as.character(txtobj[30])
  fileFormat<-as.character(txtobj[32])
  slackChannelHard<- as.character(txtobj[34])
  slackChannelLog<- as.character(txtobj[36])
  slackToken<-as.character(txtobj[38])

  #### Soft limits ####
  #RP Soft limits
  RPnLMSoftLim <- as.numeric(txtobj[41])
  RPnPeaksSoftLim <- as.numeric(txtobj[43])
  RPIPOSoftLim <- as.numeric(txtobj[45])
  RPIntPropSoftLim <- as.numeric(txtobj[47])
  RPRtPropSoftLim <- as.numeric(txtobj[49])
  RPTICSoftLim<-as.numeric(txtobj[51])
  RPNoiseSoftLim<-as.numeric(txtobj[53])
  RPHeightSoftLim<-as.numeric(txtobj[55])
  RPTFSoftLim<-as.numeric(txtobj[57])
  RPSNSoftLim<-as.numeric(txtobj[59])
  RPDataPointSoftLim<-as.numeric(txtobj[61])
  RPFWHMSoftLim<-as.numeric(txtobj[63])

  #RN Soft limits
  RNnLMSoftLim <- as.numeric(txtobj[66])
  RNnPeaksSoftLim <- as.numeric(txtobj[68])
  RNIPOSoftLim <- as.numeric(txtobj[70])
  RNIntPropSoftLim <- as.numeric(txtobj[72])
  RNRtPropSoftLim <- as.numeric(txtobj[74])
  RNTICSoftLim<-as.numeric(txtobj[76])
  RNNoiseSoftLim<-as.numeric(txtobj[78])
  RNHeightSoftLim<-as.numeric(txtobj[80])
  RNTFSoftLim<-as.numeric(txtobj[82])
  RNSNSoftLim<-as.numeric(txtobj[84])
  RNDataPointSoftLim<-as.numeric(txtobj[86])
  RNFWHMSoftLim<-as.numeric(txtobj[88])

  #HP Soft limits
  HPnLMSoftLim <- as.numeric(txtobj[91])
  HPnPeaksSoftLim <- as.numeric(txtobj[93])
  HPIPOSoftLim <- as.numeric(txtobj[95])
  HPIntPropSoftLim <- as.numeric(txtobj[97])
  HPRtPropSoftLim <- as.numeric(txtobj[99])
  HPTICSoftLim<-as.numeric(txtobj[101])
  HPNoiseSoftLim<-as.numeric(txtobj[103])
  HPHeightSoftLim<-as.numeric(txtobj[105])
  HPTFSoftLim<-as.numeric(txtobj[107])
  HPSNSoftLim<-as.numeric(txtobj[109])
  HPDataPointSoftLim<-as.numeric(txtobj[111])
  HPFWHMSoftLim<-as.numeric(txtobj[113])

  #HN Soft limits
  HNnLMSoftLim <- as.numeric(txtobj[116])
  HNnPeaksSoftLim <- as.numeric(txtobj[118])
  HNIPOSoftLim <- as.numeric(txtobj[120])
  HNIntPropSoftLim <- as.numeric(txtobj[122])
  HNRtPropSoftLim <- as.numeric(txtobj[124])
  HNTICSoftLim<-as.numeric(txtobj[126])
  HNNoiseSoftLim<-as.numeric(txtobj[128])
  HNHeightSoftLim<-as.numeric(txtobj[130])
  HNTFSoftLim<-as.numeric(txtobj[132])
  HNSNSoftLim<-as.numeric(txtobj[134])
  HNDataPointSoftLim<-as.numeric(txtobj[136])
  HNFWHMSoftLim<-as.numeric(txtobj[138])

  #### Hard limits ####
  #RP Hard limits
  RPnLMHardLim <- as.numeric(txtobj[141])
  RPnPeaksHardLim <- as.numeric(txtobj[143])
  RPIPOHardLim <- as.numeric(txtobj[145])
  RPIntPropHardLim <- as.numeric(txtobj[147])
  RPRtPropHardLim <- as.numeric(txtobj[149])
  RPTICHardLim<-as.numeric(txtobj[151])
  RPNoiseHardLim<-as.numeric(txtobj[153])
  RPHeightHardLim<-as.numeric(txtobj[155])
  RPTFHardLim<-as.numeric(txtobj[157])
  RPSNHardLim<-as.numeric(txtobj[159])
  RPDataPointHardLim<-as.numeric(txtobj[161])
  RPFWHMHardLim<-as.numeric(txtobj[163])

  #RN Hard limits
  RNnLMHardLim <- as.numeric(txtobj[166])
  RNnPeaksHardLim <- as.numeric(txtobj[168])
  RNIPOHardLim <- as.numeric(txtobj[170])
  RNIntPropHardLim <- as.numeric(txtobj[172])
  RNRtPropHardLim <- as.numeric(txtobj[174])
  RNTICHardLim<-as.numeric(txtobj[176])
  RNNoiseHardLim<-as.numeric(txtobj[178])
  RNHeightHardLim<-as.numeric(txtobj[180])
  RNTFHardLim<-as.numeric(txtobj[182])
  RNSNHardLim<-as.numeric(txtobj[184])
  RNDataPointHardLim<-as.numeric(txtobj[186])
  RNFWHMHardLim<-as.numeric(txtobj[188])

  #HP Hard limits
  HPnLMHardLim <- as.numeric(txtobj[191])
  HPnPeaksHardLim <- as.numeric(txtobj[193])
  HPIPOHardLim <- as.numeric(txtobj[195])
  HPIntPropHardLim <- as.numeric(txtobj[197])
  HPRtPropHardLim <- as.numeric(txtobj[199])
  HPTICHardLim<-as.numeric(txtobj[201])
  HPNoiseHardLim<-as.numeric(txtobj[203])
  HPHeightHardLim<-as.numeric(txtobj[205])
  HPTFHardLim<-as.numeric(txtobj[207])
  HPSNHardLim<-as.numeric(txtobj[209])
  HPDataPointHardLim<-as.numeric(txtobj[211])
  HPFWHMHardLim<-as.numeric(txtobj[213])

  #HN Hard limits
  HNnLMHardLim <- as.numeric(txtobj[216])
  HNnPeaksHardLim <- as.numeric(txtobj[218])
  HNIPOHardLim <- as.numeric(txtobj[220])
  HNIntPropHardLim <- as.numeric(txtobj[222])
  HNRtPropHardLim <- as.numeric(txtobj[224])
  HNTICHardLim<-as.numeric(txtobj[226])
  HNNoiseHardLim<-as.numeric(txtobj[228])
  HNHeightHardLim<-as.numeric(txtobj[230])
  HNTFHardLim<-as.numeric(txtobj[232])
  HNSNHardLim<-as.numeric(txtobj[234])
  HNDataPointHardLim<-as.numeric(txtobj[236])
  HNFWHMHardLim<-as.numeric(txtobj[238])

  #### Status ####
  statusInt<-as.integer(txtobj[240])
  statusRT<-as.integer(txtobj[242])
  statusHeight<-as.integer(txtobj[244])
  statusFWHM<-as.integer(txtobj[246])
  statusTF<-as.integer(txtobj[248])
  statusSN<-as.integer(txtobj[250])
  statusNoise<-as.integer(txtobj[252])
  statusDataPoints<-as.integer(txtobj[254])
  statusIPO<-as.integer(txtobj[256])
  statusNPeaks<-as.integer(txtobj[258])
  statusTIC<-as.integer(txtobj[260])
  statusnLM<-as.integer(txtobj[262])
  statusLim<-as.numeric(txtobj[264])

  nSampsMonitor<-as.numeric(txtobj[266])
  # doCheckProj<-as.character(txtobj[268])

  cwp_peakwidthL<-as.character(txtobj[268])
  cwp_peakwidthR<-as.character(txtobj[270])
  cwp_noise<-as.character(txtobj[272])
  cwp_ppm<-as.character(txtobj[274])
  cwp_mzdiff<-as.character(txtobj[276])
  cwp_prefilterL<-as.character(txtobj[278])
  cwp_prefilterR<-as.character(txtobj[280])
  cwp_integrate<-as.character(txtobj[282])
  cwp_snthresh<-as.character(txtobj[284])


  config <- list("dir" = dir,
                 "tmp_sink_dir" = tmp_sink_dir ,
                 "outdir" = outdir,
                 "msconvert" = msconvert,
                 "dbName" = dbName,
                 "noCheck" = noCheck,
                 # "cwp" = cwp,
                 # "projName" = projName,
                 "instrument" = instrument,
                 "ltQCname"=ltQCname,
                 "sampleMatrix" = sampleMatrix,
                 "dPPM" = dPPM,
                 "rtWin" = rtWin,
                 "alpha" = alpha,
                 "sleep.time" = sleep.time,
                 "minFileSize"=minFileSize,
                 "folderDepth"=folderDepth,
                 "fileFormat"=fileFormat,
                 "slackChannelHard"=slackChannelHard,
                 "slackChannelLog"=slackChannelLog,
                 "slackToken"=slackToken,


                 #Soft limits
                 #RP soft limits
                 "RPnLMSoftLim" = RPnLMSoftLim,
                 "RPnPeaksSoftLim" = RPnPeaksSoftLim,
                 "RPIPOSoftLim" = RPIPOSoftLim,
                 "RPIntPropSofttLim" = RPIntPropSoftLim,
                 "RPRtPropSoftLim"= RPRtPropSoftLim,
                 "RPHeightSoftLim"=RPHeightSoftLim,
                 "RPTFSoftLim"=RPTFSoftLim,
                 "RPSNSoftLim"=RPSNSoftLim,
                 "RPDataPointSoftLim"=RPDataPointSoftLim,
                 "RPFWHMSoftLim"=RPFWHMSoftLim,
                 "RPTICSoftLim"=RPTICSoftLim,
                 "RPNoiseSoftLim"=RPNoiseSoftLim,

                 #RN soft limits
                 "RNnLMSoftLim" = RNnLMSoftLim,
                 "RNnPeaksSoftLim" = RNnPeaksSoftLim,
                 "RNIPOSoftLim" =RNIPOSoftLim,
                 "RNIntPropSofttLim" = RNIntPropSoftLim,
                 "RNRtPropSoftLim"= RNRtPropSoftLim,
                 "RNHeightSoftLim"=RNHeightSoftLim,
                 "RNTFSoftLim"=RNTFSoftLim,
                 "RNSNSoftLim"=RNSNSoftLim,
                 "RNDataPointSoftLim"=RNDataPointSoftLim,
                 "RNFWHMSoftLim"=RNFWHMSoftLim,
                 "RNTICSoftLim"=RNTICSoftLim,
                 "RNNoiseSoftLim"=RNNoiseSoftLim,

                 #HP soft limits
                 "HPnLMSoftLim" = HPnLMSoftLim,
                 "HPnPeaksSoftLim" = HPnPeaksSoftLim,
                 "HPIPOSoftLim" = HPIPOSoftLim,
                 "HPIntPropSofttLim" = HPIntPropSoftLim,
                 "HPRtPropSoftLim"= HPRtPropSoftLim,
                 "HPHeightSoftLim"=HPHeightSoftLim,
                 "HPTFSoftLim"=HPTFSoftLim,
                 "HPSNSoftLim"=HPSNSoftLim,
                 "HPDataPointSoftLim"=HPDataPointSoftLim,
                 "HPFWHMSoftLim"=HPFWHMSoftLim,
                 "HPTICSoftLim"=HPTICSoftLim,
                 "HPNoiseSoftLim"=HPNoiseSoftLim,

                 #HN soft limits
                 "HNnLMSoftLim" = HNnLMSoftLim,
                 "HNnPeaksSoftLim" = HNnPeaksSoftLim,
                 "HNIPOSoftLim" =HNIPOSoftLim,
                 "HNIntPropSofttLim" = HNIntPropSoftLim,
                 "HNRtPropSoftLim"= HNRtPropSoftLim,
                 "HNHeightSoftLim"=HNHeightSoftLim,
                 "HNTFSoftLim"=HNTFSoftLim,
                 "HNSNSoftLim"=HNSNSoftLim,
                 "HNDataPointSoftLim"=HNDataPointSoftLim,
                 "HNFWHMSoftLim"=HNFWHMSoftLim,
                 "HNTICSoftLim"=HNTICSoftLim,
                 "HNNoiseSoftLim"=HNNoiseSoftLim,


                 #Hard limits
                 #RP hard limits
                 "RPnLMHardLim" = RPnLMHardLim,
                 "RPnPeaksHardLim" = RPnPeaksHardLim,
                 'RPIPOHardLim' = RPIPOHardLim,
                 "RPIntPropHardtLim" = RPIntPropHardLim,
                 "RPRtPropHardLim"= RPRtPropHardLim,
                 "RPHeightHardLim"=RPHeightHardLim,
                 "RPTFHardLim"=RPTFHardLim,
                 "RPSNHardLim"=RPSNHardLim,
                 "RPDataPointHardLim"=RPDataPointHardLim,
                 "RPFWHMHardLim"=RPFWHMHardLim,
                 "RPTICHardLim"=RPTICHardLim,
                 "RPNoiseHardLim"=RPNoiseHardLim,

                 #RN hard limits
                 "RNnLMHardLim" = RNnLMHardLim,
                 "RNnPeaksHardLim" = RNnPeaksHardLim,
                 'RNIPOHardLim' = RNIPOHardLim,
                 "RNIntPropHardtLim" = RNIntPropHardLim,
                 "RNRtPropHardLim"= RNRtPropHardLim,
                 "RNHeightHardLim"=RNHeightHardLim,
                 "RNTFHardLim"=RNTFHardLim,
                 "RNSNHardLim"=RNSNHardLim,
                 "RNDataPointHardLim"=RNDataPointHardLim,
                 "RNFWHMHardLim"=RNFWHMHardLim,
                 "RNTICHardLim"=RNTICHardLim,
                 "RNNoiseHardLim"=RNNoiseHardLim,

                 #HP hard limits
                 "HPnLMHardLim" = HPnLMHardLim,
                 "HPnPeaksHardLim" = HPnPeaksHardLim,
                 'HPIPOHardLim' = HPIPOHardLim,
                 "HPIntPropHardtLim" = HPIntPropHardLim,
                 "HPRtPropHardLim"= HPRtPropHardLim,
                 "HPHeightHardLim"=HPHeightHardLim,
                 "HPTFHardLim"=HPTFHardLim,
                 "HPSNHardLim"=HPSNHardLim,
                 "HPDataPointHardLim"=HPDataPointHardLim,
                 "HPFWHMHardLim"=HPFWHMHardLim,
                 "HPTICHardLim"=HPTICHardLim,
                 "HPNoiseHardLim"=HPNoiseHardLim,

                 #HN hard limits
                 "HNnLMHardLim" = HNnLMHardLim,
                 "HNnPeaksHardLim" = HNnPeaksHardLim,
                 'HNIPOHardLim' = HNIPOHardLim,
                 "HNIntPropHardtLim" = HNIntPropHardLim,
                 "HNRtPropHardLim"= HNRtPropHardLim,
                 "HNHeightHardLim"=HNHeightHardLim,
                 "HNTFHardLim"=HNTFHardLim,
                 "HNSNHardLim"=HNSNHardLim,
                 "HNDataPointHardLim"=HNDataPointHardLim,
                 "HNFWHMHardLim"=HNFWHMHardLim,
                 "HNTICHardLim"=HNTICHardLim,
                 "HNNoiseHardLim"=HNNoiseHardLim,


                 #Status on/off
                 "statusnLM" = statusnLM,
                 "statusInt"=statusInt,
                 "statusRT"=statusRT,
                 "statusHeight"=statusHeight,
                 "statusFWHM"=statusFWHM,
                 "statusTF"=statusTF,
                 "statusSN"=statusSN,
                 "statusNoise"=statusNoise,
                 "statusDataPoints"=statusDataPoints,
                 "statusIPO"=statusIPO,
                 "statusNPeaks"=statusNPeaks,
                 "statusTIC"=statusTIC,
                 "statusLim"=statusLim,

                 #nSamps to monitor
                 "nSampsMonitor"=nSampsMonitor,
                 # "doCheckProj"=doCheckProj,

                 #cwp parameters
                 "cwp_peakwidthL"=cwp_peakwidthL,
                 "cwp_peakwidthR"=cwp_peakwidthR,
                 "cwp_noise"=cwp_noise,
                 "cwp_ppm"=cwp_ppm,
                 "cwp_mzdiff"=cwp_mzdiff,
                 "cwp_prefilterL"=cwp_prefilterL,
                 "cwp_prefilterR"=cwp_prefilterR,
                 "cwp_integrate"=cwp_integrate,
                 "cwp_snthresh"=cwp_snthresh
               )

  return(config)
}
