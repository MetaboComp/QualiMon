#' fetchLMFeatureTable - A function for collecting LM feature data from a DB file
#'
#' @param dbName Name of the DB to fetch from
#' @param chromPol The chromatography and ionization polarity of the files to fetch, expressed as :"RP", "RN", "HP" or "HN"
#' @param projName The projName of interest as it is stored in the DB file
#' @param sampType The type of injections of interest, "sQC" or "sample"
#'
#' @return Returning data frame of LaMa data from all samples fulfilling filters
#'
#' @export fetchLMFeatureTable

fetchLMFeatureTable <- function(dbName="NameOfDB.db", chromPol, projName, sampType){

  # Fetch landmark identifiers from DB to build int and RT tables
  landmarks <- fetchLM(dbName=dbName, chromPol=chromPol) # Change to dynamic generation
  landmarks[,2]<-as.double(landmarks[,2])
  landmarks[,3]<-as.double(landmarks[,3])

  #Setting up objects for collecting data to build tables
  reportObject<-list()
  toFetch<-list()

  #Checking if sQC or sample data to be collected
  if (sampType == "sQC"){

    toFetch$type<-"sQC"
    toFetch$chromPol<-chromPol
    toFetch$projName <- projName

    reportObject$LMPeaks <- fetchLMRefDF(dbName=dbName, toFetch=toFetch)
    reportObject$IPOnLM <- fetchIPOnLM(dbName=dbName, toFetch=toFetch)
  } else {

    toFetch$type<-"sample"
    toFetch$chromPol<-chromPol
    toFetch$projName <- projName

    reportObject$LMPeaks <- fetchLMRefDF(dbName=dbName, toFetch=toFetch)
    reportObject$IPOnLM <- fetchIPOnLM(dbName=dbName, toFetch=toFetch)
  }

  #Building matrices for RT and intensity from data fetched from DB on previous samples
  LM_Ref<-list()
  LM_Ref$intensity<-matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$RT<-matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$height <- matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$fwhm <- matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$tf <- matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$sn <- matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$noise <- matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$dataPoints <- matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$nLMMatch<-matrix(NA,nrow=length(unique(reportObject$LMPeaks$injID)),ncol=nrow(landmarks))
  LM_Ref$IPO<-as.double(reportObject$IPOnLM[,2])
  LM_Ref$n<-as.integer(reportObject$IPOnLM[,3])
  LM_Ref$nPeaks<-as.integer(reportObject$IPOnLM[,4])
  LM_Ref$TIC <- as.double(reportObject$IPOnLM[,5])
  # LM_Ref$noise <- as.integer(reportObject$IPOnLM[,6])
  LM_Ref$name<-as.character(reportObject$IPOnLM[,6])


  colnames(LM_Ref$intensity)<-landmarks$LMID
  rownames(LM_Ref$intensity)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$RT)<-landmarks$LMID
  rownames(LM_Ref$RT)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$height)<-landmarks$LMID
  rownames(LM_Ref$height)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$fwhm)<-landmarks$LMID
  rownames(LM_Ref$fwhm)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$tf)<-landmarks$LMID
  rownames(LM_Ref$tf)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$sn)<-landmarks$LMID
  rownames(LM_Ref$sn)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$noise)<-landmarks$LMID
  rownames(LM_Ref$noise)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$dataPoints)<-landmarks$LMID
  rownames(LM_Ref$dataPoints)<-unique(reportObject$LMPeaks$injID)
  colnames(LM_Ref$nLMMatch)<-landmarks$LMID
  rownames(LM_Ref$nLMMatch)<-unique(reportObject$LMPeaks$injID)

  #Filling in intensity and RT tables
  for(i in 1:length(LM_Ref$intensity[,1])){
    LM_Ref$intensity[i,]<-reportObject$LMPeaks$int[which(reportObject$LMPeaks$injID==rownames(LM_Ref$intensity)[i])]
    LM_Ref$RT[i,]<-reportObject$LMPeaks$RT[which(reportObject$LMPeaks$injID==rownames(LM_Ref$RT)[i])]
    LM_Ref$nLMMatch[i,]<-reportObject$LMPeaks$nLMMatch[which(reportObject$LMPeaks$injID==rownames(LM_Ref$nLMMatch)[i])]
    LM_Ref$height[i,]<-reportObject$LMPeaks$height[which(reportObject$LMPeaks$injID==rownames(LM_Ref$height)[i])]
    LM_Ref$fwhm[i,]<-reportObject$LMPeaks$fwhm[which(reportObject$LMPeaks$injID==rownames(LM_Ref$fwhm)[i])]
    LM_Ref$tf[i,]<-reportObject$LMPeaks$tf[which(reportObject$LMPeaks$injID==rownames(LM_Ref$tf)[i])]
    LM_Ref$sn[i,]<-reportObject$LMPeaks$sn[which(reportObject$LMPeaks$injID==rownames(LM_Ref$sn)[i])]
    LM_Ref$noise[i,]<-reportObject$LMPeaks$noise[which(reportObject$LMPeaks$injID==rownames(LM_Ref$noise)[i])]
    LM_Ref$dataPoints[i,]<-reportObject$LMPeaks$dataPoints[which(reportObject$LMPeaks$injID==rownames(LM_Ref$dataPoints)[i])]

  }

  return(LM_Ref)
}
