#' optimizeFindLM - A function for optimization of finding new LaMas
#'
#' @param preFCPfilepath File path to a .rds file which contains an XCMS object which has been processed until, but not includining, gap-filling
#' @param split The character used in the XCMS object for splitting up mz & RT
#' @param minrttocheck Dead volume cut-off in beginning of chromatogram to disregard when looking for LaMas
#' @param Prefilterintensity Prefilter intensity settings used when peak picking with XCMS
#' @param mode ???
#'
#' @return Returning 10 sets of optimized LaMas and the final parameters

optimizeFindLM <- function(preFCPfilepath, split='@', minrttocheck=40, Prefilterintensity, mode, minLM=100, maxLM=200){
  PT_NoFill <- readRDS(preFCPfilepath)
  preFCPnames <- colnames(PT_NoFill)
  splitnames <- strsplit(preFCPnames, split=split)
  mzvec <- as.numeric(sapply(splitnames, "[[", 1))
  rtvec <- as.numeric(sapply(splitnames, "[[", 2))
  runTime <- max(rtvec)
  runTime <- round(runTime, digits=0)
  mzrtdf <- data.frame(mzvec,rtvec)
  minrttocheck <- minrttocheck
  minIntensity <- seq(from=Prefilterintensity, to=Prefilterintensity*5, length.out=10)
  allowedmissingness <- c(0.25, 0.20, 0.15, 0.1, 0.05, 0.01)
  mzdiff <- seq(from=0.1, to=1, by=0.1)
  rtdiff <- seq(from=runTime/36, to=runTime/18, length.out=8)
  Des <- expand.grid(minIntensity, allowedmissingness, mzdiff, rtdiff)
  colnames(Des) <- c("minIntensity", "allowedmissingness", "mzdiff", "rtdiff")
  foundLMs <- list()
  for (i in 1:nrow(Des)){
    foundLMs[[i]] <- findLandmarks(mode=mode, dat = PT_NoFill, mzrtdf = mzrtdf, mzdiff = Des$mzdiff[i], rtdiff = Des$rtdiff[i], minIntensity = Des$minIntensity[i], minrttocheck = minrttocheck, allowedmissingness=Des$allowedmissingness[i])
    incProgress(1/nrow(Des))
  }

  nLMvec <- sapply(foundLMs, FUN=nrow)
  whcorrectnLM <- which(nLMvec<maxLM & nLMvec>minLM)
  Coverage <- vector()
  Settings <- Des[whcorrectnLM,]
  for (i in 1:length(whcorrectnLM)){
    Settingcandidate <- foundLMs[[whcorrectnLM[i]]]
    Coverage[i] <- calcCoverage(Settingcandidate[,2], runTime)
  }
  Resmatr <- cbind(Settings, Coverage)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  Scaledmatr <- apply(Resmatr, 2, FUN=range01)
  Scaledmatr[,2] <- Scaledmatr[,2]*-1
  testobjfun <- apply(Scaledmatr, 1, sum)
  sortedscore  <- sort(testobjfun, decreasing=T)
  sortedscore <- as.numeric(names(sortedscore)[1:10])
  rank1 <- foundLMs[[sortedscore[1]]]
  rank1settings <- cbind(Des[sortedscore[1],], Coverage[sortedscore[1]])
  rank2 <- foundLMs[[sortedscore[2]]]
  rank2settings <- cbind(Des[sortedscore[2],], Coverage[sortedscore[2]])
  rank3 <- foundLMs[[sortedscore[3]]]
  rank3settings <- cbind(Des[sortedscore[3],], Coverage[sortedscore[3]])
  rank4 <- foundLMs[[sortedscore[4]]]
  rank4settings <- cbind(Des[sortedscore[4],], Coverage[sortedscore[4]])
  rank5 <- foundLMs[[sortedscore[5]]]
  rank5settings <- cbind(Des[sortedscore[5],], Coverage[sortedscore[5]])
  rank6 <- foundLMs[[sortedscore[6]]]
  rank6settings <- cbind(Des[sortedscore[6],], Coverage[sortedscore[6]])
  rank7 <- foundLMs[[sortedscore[7]]]
  rank7settings <- cbind(Des[sortedscore[7],], Coverage[sortedscore[7]])
  rank8 <- foundLMs[[sortedscore[8]]]
  rank8settings <- cbind(Des[sortedscore[8],], Coverage[sortedscore[8]])
  rank9 <- foundLMs[[sortedscore[9]]]
  rank9settings <- cbind(Des[sortedscore[9],], Coverage[sortedscore[9]])
  rank10 <- foundLMs[[sortedscore[10]]]
  rank10settings <- cbind(Des[sortedscore[10],], Coverage[sortedscore[10]])
  LMs <- list(rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8, rank9, rank10)
  Settings <- list(rank1settings, rank2settings, rank3settings, rank4settings, rank5settings, rank6settings, rank7settings, rank8settings, rank9settings, rank10settings)
  Out <- list(LMs, Settings, mode)
  return(Out)
}

makeInterval <- function(rts){
  rts <- round(rts, digits=0)
  interval <- seq(from=rts-20, to=rts+20, by=1)
}

calcCoverage <- function(rts, runTime){
  interval <- c(sapply(rts, FUN=makeInterval))
  interval <- interval[which(!interval>runTime)]
  interval <- interval[which(!interval<0)]
  coverage <- length(unique(interval))/(runTime+1)
}
