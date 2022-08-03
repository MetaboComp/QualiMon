#' submitLMPeaksToDB - A function for submitting injections to DB
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function buildDB
#' @param injToSub A df with 9 columns: sampID, date, batchWeek, seqInj, chromPol, instrument, msMode, IPOscore and nLMs
#'
#' @return Nothing

submitLMPeaksToDB<-function(dbName="Test.db", injToSub){

  lmPeaks<-data.frame(peakIDLM=integer(),
                      injID=integer(),
                      mz=double(),
                      RT=double(),
                      int=double(),
                      nLMMatch=integer(),
                      LMID=integer(),
                      height=double(),
                      fwhm=double(),
                      tf=double(),
                      sn=double(),
                      noise=double(),
                      dataPoints=integer())

  conn <- dbConnect(RSQLite::SQLite(),dbName)
  lmPeaksInd<-as.integer(dbGetQuery(conn,"SELECT COUNT(*) FROM lmPeaks")$'COUNT(*)')

  n<-1

  #Loop which is putting all information from injToSub in the temp injections df for submission
  for(i in 1:length(injToSub$filePath)){


    s1<-sprintf("SELECT injID FROM injections i WHERE i.name='%s'",injToSub$filePath[i])
    injID<-dbGetQuery(conn, s1)

    for(l in 1:length(injToSub$LMtoSub[[i]][,1])){
      currPeakList<-injToSub$LMtoSub[[i]]
      lmPeaksInd<-lmPeaksInd+1
      lmPeaks[n,]<-c(lmPeaksInd, injID, currPeakList[l,]) #currPeakList[l,1], currPeakList[l,2], currPeakList[l,3], currPeakList[l,4], currPeakList[l,5], currPeakList[l,6], currPeakList[l,7]
      n<-n+1
    }
  }

  dbWriteTable(conn, "lmPeaks", lmPeaks, append=TRUE)
  dbDisconnect(conn)

  writeLines(paste("Submission of new lmPeaks successful!\n"))
}
