#' submitInjToDB - A function for submitting injections to DB
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"
#' @param injToSub A df with 9 columns: sampID, date, batchWeek, seqInj, chromPol, instrument, msMode, IPOscore and nLMs
#'
#' @return Returning status of whether injection already was in DB or not


submitInjToDB<-function(dbName="NameOfDB.db", injToSub){

  #Temp injections table to fill in for submission
  injections<-data.frame(injID=integer(),
                         sampID=integer(),
                         date=character(),
                         batchWeek=character(),
                         seqInj=integer(),
                         chromPol=character(),
                         instrument=character(),
                         MSMode=character(),
                         IPOscore=double(),
                         nLMs=integer(),
                         nPeaks=integer(),
                         TIC=integer(),
                         name=character())

  conn <- dbConnect(RSQLite::SQLite(),dbName)
  sqliteSetBusyHandler(conn, 10000)
  injIDInd<-as.integer(dbGetQuery(conn,"SELECT COUNT(*) FROM injections")$'COUNT(*)')

  #Loop which is putting all information from injToSub in the temp injections df for submission
  for(i in 1:length(injToSub$filePath)){
    #Checking if injection already submitted to DB
    s1<-sprintf("SELECT * FROM injections i, samples s WHERE i.date='%s' AND i.batchWeek='%s' AND i.seqInj='%s' AND i.chromPol='%s' AND i.instrument='%s' AND i.MSMode='%s' AND i.name='%s' AND s.sampID=i.sampID AND s.projName='%s'",
                injToSub$date[i],
                injToSub$batchWeek[i],
                injToSub$seqInj[i],
                injToSub$chromPol[i],
                injToSub$instrument[i],
                injToSub$MSMode[i],
                injToSub$filePath[i],
                injToSub$projName[i])
    injectionSubmitted<-dbGetQuery(conn,s1)
    if(dim(injectionSubmitted)[1]!=0){
      next
    }

    s2<-sprintf("SELECT * from samples s WHERE s.name='%s'",
                paste(strsplit(injToSub$filePath[i],"_")[[1]][1],"_",strsplit(injToSub$filePath[i],"_")[[1]][5],sep=""))
    sampleSubmitted<-dbGetQuery(conn,s2)
    if(dim(sampleSubmitted)[1]!=0){
      sampID<-sampleSubmitted$sampID
    } else {
      sampID<-0
    }

    injIDInd<-injIDInd+1

    #NEEDS TO BE CHANGED TO PICK THE CORRECT SAMPLE TO CONNECT THE INJECTION TO



    injections[i,]<-c(injIDInd, sampID, injToSub$date[i], injToSub$batchWeek[i], injToSub$seqInj[i], injToSub$chromPol[i], injToSub$instrument[i], injToSub$MSMode[i],injToSub$IPOScore[i],injToSub$nLMs[i], injToSub$nPeaks[i], injToSub$TIC[i], injToSub$filePath[i])
  }

  dbDisconnect(conn)

  if(dim(injections)[1]!=0){
    conn <- dbConnect(RSQLite::SQLite(),dbName)
    dbWriteTable(conn, "injections", injections, append=TRUE)
    dbDisconnect(conn)

    writeLines(paste("Submission of injections successful!\n"))

    return(0)
  } else {
    writeLines(paste("Injection already present in DB"))
    return(1)
  }
}
