#' submitLMQualityToDB - A function for submitting iteration data for newly run samples
#'
#' @param testInj A df with 9 columns: sampID, date, batchWeek, seqInj, chromPol, instrument, msMode, IPOscore and nLMs
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function buildDB
#' @param qualityTable The table with the latest iteration of quality checks to be submitted to DB
#'
#' @return Nothing

submitLMQualityToDB <- function(testInj, dbName, qualityTable){

  fetchQuery <- sprintf("SELECT injID FROM injections WHERE injections.name IN (%s)",
                   paste("'",as.character(qualityTable$FilePath),"'",collapse=", ",sep=""))

  createViewQuery <- sprintf("CREATE VIEW IF NOT EXISTS %s AS SELECT injections.name, injections.chromPol, injections.batchWeek, samples.matrix, samples.projName, samples.type, lmQuality.* FROM lmQuality, samples, injections WHERE injections.injID=lmQuality.injID AND samples.sampID=injections.sampID", #ON injections.injID=lmQuality.injID WHERE samples.sampID=injections.sampID
                             paste0("QTable_",testInj$matrix))

  #distinct(inner_join(x=sampleLevelDT, y=visDataPermDT[,.(sampleNumber, batchWeek, name)], by="name"), name, nLMs, nPeaks, TIC, IPOscore, sampleNumber, batchWeek)
  createViewQuery2 <- sprintf("CREATE VIEW IF NOT EXISTS %s AS SELECT DISTINCT injections.name, injections.nLMs, injections.nPeaks, injections.TIC, injections.IPOscore, lmQuality.sampleNumber, injections.batchWeek, injections.chromPol, samples.projName, samples.type FROM lmQuality, samples, injections WHERE injections.injID=lmQuality.injID AND samples.sampID=injections.sampID", #ON injections.injID=lmQuality.injID WHERE samples.sampID=injections.sampID
                             paste0("QTableDistinct_",testInj$matrix))

  sampleNumberQuery <- sprintf("SELECT MAX(q.sampleNumber) FROM [%s] q WHERE q.chromPol='%s' AND q.type='%s'",
                              paste0("QTable_",testInj$matrix),
                              testInj$chromPol,
                              testInj$type)

  sampleIterQuery <- sprintf("SELECT MAX(q.sampleIter) FROM [%s] q WHERE q.chromPol='%s' AND q.type='%s'",
                            paste0("QTable_",testInj$matrix),
                            testInj$chromPol,
                            testInj$type)

  conn <- dbConnect(RSQLite::SQLite(),dbName)
  sqliteSetBusyHandler(conn, 40000)
  injIDs <- as.vector(unlist(dbGetQuery(conn, fetchQuery)))
  dbExecute(conn, createViewQuery)
  dbExecute(conn, createViewQuery2)
  sampNumb <- as.integer(dbGetQuery(conn, sampleNumberQuery))
  sampIter <- as.integer(dbGetQuery(conn, sampleIterQuery))

  if(is.na(sampNumb) || is.na(sampIter)){
    sampNumb=1
    sampIter=0
  } else {
    #SampIter taking first two solo samples of a matrix into account
    if(sampIter==0 && sampNumb < 2){
      sampIter==0
    } else {
      sampIter=sampIter+1
    }

    sampNumb = sampNumb+1
  }

  qualityTable <- cbind(injIDs, qualityTable[,-c(1, 16:23)]) #Removing sample specific columns and filepath
  colnames(qualityTable)[1] <- c("injID")
  #SampNumb taking first two solo samples of a matrix into account
  if(sampNumb < 3){
    qualityTable$sampleNumber <- sampNumb
  } else if(sampNumb==3){
    qualityTable$sampleNumber <- c(1:3)
  } else {
    if(sampNumb-(testInj$nSampsMonitor-1) <= 0){

    } else {
      qualityTable$sampleNumber <- c((sampNumb-(testInj$nSampsMonitor-1)):sampNumb)
    }
  }
  qualityTable$sampleIter <- rep(as.integer(sampIter), nrow(qualityTable))

  dbWriteTable(conn, "lmQuality", qualityTable, append=TRUE)

  dbDisconnect(conn)
}
