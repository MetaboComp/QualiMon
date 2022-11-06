#' submitSampToDB - A function for submitting samples to DB
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"
#' @param injToSub A df with 9 columns: sampID, date, batchWeek, seqInj, chromPol, instrument, msMode, IPOscore and nLMs
#'
#' @return Nothing

submitSampToDB<-function(dbName="NameOfDB.db", injToSub){

  samples<-data.frame(sampID=integer(),
                      projName=integer(),
                      type=character(),
                      matrix=character(),
                      name=character())

  conn <- dbConnect(RSQLite::SQLite(),dbName)
  sqliteSetBusyHandler(conn, 40000)
  samplesInd<-dbGetQuery(conn,"SELECT COUNT(*) FROM samples")$'COUNT(*)'



  for(i in 1:length(injToSub$filePath)){
    #Checking if sample already submitted to DB
    s1<-sprintf("SELECT * FROM samples s WHERE s.projName='%s' AND s.type='%s' AND s.matrix='%s' AND s.name='%s'",
                injToSub$projName[i],injToSub$type[i],injToSub$matrix[i],injToSub$sampName[i])
    sampleSubmitted<-dbGetQuery(conn,s1)
    if(dim(sampleSubmitted)[1]!=0){
      next
    }



    if(injToSub$type[i]=="sQC" && any(samples$type=="sQC")){
    } else {
      samplesInd<-samplesInd+1
      samples[i,]<-c(samplesInd, injToSub$projName[i], injToSub$type[i], injToSub$matrix[i], injToSub$sampName[i])
    }
  }
  dbDisconnect(conn)

  if(dim(samples)[1]!=0){
    conn <- dbConnect(RSQLite::SQLite(),dbName)
    dbWriteTable(conn, "samples", samples, append=TRUE)
    dbDisconnect(conn)

    writeLines(paste("Submission of samples successful!\n"))
  } else {
    writeLines(paste("Sample already present in DB"))
  }
}
