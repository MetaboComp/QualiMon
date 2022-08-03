#' submitLMToDB - A function for fetching injection data from DB
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function buildDB
#' @param injToSub A df with 9 columns: sampID, date, batchWeek, seqInj, chromPol, instrument, msMode, IPOscore and nLMs
#'
#' @return Nothing

submitLMToDB <- function(dbName="NameOfDB.db", injToSub){

  #Temp landmarks table to fill in for submission
  landmarks<-data.frame(LMID=integer(),
                        LMmz=double(),
                        LMRT=double(),
                        chromPol=character(),
                        LMName=character())

  #Collecting number of LMID inputs
  conn <- dbConnect(RSQLite::SQLite(),dbName)
  lmIDInd<-as.integer(dbGetQuery(conn,"SELECT COUNT(*) FROM landmarks")$'COUNT(*)')

  for(i in 1:length(injToSub[,1])){
    #Counting up from last LMID
    lmIDInd<-lmIDInd+1
    landmarks[i,]<-c((lmIDInd),injToSub[i,2], injToSub[i,3], injToSub[i,4],injToSub[i,5])
  }

  dbWriteTable(conn, name="landmarks", landmarks, append=TRUE)

  dbDisconnect(conn)
}
