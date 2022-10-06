#' fetchIPOnLM - A function for fetching previous injections in the same seq.
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"
#' @param toFetch An object containing information on what to fetch
#'
#' @return Returning data frame of LaMa data



fetchIPOnLM<-function(dbName="Test.db", toFetch){

  IPOnLM<-list()

  conn <- dbConnect(RSQLite::SQLite(),dbName)

  s1 <- sprintf("SELECT DISTINCT injID FROM [IPOnLMs] l WHERE l.type='%s' AND l.chromPol='%s'",
                toFetch$type,
                toFetch$chromPol)
  distInjID <- as.vector(unlist(dbGetQuery(conn, s1)))
  distInjID <- ifelse(is.na(distInjID), 0, distInjID)

  if(length(distInjID)>(toFetch$nSampsMonitor-1)){
    distInjID <- distInjID[c((length(distInjID)-(toFetch$nSampsMonitor-2)):length(distInjID))]
  }

  s2<-sprintf("SELECT injID, IPOscore, nLMs, nPeaks, TIC, name FROM [IPOnLMs] WHERE type='%s' AND chromPol='%s' AND injID IN (%s)",
              toFetch$type,
              toFetch$chromPol,
              paste(as.character(distInjID), collapse=", ",sep="")) #AND projName='%s'    toFetch$projName,
  IPOnLM<-dbGetQuery(conn, s2)

  dbDisconnect(conn)

  return(IPOnLM)

}
