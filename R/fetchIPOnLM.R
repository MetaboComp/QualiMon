#' fetchIPOnLM - A function for fetching previous injections in the same seq.
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"
#' @param toFetch An object containing information on what to fetch
#'
#' @return Returning data frame of LaMa data



fetchIPOnLM<-function(dbName="Test.db", toFetch){

  IPOnLM<-list()

  conn <- dbConnect(RSQLite::SQLite(),dbName)

  s1<-sprintf("SELECT injID, IPOscore, nLMs, nPeaks, TIC, name FROM [IPOnLMs] WHERE type='%s' AND chromPol='%s'", toFetch$type, toFetch$chromPol) #AND projName='%s'    toFetch$projName,
  IPOnLM<-dbGetQuery(conn, s1)

  dbDisconnect(conn)

  return(IPOnLM)

}
