#' fetchLMRefDF - A function for fetching previous injections in the same seq.
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"
#' @param toFetch An object containing three variables: 'type' (containing 'sQC' or 'sample'), 'batchweek' (to determine which sequence the sample was injected in) and 'chromPol' (chromatography & pol; 'RP', 'RN', 'HP', 'HN')
#'
#' @return Returning table of LaMas for specified chromPol and sample type


fetchLMRefDF<-function(dbName="Test.db", toFetch){

  lmPeaks<-list()

  conn <- dbConnect(RSQLite::SQLite(),dbName)

  s1<-sprintf("SELECT * FROM [lmPeaks_%s_%s]",toFetch$type,toFetch$chromPol) # WHERE projName='%s'   ,toFetch$projName
  lmPeaks<-dbGetQuery(conn, s1)

  dbDisconnect(conn)

  return(lmPeaks)

}
