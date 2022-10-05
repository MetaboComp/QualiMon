#' fetchLMRefDF - A function for fetching previous injections in the same seq.
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"
#' @param toFetch An object containing three variables: 'type' (containing 'sQC' or 'sample'), 'batchweek' (to determine which sequence the sample was injected in) and 'chromPol' (chromatography & pol; 'RP', 'RN', 'HP', 'HN')
#'
#' @return Returning table of LaMas for specified chromPol and sample type


fetchLMRefDF<-function(dbName="Test.db", toFetch){

  lmPeaks<-list()

  conn <- dbConnect(RSQLite::SQLite(),dbName)

  s1 <- sprintf("SELECT DISTINCT injID FROM [lmPeaks_%s_%s] l",
               toFetch$type,
               toFetch$chromPol)
  distInjID <- as.vector(unlist(dbGetQuery(conn, s1)))
  distInjID <- ifelse(is.na(distInjID), 0, distInjID)

  if(length(distInjID)>(toFetch$nSampsMonitor-1)){
    distInjID <- distInjID[c((length(distInjID)-(toFetch$nSampsMonitor-2)):length(distInjID))]
  }

  s2<-sprintf("SELECT * FROM [lmPeaks_%s_%s] l WHERE l.injID IN (%s)",
              toFetch$type,
              toFetch$chromPol,
              paste(as.character(distInjID), collapse=", ",sep="")) # WHERE projName='%s'   ,toFetch$projName

  lmPeaks<-dbGetQuery(conn, s2)
  dbDisconnect(conn)

  return(lmPeaks)

}
