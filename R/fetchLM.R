#' fetchLM - A function for fetching injection data from DB
#'
#' @param dbName Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"
#' @param chromPol The chromatography + polarity of the analysis: "RP" = Reversed phase positive, "HN" = HILIC negative, etc.
#'
#' @return Returning a data frame of LaMas


fetchLM <- function(dbName="Test.db", chromPol){

  landmarks<-list()

  conn <- dbConnect(RSQLite::SQLite(),dbName)

  s1<-sprintf("SELECT * FROM [landmarks_%s]",chromPol)
  landmarks<-dbGetQuery(conn, s1)

  dbDisconnect(conn)

  return(landmarks)
}
