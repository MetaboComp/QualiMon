#' addProject - A function for adding a project to the DB
#'
#' @param dbName The name of the .db file in which views for landmarks will be setup
#' @param projName The name of the project to be submitted to the DB
#' @param person The person who's in charge of this project
#'
#' @return Nothing

addProject<-function(dbName, projName, person="Unknown"){
  conn <- dbConnect(RSQLite::SQLite(),dbName)

  s1<-sprintf('INSERT INTO projects (projName, person) VALUES (\'%s\',\'%s\');', projName, person)
  dbExecute(conn,s1)

  dbDisconnect(conn)
}
