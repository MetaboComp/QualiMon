#' dropLandmarkViews - A function for removing landmark-views in the DB
#'
#' @param dbName The name of the .db file in which views for landmarks will be setup
#'
#' @return Nothing


dropLandmarkViews<-function(dbName="NameOfDB.db"){

  s1 <- sprintf("DROP VIEW [lmPeaks_sQC_RP]")
  s2 <- sprintf("DROP VIEW [lmPeaks_sQC_RN]")
  s3 <- sprintf("DROP VIEW [lmPeaks_sQC_HP]")
  s4 <- sprintf("DROP VIEW [lmPeaks_sQC_HN]")

  s5 <- sprintf("DROP VIEW [lmPeaks_sample_RP]")
  s6 <- sprintf("DROP VIEW [lmPeaks_sample_RN]")
  s7 <- sprintf("DROP VIEW [lmPeaks_sample_HP]")
  s8 <- sprintf("DROP VIEW [lmPeaks_sample_HN]")

  s9 <- sprintf("DROP VIEW [landmarks_RP]")
  s10 <- sprintf("DROP VIEW [landmarks_RN]")
  s11 <- sprintf("DROP VIEW [landmarks_HP]")
  s12 <- sprintf("DROP VIEW [landmarks_HN]")

  s13 <- sprintf("DROP VIEW [IPOnLMs]")

  conn <- dbConnect(RSQLite::SQLite(),dbName)

  dbExecute(conn,s1)
  dbExecute(conn,s2)
  dbExecute(conn,s3)
  dbExecute(conn,s4)
  dbExecute(conn,s5)
  dbExecute(conn,s6)
  dbExecute(conn,s7)
  dbExecute(conn,s8)
  dbExecute(conn,s9)
  dbExecute(conn,s10)
  dbExecute(conn,s11)
  dbExecute(conn,s12)
  dbExecute(conn,s13)

  dbDisconnect(conn)
}
