#' setupLandmarkViews - A function for setting up landmark-views in the DB
#'
#' @param dbName The name of the .db file in which views for landmarks will be setup
#' @param path Path where DB is located
#'
#' @return Nothing

setupLandmarkViews<-function(dbName="NameOfDB.db", path=getwd()){

  ####lmPeaks views####
  #sQC views
  s1 <- sprintf("CREATE VIEW [lmPeaks_sQC_RP] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sQC' AND i.chromPol='RP'")
  s2 <- sprintf("CREATE VIEW [lmPeaks_sQC_RN] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sQC' AND i.chromPol='RN'")
  s3 <- sprintf("CREATE VIEW [lmPeaks_sQC_HP] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sQC' AND i.chromPol='HP'")
  s4 <- sprintf("CREATE VIEW [lmPeaks_sQC_HN] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sQC' AND i.chromPol='HN'")

  #sample views
  s5 <- sprintf("CREATE VIEW [lmPeaks_sample_RP] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sample' AND i.chromPol='RP'")
  s6 <- sprintf("CREATE VIEW [lmPeaks_sample_RN] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sample' AND i.chromPol='RN'")
  s7 <- sprintf("CREATE VIEW [lmPeaks_sample_HP] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sample' AND i.chromPol='HP'")
  s8 <- sprintf("CREATE VIEW [lmPeaks_sample_HN] AS SELECT l.injID, l.RT, l.int, l.height, l.fwhm, l.tf, l.sn, l.noise, l.dataPoints, l.LMID, s.projName, l.nLMMatch FROM lmPeaks l, injections i, samples s WHERE i.injID=l.injID AND s.sampID=i.sampID AND s.type='sample' AND i.chromPol='HN'")

  ####landmark views####
  s9<-sprintf("CREATE VIEW [landmarks_RP] AS SELECT * FROM landmarks WHERE chromPol='RP'")
  s10<-sprintf("CREATE VIEW [landmarks_RN] AS SELECT * FROM landmarks WHERE chromPol='RN'")
  s11<-sprintf("CREATE VIEW [landmarks_HP] AS SELECT * FROM landmarks WHERE chromPol='HP'")
  s12<-sprintf("CREATE VIEW [landmarks_HN] AS SELECT * FROM landmarks WHERE chromPol='HN'")

  #####IPO & nLMs####
  s13<-sprintf("CREATE VIEW [IPOnLMs] AS SELECT i.injID, i.IPOscore, i.nLMs, i.nPeaks, i.chromPol, i.TIC, i.name, s.projName, s.type FROM injections i, samples s WHERE i.sampID=s.sampID")

  ####Annotation related####
  s14<-sprintf("CREATE VIEW [UniqueStds] AS SELECT DISTINCT p.annotation FROM peaks p, injections i, samples s WHERE s.type = 'Std' AND s.sampID = i.sampID AND p.injID = i.injID")

  conn <- dbConnect(RSQLite::SQLite(),paste0(path,"//",dbName))

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
  dbExecute(conn,s14)

  dbDisconnect(conn)
}
