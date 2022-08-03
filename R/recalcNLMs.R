#' recalcNLMs - Function to recalculate nLMs from the information in the database. Only neccessary when code has been written wrongly in the first place.
#'
#' @param dbName Name of the DB to fetch from
#' @param chromPol The chromatography and ionization polarity of the files to fetch, expressed as :"RP", "RN", "HP" or "HN"
#' @param projName The projName of interest as it is stored in the DB file
#' @param sampType The type of injections of interest, "sQC" or "sample"
#'
#' @return Nothing

recalcNLMs<-function(dbName="MedGICarbDB.db", chromPol, projName, sampType){

  conn <- dbConnect(RSQLite::SQLite(),dbName)

  toFetch<-list()
  toFetch$type<-sampType
  toFetch$chromPol<-chromPol
  toFetch$projName<-projName

  fetched<-fetchLMRefDF(dbName=dbName, toFetch=toFetch)
  fetchedInjID<-unique(fetched[,1])

  nLMs<-rep(NA,length(fetchedInjID))

  #Going through every injection and recalculating the number of LMs
  for(i in 1:length(nLMs)){
    if(identical(is.na(fetched[which(fetched[,1]==fetchedInjID[i]),2]),is.na(fetched[which(fetched[,1]==fetchedInjID[i]),3]))){
      nLMs[i]<-length(fetched[which(fetched[,1]==fetchedInjID[i]),2])-sum(is.na(fetched[which(fetched[,1]==fetchedInjID[i]),2]))
    }
  }

  #Update all injection ids which had their nLMs recalculated
  for(i in 760:length(nLMs)){
    sTemp<-sprintf("UPDATE injections SET nLMs=%s WHERE injID=\"%s\"",
                   nLMs[i],
                   fetchedInjID[i])
    dbExecute(conn,sTemp)
    Sys.sleep(0.01)
  }

  dbDisconnect(conn)

}
