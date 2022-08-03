#' buildDB - A function for creating a new, empty .db file with the structure used
#'
#' @param dbName The desired name for the .db file which will be created
#' @param path Path to where DB is located, defaults to working directory
#'
#' @return Nothing


buildDB<-function(dbName="NameOfDB.db", path=getwd()){

  ###Setting up the structure for the DB using data frames###
  people<-data.frame(person=character(),
                     email=character())
  # telnumb=integer(),
  # position=character())

  projects<-data.frame(projectID=integer(),
                       projName=character(),
                       person=character())

  samples<-data.frame(sampID=integer(),
                      projName=integer(),
                      type=character(),
                      matrix=character(),
                      name=character())

  injections<-data.frame(injID=integer(),
                         sampID=integer(),
                         date=character(),
                         batchWeek=character(),
                         seqInj=integer(),
                         chromPol=character(),
                         instrument=character(),
                         MSMode=character(),
                         IPOscore=double(),
                         nLMs=integer(),
                         nPeaks=integer(),
                         TIC=integer(),
                         name=character())

  peaks<-data.frame(peakID=integer(),
                    injID=integer(),
                    mz=double(),
                    RT=double(),
                    int=double(),
                    annotation=character(),
                    adduct=character(),
                    IDLVL=integer())

  peakMS2link<-data.frame(peakID=integer(),
                          ms2ID=integer(),
                          precMZ=double(),
                          precRT=double(),
                          precINT=double(),
                          CollE=double(),
                          injID=integer(),
                          curator=character())

  ms2spectra<-data.frame(ms2ID=integer(),
                         mz=double(),
                         int=double())

  landmarks<-data.frame(LMID=integer(),
                        LMmz=double(),
                        LMRT=double(),
                        chromPol=character(),
                        LMName=character())

  lmPeaks<-data.frame(peakIDLM=integer(),
                      injID=integer(),
                      mz=double(),
                      RT=double(),
                      int=double(),
                      nLMMatch=integer(),
                      LMID=integer(),
                      height=double(),
                      fwhm=double(),
                      tf=double(),
                      sn=double(),
                      noise=double(),
                      dataPoints=integer())

  lmQuality <- data.frame(qualLMID=integer(),
                          injID=integer(),
                          lmIntOutliers=integer(),
                          lmIntOutliers_p=integer(),
                          lmRTOutliers=integer(),
                          lmRTOutliers_p=integer(),
                          lmHeightOutliers=integer(),
                          lmHeightOutliers_p=integer(),
                          lmFWHMOutliers=integer(),
                          lmFWHMOutliers_p=integer(),
                          lmTFOutliers=integer(),
                          lmTFOutliers_p=integer(),
                          lmSNOutliers=integer(),
                          lmSNOutliers_p=integer(),
                          lmDPOutliers=integer(),
                          lmDPOutliers_p=integer(),
                          lmNoise=integer(),
                          lmNoise_p=integer(),
                          status=double(),
                          sampleNumber=integer(),
                          sampleIter=integer())





  ###Setting up statements to pass to SQLite so that rules for the tables can be added

  #People
  s1 <- sprintf("create table %s(%s, primary key(%s))",
                "people",
                paste(names(people), collapse = ", "),
                names(people)[1])

  #Projects
  s2 <- sprintf("create table %s(%s integer primary key not null, %s, foreign key(person) references people(person))",
                "projects",
                paste(names(projects)[1], collapse = ", "),
                paste(names(projects)[-1], collapse = ", "))

  #Samples
  s3 <- sprintf("create table %s(%s integer primary key not null, %s, foreign key(projName) references projects(projName))",
                "samples",
                names(samples)[1],
                paste(names(samples)[-1], collapse = ", "))

  #Injections
  ###OBS Check naming conventions for chrom and pol. Might not be correct here
  s4 <- sprintf("create table %s(%s integer primary key not null, %s, foreign key(sampID) references samples(sampID), constraint CHK_chromPol check (chromPol in ('HP', 'HN', 'RP','RN')))",
                "injections",
                names(injections)[1],
                paste(names(injections)[-1], collapse = ", "))

  #Peaks
  s5 <- sprintf("create table %s(%s integer primary key not null, %s, foreign key(injID) references injections(injID))",
                "peaks",
                names(peaks)[1],
                paste(names(peaks)[-1], collapse = ", "))

  #PeakMS2Link
  s6 <- sprintf("create table %s(%s, foreign key(peakID) references peaks(peakID), foreign key (ms2ID) references ms2spectra(ms2ID), foreign key (injID) references injections(injID), foreign key (curator) references people(person))",
                "peakMS2link", #, primary key(peakID, ms2ID)
                paste(names(peakMS2link), collapse = ", "))

  #MS2Spectra
  s7 <- sprintf("create table %s(%s)",
                "ms2spectra",
                paste(names(ms2spectra), collapse = ", "))
  #Landmarks
  s8 <- sprintf("create table %s(%s integer primary key not null, %s)",
                "landmarks",
                "LMID",
                paste(names(landmarks)[-1], collapse=", "))

  #LMPeaks
  s9 <- sprintf("create table %s(%s integer primary key not null, %s, foreign key(injID) references injections(injID), foreign key(LMID) references landmarks(LMID))",
                "lmPeaks",
                "peakIDLM",
                paste(names(lmPeaks)[-1], collapse=", "))

  #lmQuality
  s10 <- sprintf("create table %s(%s integer primary key not null, %s, foreign key(injID) references injections(injID))",
                 "lmQuality",
                 "qualLMID",
                 paste(names(lmQuality)[-1], collapse=", "))



  #Connecting to new DB and writing table to it
  conn <- dbConnect(RSQLite::SQLite(),paste0(path,"\\",dbName))

  dbExecute(conn,s1)
  dbExecute(conn,s2)
  dbExecute(conn,s3)
  dbExecute(conn,s4)
  dbExecute(conn,s5)
  dbExecute(conn,s7)
  dbExecute(conn,s6)
  dbExecute(conn,s8)
  dbExecute(conn,s9)
  dbExecute(conn,s10)

  dbDisconnect(conn)

  setupLandmarkViews(dbName, path)
}
