#' find_LM - A function to find landmarks in a XCMS object
#'
#' @param mode For setting the name of the output
#' @param XCMSObj XCMS object after correspondence
#' @param time_interval Specifies how large retention time interval that the landmarks are selected in
#' @param mzdif The m/z difference allowed between features
#' @param rtdif The retention time difference allowed between features
#'
#' @return A list of the metadata of the landmarks.


find_LM <- function(mode, XCMSObj, time_interval, mzdif, rtdif) #Changed to "find_LM" instead of "find_landmarks"
{
  ####Extracting feature information from XCMS object and setting up counters and new dataframes
  featurevalues = featureValues(XCMSObj)
  featuretable = featureDefinitions(XCMSObj)[complete.cases(featurevalues),] %>% as.data.frame()
  featuretable=featuretable[featuretable$rtmed <= 630,] #Should this hard filter value here be based on an input from the user?
  dat = featuretable[,c(1,4)]
  dat=dat[!duplicated(dat$rtmed),]
  landmarks = matrix(NA, nrow = nrow(dat), ncol = 2) %>% as.data.frame()
  colnames(landmarks) = colnames(dat)
  i = 0
  N = min(dat$rtmed)

  #While N is not higher than highest feature RT median in the XCMS object supplied with the function, keep looping
  while (!N>max(dat$rtmed)) {
    interval = dat[dat$rtmed>=N & dat$rtmed<N+time_interval,]
    if(nrow(interval) == 0){
      i = i+1
      N = N+time_interval
    }

    else if(nrow(interval)==1){
      a = interval$mzmed
      landmarks[(i+1):(i+length(a)),] = dat[dat$mzmed%in%a,]
      N = N+time_interval
      i = i+length(a)
    }

    else {
      j = 1
      k = c(j)
      mz = interval$mzmed %>% sort()
      sbc = outer(mz,mz,"-") %>% abs()

      while (j < nrow(sbc)) {
        d = sbc[,j] > mzdif
        g = d[-(1:j)]

        if(all(g==FALSE))
        {break}
        else{
          h = which(g) %>% min()+j
          k = c(k,h)
          j = h
        }
      }
      a = interval$mzmed[k]
      landmarks[(i+1):(i+length(a)),] = dat[dat$mzmed%in%a,]
      N = N+time_interval
      i = i+length(a)
    }
  }

  ####Formatting output from loop####
  landmarks = landmarks[!is.na(landmarks$mzmed),]
  landmarks = landmarks[order(landmarks$rtmed),]
  landmarks = landmarks[abs(diff(landmarks$rtmed))>rtdif & abs(diff(landmarks$mzmed))>mzdif,]
  landmarks_name = data.frame(landmarks=paste(mode,landmarks$mzmed, landmarks$rtmed, sep = "_"))
  rownames(landmarks) = landmarks_name[,1]
  plot(x = landmarks$rtmed, y = landmarks$mzmed, main = paste(mode,"landmarks", sep = "_"))
  return(landmarks)
}
