#' IPOScore - Function for calculating isotope score for entire injection
#'
#' @param xset XCMS object
#' @param isotopeIdentification specify which identification algorithm to use, for more information, see IPO package
#' @param ... ???
#'
#' @return PPS value from IPO

IPOscore <- function(xset, isotopeIdentification=c("IPO", "CAMERA"), ...) {

  if (class(xset)[1]=='XCMSnExp'){
    xset <- as(xset, "xcmsSet")
  }

  if(class(xset)[1]!='xcmsSet'){
    stop("Object not XCMSnExp or xcmsSet")
  }

  isotopeIdentification <- match.arg(isotopeIdentification)

  ret <- vector(mode="numeric", 5) #array(0, dim=c(1,5))
  names(ret) <- c("ExpId", "#peaks", "#NonRP", "#RP", "PPS")
  if(is.null(xset)) {
    return(ret)
  }

  if(nrow(peaks_IPO(xset)) == 0) {
    return(ret)
  }

  peak_source <- peaks_IPO(xset)[,c("mz", "rt", "sample", "into", "mzmin",
                                    "mzmax", "rtmin", "rtmax"),drop=FALSE]
  ret[2] <- nrow(peak_source)

  if(isotopeIdentification == "IPO")
    iso_mat <- findIsotopes.IPO(xset, ...)
  else
    iso_mat <- findIsotopes.CAMERA(xset, ...)

  samples <- unique(peak_source[,"sample"])
  isotope_abundance = 0.01108

  #calculating low intensity peaks
  for(sample in samples) {
    non_isos_peaks <- peak_source

    if(nrow(iso_mat) > 0) {
      non_isos_peaks <- peak_source[-unique(c(iso_mat)),,drop=FALSE]
    }

    speaks <- non_isos_peaks[non_isos_peaks[,"sample"]==sample,,drop=FALSE]
    intensities <- speaks[,"into"]
    na_int <- is.na(intensities)
    intensities <- intensities[!na_int]

    if(length(intensities)>0) {
      tmp <- intensities[order(intensities)]
      int_cutoff <- mean(tmp[1:max(round((length(tmp)/33),0),1)])

      masses <- speaks[!na_int, "mz"]
      #floor((masses-2*CH3)/CH2) + 2
      maximum_carbon <- calcMaximumCarbon(masses)
      carbon_probabilty <- maximum_carbon*isotope_abundance

      iso_int <- intensities * carbon_probabilty

      not_loq_peaks <- sum(iso_int>int_cutoff)
      ret[3] <- ret[3] + not_loq_peaks
    }
  }#end_for_sample

  ret[4] <- length(unique(c(iso_mat)))
  if(ret[3] == 0) {
    ret[5] <- (ret[4]+1)^2/(ret[3]+1)
  } else {
    ret[5] <- ret[4]^2/ret[3]
  }

  return(ret)
}

findIsotopes.IPO <-
  function(xset, checkPeakShape=c("none", "borderIntensity", "sinusCurve",
                                  "normalDistr")) {

    checkPeakShape <- match.arg(checkPeakShape)

    iso_mat <- matrix(0, nrow=0, ncol=2)
    if(is.null(xset)) {
      return(iso_mat)
    }

    colnames(iso_mat) <- c("12C", "13C")
    peak_source <- peaks_IPO(xset)[,c("mz", "rt", "sample", "into", "maxo", "mzmin",
                                      "mzmax", "rtmin", "rtmax"), drop=FALSE]

    for(i in 1:ncol(peak_source)) {
      peak_source <- peak_source[!is.na(peak_source[,i]),,drop=FALSE]
    }

    peak_source <- cbind(1:nrow(peak_source), peak_source)
    colnames(peak_source)[1] <- "id"

    #carbon = 12.0
    #hydrogen	= 1.0078250170
    #CH3 = carbon + 3 * hydrogen
    #CH2 = carbon + 2 * hydrogen
    isotope_mass = 1.0033548
    isotope_abundance = 0.01108

    samples <- max(peak_source[,"sample"])

    #start_sample
    for(sample in 1:samples) {
      #only looking into peaks from current sample
      speaks <- peak_source[peak_source[,"sample"]==sample,,drop=FALSE]
      split <- 250
      if(!(checkPeakShape=="none"))
        rawdata <- loadRaw(xcmsSource(filepaths(xset)[sample]))

      if(nrow(speaks)>1) {
        #speaks <- speaks[,-c("sample")]
        speaks <- speaks[order(speaks[,"mz"]),]

        while(!is.null(nrow(speaks)) & length(speaks) > 3) {
          part_peaks <- NULL
          #splitting the data into smaller pieces to improve speed
          if(nrow(speaks) < split) {
            part_peaks <- speaks
          } else {
            upper_bound <- speaks[split,"mzmax"] + isotope_mass
            end_point <- sum(speaks[,"mz"] < upper_bound)
            part_peaks <- speaks[1:end_point,,drop=FALSE]
          }

          rt <- part_peaks[,"rt"]
          rt_window <- rt * 0.005
          rt_lower <- part_peaks[,"rt"] - rt_window
          rt_upper <- part_peaks[,"rt"] + rt_window
          rt_matrix <-
            t(matrix(rep(rt, nrow(part_peaks)), ncol=nrow(part_peaks)))
          rt_matrix_bool <- rt_matrix >= rt_lower & rt_matrix <= rt_upper

          mz <- part_peaks[,"mz"]
          #isotope_masses - mz_window
          mz_lower <- part_peaks[,"mzmin"] + isotope_mass
          #isotope_masses + mz_window
          mz_upper <- part_peaks[,"mzmax"] + isotope_mass
          mz_matrix <-
            t(matrix(rep(mz, nrow(part_peaks)), ncol=nrow(part_peaks)))
          mz_matrix_bool <- mz_matrix >= mz_lower & mz_matrix <= mz_upper

          rt_mz_matrix_bool <- rt_matrix_bool & mz_matrix_bool

          rt_mz_peak_ids <- which(rowSums(rt_mz_matrix_bool)>0)
          calculations <- min(split, nrow(speaks))
          rt_mz_peak_ids <- rt_mz_peak_ids[rt_mz_peak_ids < calculations]

          for(i in rt_mz_peak_ids) {
            current <- part_peaks[i, ,drop=FALSE]
            rt_mz_peaks <- part_peaks[rt_mz_matrix_bool[i,],,drop=FALSE]
            rt_difference <-
              abs(current[,"rt"] - rt_mz_peaks[, "rt"]) / current[,"rt"]
            rt_mz_peaks <- cbind(rt_mz_peaks, rt_difference)
            #test intensity_window
            #floor((current["mz"]-2*CH3)/CH2) + 2
            maximum_carbon <- calcMaximumCarbon(current[,"mz"])
            carbon_probabilty <- c(1,maximum_carbon)*isotope_abundance
            iso_intensity <- current[,"into"] * carbon_probabilty

            int_bools <-
              rt_mz_peaks[,"into"] >= iso_intensity[1] &
              rt_mz_peaks[,"into"] <= iso_intensity[2]

            if(sum(int_bools) > 0) {
              int_peaks <- rt_mz_peaks[int_bools,,drop=FALSE]
              boundary_bool <- rep(TRUE, (nrow(int_peaks)+1))
              if(!(checkPeakShape=="none")) {
                if(checkPeakShape=="borderIntensity") {
                  boundary_bool <- checkIntensitiesAtRtBoundaries(
                    rawdata,
                    rbind(current,int_peaks[,-ncol(int_peaks), drop=FALSE]))
                } else {
                  if(checkPeakShape=="sinusCurve") {
                    boundary_bool <- checkSinusDistribution(
                      rawdata,
                      rbind(current,int_peaks[,-ncol(int_peaks),drop=FALSE]))
                  } else {
                    boundary_bool <- checkNormalDistribution(
                      rawdata,
                      rbind(current,int_peaks[,-ncol(int_peaks),drop=FALSE]))
                  }
                }
              } #else {
              #boundary_bool <- rep(TRUE, (nrow(int_peaks)+1))
              #}
              if(boundary_bool[1] & sum(boundary_bool[-1])>0) {
                iso_peaks <- int_peaks[boundary_bool[-1],,drop=FALSE]
                iso_id <-
                  iso_peaks[which.min(iso_peaks[,"rt_difference"]), "id"]
                #iso_list[[length(iso_list)+1]] <- c(current[,"id"], iso_id)
                iso_mat <- rbind(iso_mat, c(current[,"id"], iso_id))
              }
            }
          }
          speaks <- speaks[-(1:calculations),]

        }#end_while_sample_peaks
      }
    }
    return(iso_mat)
  }

peaks_IPO <- function(xset) {
  # peaks function, to work with older xcms-version (<2.99.7)
  # sample column is missing, if first the first sample processed has no peaks
  # see https://github.com/sneumann/xcms/issues/220
  peaks_act <- xcms::peaks(xset)
  if (!("sample" %in% colnames(peaks_act))) {
    colnames(peaks_act)[colnames(peaks_act) == ""] <- "sample"
  }
  peaks_act
}

calcMaximumCarbon <-
  function(masses) {

    carbon = 12.0
    hydrogen  = 1.0078250170
    CH3 = carbon + 3 * hydrogen
    CH2 = carbon + 2 * hydrogen

    maximum_carbon <- floor((masses-2*CH3)/CH2) + 2

  }

checkIntensitiesAtRtBoundaries <-
  function(rawdata,
           peaks,
           minBoundaryToMaxo=1/3,
           ppmstep=15) {
    ret <- rep(TRUE, nrow(peaks))
    for(i in 1:nrow(peaks)) {
      peak <- peaks[i,]
      for(boundary in c("rtmin", "rtmax")) {
        rtIndex <- which(rawdata$rt==peak[boundary])
        if(length(rtIndex)>0) {
          if(rtIndex==length(rawdata$scanindex)) {
            rtIndices <- c(rawdata$scanindex[rtIndex], length(rawdata$mz))
          } else {
            rtIndices <- rawdata$scanindex[c(rtIndex, rtIndex+1)]
          }

          #only relevant mz and intensity values regarding retention time
          mz <- rawdata$mz[(rtIndices[1]+1):rtIndices[2]]
          intensities <- rawdata$intensity[(rtIndices[1]+1):rtIndices[2]]

          ppm <- peak[c("mzmin", "mzmax")]*ppmstep/1000000
          mzIntensities <-
            c(0, intensities[mz>=peak["mzmin"]-ppm[1] & mz<=peak["mzmax"]+ppm[2]])
          maxBoundaryIntensity <- max(mzIntensities)
          ret[i] <- ret[i] & maxBoundaryIntensity<peak["maxo"]*minBoundaryToMaxo
        }
      }
    }

    return(ret)

  }

checkSinusDistribution <- function(rawdata, peaks) {
  ret <- rep(TRUE, nrow(peaks))
  for(i in 1:nrow(peaks)) {
    ret[i] <- testSinusDistribution(rawdata, peaks[i,,drop=FALSE])
  }

  return(ret)
}

testSinusDistribution <- function(rawdata, peak) {

  y <- getIntensitiesFromRawdata(rawdata, peak)
  if(length(y) < 3) {
    return(FALSE)
  }
  if(max(y)==0) {
    return(FALSE)
  }

  normY <- (y-min(y))/(max(y)-min(y))
  sinCurve <- (sin(seq(-pi/2,pi+1.5,length=length(normY))) + 1) / 2
  correlation <- cor(sinCurve, normY)

  correlation > 0.7

}

checkNormalDistribution <- function(rawdata, peaks) {
  ret <- rep(TRUE, nrow(peaks))
  for(i in 1:nrow(peaks)) {
    ret[i] <- testNormalDistribution(rawdata, peaks[i,,drop=FALSE])
  }

  return(ret)
}

testNormalDistribution <- function(rawdata, peak) {

  y <- getIntensitiesFromRawdata(rawdata, peak)
  if(length(y) < 3) {
    return(FALSE)
  }

  if(max(y)==0) {
    return(FALSE)
  }

  normY <- (y-min(y))/(max(y)-min(y))

  mean=10;
  sd=3;

  seqModel <- seq(-4,4,length=length(normY))*sd + mean
  yModel <- dnorm(seqModel,mean,sd)
  yModel = yModel* (1/max(yModel))
  correlation <- cor(yModel, normY)

  correlation > 0.7


}

getIntensitiesFromRawdata <- function(rawdata, peak) {
  rt <- rawdata$rt >= peak[,"rtmin"] & rawdata$rt <= peak[,"rtmax"]

  rtRange <- c(min(which(rt)), max(which(rt))+1)
  scanIndices <-
    rawdata$scanindex[rtRange[1]:min(rtRange[2], length(rawdata$scanindex))]
  #  scanIndices <- scanIndices[!is.na(scanIndices)]
  if(rtRange[2]>length(rawdata$scanindex)) {
    scanIndices <- c(scanIndices, length(rawdata$intensity))
  }

  if(length(scanIndices) < 3)
    return(FALSE)

  y <- c()
  for(i in 1:(length(scanIndices)-1)) {
    scanRange <- c(scanIndices[i]+1, scanIndices[i+1])
    mz <- rawdata$mz[scanRange[1]:scanRange[2]]
    y <-
      c(y,
        max(0, (rawdata$intensity[scanRange[1]:scanRange[2]][
          mz >= peak[,"mzmin"] & mz <= peak[,"mzmax"]])
        )
      )
  }

  y
}
