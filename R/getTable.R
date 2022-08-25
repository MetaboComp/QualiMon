#' getTable - Function for extracting a peak-table from an XCMS object
#'
#' @param XObj An XCMS object which has been processed to the "groupChromPeaks" step but before applying "fillChromPeaks"
#'
#' @return A peak table containing samples and features
#'
#' @export getTable

getTable <- function(XObj) {
  table <- featureValues(XObj, value = "into")
  featInfo <- featureDefinitions(XObj)
  featInfo <- paste(featInfo$mzmed,featInfo$rtmed,sep='@')
  rownames(table) <- featInfo
  table <- t(table)
  return(table)
}
