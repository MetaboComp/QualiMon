#' calcOutliers - A function for calculating outliers for checkLM
#'
#' @param Metric A matrix supplied by user for which to calculate outliers within
#' @param alpha Alpha value for significance
#'
#' @return Outliers for all samples tested
#'
#' @export

calcOutliers <- function(Metric, alpha=0.01){
  #check For LaMas with too few hits
  nLaMasFound <- apply(Metric, 2, function(x) sum(!is.na(x)))
  whfew <- which(nLaMasFound<3)
  Metric <- Metric[,-whfew]
  df_ref <- nrow(Metric)-2
  OutliersPerSamp <- vector()
  for(i in 1:nrow(Metric)){
    ValuesToCheck <- Metric[i,]
    MatrixToCompareTo <- Metric[-i,]
    nOutliers <- 0
    for(j in 1:length(ValuesToCheck)){
      if(!is.na(ValuesToCheck[j])){
        Metric_n_z = (ValuesToCheck[j]-mean(MatrixToCompareTo[,j], na.rm=T))/sd(MatrixToCompareTo[,j], na.rm=T)
        Metric_n_p = pt(-abs(Metric_n_z), df=df_ref)
        if(Metric_n_p < alpha){
          nOutliers <- nOutliers+1
        }
      }
    }
    OutliersPerSamp[i] <- nOutliers/(ncol(Metric)-sum(is.na(ValuesToCheck)))
  }
  return(OutliersPerSamp)
}
