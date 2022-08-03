#' predictLMRT - A function for building model of difference in RT between sample and DB
#'
#' @param LMRT RT of LaMas in DB
#' @param SampRT RT of LaMas in sample
#'
#' @return Returning predicted RTs of non-found LaMas

predictLMRT<-function(LMRT, SampRT){
  loessObj<-loess(SampRT~LMRT)
  toCheck<-as.double(which(is.na(SampRT)))

  predictedRTs<-vector(length=length(toCheck))

  for(i in 1:length(toCheck)){
    predictedRTs[i]<-predict(loessObj, LMRT[toCheck[i]])
    names(predictedRTs)[i]<-rownames(SampRT)[toCheck[i]]
  }

  return(cbind(toCheck,predictedRTs))
}
