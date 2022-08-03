#' CSVtoLM - A function for taking a .csv file and formatting it for submission to LM DB
#'
#' @param filePath The path to the csv-file, based on the LMTemplate.csv file, with the LMs to be uploaded
#'
#' @return Returning data frame of LaMas

CSVtoLM<-function(filePath=""){

  if(filePath==""){
    writeLines(paste("Please select the .csv LM-containing file:"))
    filePath<-file.choose()
  }

  LMDF<-read.csv2(filePath)
  return(LMDF)

}
