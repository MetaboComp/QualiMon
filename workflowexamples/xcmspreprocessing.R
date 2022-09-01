rm(list=ls())
library(CMSITools) ##Only used for function getFiles, Only used to simplify data formatting
library(tidyverse)

LCMS_data <- getFiles("C:/temp/QC-data") %>% getRP() %>% getPOS() #Load filenames and meta data into format suitable for XCMS. Function requires specific naming strategy
##LCMS_data object us a data frame containing information on $date, $batch, $chromatography, $polarity, $injection (number), $fileformat, $filename, $folder, $path, and $fullname (folderpath + filename)


#Define groups (N.B. If QC data is used, all samples are marked as "QCs")
LCMS_data$group <- rep(NA,nrow(LCMS_data))
LCMS_data$group[LCMS_data$sample %>% grep("QC",.)] <- "sQC"
LCMS_data$group[-c(LCMS_data$sample %>% grep("QC",.))] <- "sample"
LCMS_data <- LCMS_data[-grep("CP", LCMS_data$filename),] #Removal of injections with Conditioning Plasma

LCMS_data_names <- LCMS_data$filename


#Set up meta data for injections
pd <- data.frame(
  sample_name = sub(LCMS_data_names, pattern = ".mzML", #remove .mzML from filename
                    replacement = "", fixed = TRUE),
  sample_group = paste0(LCMS_data$group),
  batch = LCMS_data$batch,
  injection = LCMS_data$injection, stringsAsFactors = FALSE)

#read raw data and perform peakpicking. Parameters suitable for the current data is necessary at this point
raw_data <- readMSData(files = LCMS_data$fullname, pdata = new("NAnnotatedDataFrame", pd),
                       mode = "onDisk") %>% filterRt(.,rt = c(0, 630)) #filterRt depends on the chromatography
cwp <- CentWaveParam(peakwidth = c(7.6, 23.8), noise = 500, ppm = 10, mzdiff = 0.0056, prefilter = c(3,2000), integrate = 1) 
xdataQC_POS <- findChromPeaks(raw_data, param = cwp)
saveRDS(xdataQC_POS,"QC_peakpicking.rds") #save intermediate object


#Perform retention time adjustment
pgp <- ObiwarpParam(binSize=0.95, response=9.75, gapInit= 0.45, gapExtend=2.5)
AdjPOS <- adjustRtime(xdataQC_POS, param = pgp)
plotAdjustedRtime(AdjPOS)

save(AdjPOS, file="Adjusted_POS_QC_data.rda")


#perform feature correspondance
pdp <- PeakDensityParam(sampleGroups = LCMS_data$group,
                        minFraction = 0.4, bw = 2, binSize = 0.015)
XdataGroupedPOS <- groupChromPeaks(AdjPOS, param=pdp)

saveRDS(XdataGroupedPOS, file='grouped POS QC_data.rds')

PTQCPOS <- getTable(XdataGroupedPOS) #creates a peak table structured correctly for input to 'find LaMas'
saveRDS(PTQCPOS, file='PeakTablePostCorresponance.rds')
