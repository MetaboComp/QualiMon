% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FolderMonitoring.R
\name{initFolderMonitoring}
\alias{initFolderMonitoring}
\title{Function to start monitoring .d files from LC/MS}
\usage{
initFolderMonitoring(
  configFilePath,
  availableChromPols,
  progressMonitor = function(i) cat(".")
)
}
\arguments{
\item{configFilePath}{File path to config file to use for monitoring}

\item{availableChromPols}{All chromPols for which LaMas are available}
}
\value{
Messages regarding the sample quality
}
\description{
Function to start monitoring .d files from LC/MS
}
