% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchLM.R
\name{fetchLM}
\alias{fetchLM}
\title{fetchLM - A function for fetching injection data from DB}
\usage{
fetchLM(dbName = "Test.db", chromPol)
}
\arguments{
\item{dbName}{Name / path to a SQLite of the same format as the example file. To make a new one see function "buildDB()"}

\item{chromPol}{The chromatography + polarity of the analysis: "RP" = Reversed phase positive, "HN" = HILIC negative, etc.}
}
\value{
Returning a data frame of LaMas
}
\description{
fetchLM - A function for fetching injection data from DB
}
