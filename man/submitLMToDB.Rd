% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/submitLMToDB.R
\name{submitLMToDB}
\alias{submitLMToDB}
\title{submitLMToDB - A function for fetching injection data from DB}
\usage{
submitLMToDB(dbName = "NameOfDB.db", injToSub)
}
\arguments{
\item{dbName}{Name / path to a SQLite of the same format as the example file. To make a new one see function buildDB}

\item{injToSub}{A df with 9 columns: sampID, date, batchWeek, seqInj, chromPol, instrument, msMode, IPOscore and nLMs}
}
\value{
Nothing
}
\description{
submitLMToDB - A function for fetching injection data from DB
}
