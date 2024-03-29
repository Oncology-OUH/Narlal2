% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectFirstNPatients.R
\name{SelectFirstNPatients}
\alias{SelectFirstNPatients}
\title{Select from the data the first n included patients}
\usage{
SelectFirstNPatients(df, n)
}
\arguments{
\item{df}{the original data frame obtain by using LoadAndPrepareData}

\item{n}{the number of patients to be selected e.g. 350 as for the primary endpoint}
}
\value{
a data frame similar to the original but only including the initial n patients based on the date of randomization. All patients randomised on the last date will be included in the data
}
\description{
Select from the data the first n included patients
}
\examples{
file <- system.file('extdata','DemoData.csv',package="Narlal2")
df <- LoadAndPrepareData(filename=file)
df <- SelectFirstNPatients(df,350)
}
