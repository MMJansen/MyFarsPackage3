% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Reads in multiple files}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector of numbers or character string with numbers}
}
\value{
For each element of the input vector, This function returns the month and year column of
dataframe containing the data in the filename if it existst.
In case filename doesn't exist, an error is displayed.
}
\description{
From given years in numbers, this function tries to read in the corresponding data sets,
and selects months and year
}
\examples{
\dontrun{
fars_read_years(c(2013,2014))
}
}
