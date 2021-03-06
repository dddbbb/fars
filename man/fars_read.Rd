% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read FARS data}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{Integer or string name of file for reading}
}
\value{
data.frame is output of the function
}
\description{
Reads data into data.frame from working directory.
.csv file should be already downloaded or error message will throw out
}
\examples{

\dontrun{
fars_read("accident_2015.csv.bz2")
}
}
