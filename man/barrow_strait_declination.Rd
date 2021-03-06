% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/science.R
\name{barrow_strait_declination}
\alias{barrow_strait_declination}
\title{Approximate declination for Barrow Strait}
\usage{
barrow_strait_declination(date_time)
}
\arguments{
\item{date_time}{A POSIXct for which declination should be calculated}
}
\value{
A numeric vector of approximated declination
}
\description{
Approximate declination for Barrow Strait
}
\examples{
barrow_strait_declination(Sys.time())

}
