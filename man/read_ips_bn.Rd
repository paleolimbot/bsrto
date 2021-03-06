% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-ips.R
\name{read_ips_bn}
\alias{read_ips_bn}
\alias{read_ips_bn_vector}
\title{Read IPS files}
\usage{
read_ips_bn(file, tz = "UTC")

read_ips_bn_vector(file_vector, tz = "UTC")
}
\arguments{
\item{file}{A file, URL, or connection. Files ending in .gz, .bz2, .xz, or
.zip will be automatically uncompressed; URLs will be automatically
downloaded. See \code{\link[readr:read_lines]{readr::read_lines()}} for a full description of how
this parameter is interpreted.}

\item{tz}{Timezone for which date times are representative.}

\item{file_vector}{A vector of files or URLs}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}}
}
\description{
Read IPS files
}
\examples{
bn_file <- bs_example("ips/191010AA.bn1")
read_ips_bn(bn_file)
read_ips_bn_vector(bn_file)

}
