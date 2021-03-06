% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-imm.R
\name{read_imm}
\alias{read_imm}
\alias{read_imm_vector}
\title{Read IMM files}
\usage{
read_imm(file, tz = "UTC")

read_imm_vector(file_vector, tz = "UTC")
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
Read IMM files
}
\examples{
imm_file <- bs_example("imm/18082902.imm")
read_imm(imm_file)
read_imm_vector(imm_file)

}
