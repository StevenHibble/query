% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{read_lines}
\alias{read_lines}
\title{Read a text file into a single string.}
\usage{
read_lines(file)
}
\arguments{
\item{file}{Path of a text file.}
}
\value{
A character vector of length 1.
}
\description{
Read a text file into a single string.
}
\examples{
fl <- tempfile(fileext = ".sql")
writeLines("SELECT *
FROM tab", fl)
read_lines(fl)
}
