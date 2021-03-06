% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_queries}
\alias{get_queries}
\title{Read a directory of queries into a single list.}
\usage{
get_queries(path = "", pattern = "\\\\.sql$")
}
\arguments{
\item{path}{Path to a directory containing query files.}

\item{pattern}{A regular expression for finding relevant query files.}
}
\value{
A list of character vectors, each of length 1.
}
\description{
Takes a directory and finds all query files and organizes them into a list.
The list makes for easy autocompletion and interoperability with \code{query()}.
}
\examples{
dr <- tempdir()
writeLines("SELECT * FROM view1 WHERE col = {val}", paste0(dr, "/view1.sql"))
writeLines("SELECT * FROM view2 WHERE col = {val}", paste0(dr, "/view2.sql"))
get_queries(dr)

# The queries are then easy to access and pass to query()
\dontrun{
my_queries <- get_queries(dr)
query(conn, my_queries$view1, val = "abc")
}
}
