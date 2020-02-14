#' query: A package for managing SQL queries in external files.
#'
#' The query package provides two simple functions:
#' \code{query()} and get_queries(). These can be used together to make managing and
#' running SQL queries easy. Use \code{query()} to run SQL queries (as a string or stored in a file)
#' with interpolation provided by \code{glue::glue_sql()}. Use \code{get_queries()} to register a
#' directory of query files and run them using \code{query()}.
#'
#' @docType package
#' @name query
NULL
