#' Read a text file into a single string.
#'
#' @param file Path of a text file.
#' @return A character vector of length 1.
#' @examples
#' fl <- tempfile(fileext = ".sql")
#' writeLines("SELECT *
#' FROM tab", fl)
#' read_lines(fl)
read_lines <- function(file) readChar(file, file.info(file)$size)


#' Read a query from a file or a string and execute it (or print instead).
#'
#' Executes a parameterized query and returns the results as a data frame.
#' Optionally, the query can returned as a string instead (\code{show_query = TRUE}).
#' \code{glue::glue_sql} is used to interpolate variables. This is not as safe or efficient
#' as parameterized queries, but provides a consistent interface across SQL backends. Variables can
#' be read from the calling environment or passed directly using named parameters.
#'
#' @param conn A DBI connection object obtained from \code{DBI::dbConnect()}).
#' @param query A path to a query file or a query string.
#' @param ... Named parameters to pass to \code{glue::glue_sql()}).
#' @return If \code{show_query = FALSE} (the default), a data frame.
#' If \code{show_query = TRUE}, a character vector of length 1.
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbWriteTable(con, "iris", iris)
#' query(con, "select * from iris")
#'
#' # Parameters can be passed from the global environment
#' val <- "setosa"
#' query(con, "select * from iris where species = {val}")
#'
#' # Parameters can also be passed directly to query()
#' query(con, "select * from iris where species = {val}", val = "virginica")
#'
#' # If you want to view the query without running it, use show_query = TRUE
#' # Note that parameters passed are used instead of global variables of the same name
#' query(con, "select * from iris where species = {val}", val = "virginica", show_query = TRUE)
#' @export
query <- function(conn, query, show_query = FALSE, ...) {

  if (file.exists(query)) {
    query <- read_lines(query)
  }

  query <- glue::glue_sql(.con = conn, query, ...)

  if (show_query) {
    return(query)
  } else {
    return(DBI::dbGetQuery(conn = conn, statement = query))
  }
}

#' Read a directory of queries into a single list.
#'
#' Takes a directory and finds all query files and organizes them into a list.
#' The list makes for easy autocompletion and interoperability with \code{query()}.
#'
#' @param path Path to a directory containing query files.
#' @param pattern A regular expression for finding relevant query files.
#' @return A list of character vectors, each of length 1.
#' @examples
#' dr <- tempdir()
#' writeLines("SELECT * FROM view1 WHERE col = {val}", paste0(dr, "/view1.sql"))
#' writeLines("SELECT * FROM view2 WHERE col = {val}", paste0(dr, "/view2.sql"))
#' get_queries(dr)
#'
#' # The queries are then easy to access and pass to query()
#'\dontrun{
#' my_queries <- get_queries(dr)
#' query(conn, my_queries$view1, val = "abc")
#'}
#' @export
get_queries <- function(path = "", pattern = "\\.sql$") {

  folder <- normalizePath(path, winslash = "/")
  file_list <- dir(folder, pattern = pattern, full.names = TRUE, recursive = TRUE)

  nms <- file_list
  nms <- sub(folder, "", nms, fixed = TRUE)
  nms <- sub(pattern, "", nms)
  nms <- sub("^[/\\]", "", nms)
  names(file_list) <- nms

  as.list(file_list)
}
