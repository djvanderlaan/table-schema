# TODO: handle missing values using na.strings
# TODO: handle ; separator  


#' Read data from a CSV-file using table-schema
#'
#' @param filename the name of the CSV-file
#' @param schema the name of the file containing the table-schema.
#' @param use_fread use the \code{\link[data.table]{fread}} function instead of
#'   \code{\link[utils]{read.csv}} and return a \code{data.table}.
#' @param ... additional arguments are passed on to \code{read.csv} or 
#'   \code{fread}.
#'
#' @details
#' The data is read and the converted to the correct R-type using the schema.
#' When possible it is attempted to leave the conversion to \code{read.csv} or
#' \code{fread}. For example, a column of type \code{number} with a decimal mark
#' of '.' and no thousand separator is read in as is. Other fields are generally
#' read in as character and then converted to the correct R-type. 
#'
#' @return
#' Returns a \code{data.frame} (or \code{data.table} when 
#' \code{use_fread = TRUE}). The schema is added in the \code{schema} attribute
#' of the \code{data.frame} and the schema of the columns/fields is added to
#' each of the columns. 
#' 
#' @export
csv_read <- function(filename, 
    schema = paste0(tools::file_path_sans_ext(filename), ".schema.json"), 
    use_fread = FALSE, ...) {
  if (is.character(schema)) schema <- jsonlite::read_json(schema)
  # Determine how we need to read each of the columns
  colclasses <- sapply(schema$fields, csv_colclass)
  # Read
  if (use_fread) {
    if (!requireNamespace("data.table")) stop("In order to use ", 
        "'use_fread=TRUE' the data.table package needs to be installed.")
    dta <- data.table::fread(filename, 
      colClasses = colclasses, stringsAsFactors = FALSE, ...)
  } else {
    dta <- utils::read.csv(filename, colClasses = colclasses, 
      stringsAsFactors = FALSE, na.strings = "", ...)
  }
  convert_using_schema(dta, schema)
}

