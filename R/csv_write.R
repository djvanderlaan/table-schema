
#' Write data to CSV-file and the table schema to a JSON file
#' 
#' @param x the dataset to write to file. 
#' @param filename the name of the CSV-file. If empty string the contents are
#'   written to the console.
#' @param schema the name fo the JSON-file to which the schema should be
#'   written. If missing and \code{filename} is an empty string the output is 
#'   written to the console.
#' @param delimiter the field separator character. See the \code{sep} argument
#'   of \code{\link[utils]{read.csv}} and \code{\link[data.table]{fread}}.
#' @param decimalChar the decimal separator to use in number field. Note that 
#'   separator is only used for a field when the field schema does not specify a 
#'   separator to use.
#' @param ... ignored for now.
#'
#' @details
#' The function will get the schema from the schema attribute of \code{x}. The
#' schema of the fields will be rebuilt (using the schema attribute of the
#' columns). When the data set does not contain any schema a default schema is
#' generated. 
#'
#' The function should ensure that date written using \code{csv_write} can be 
#' read again using \code{csv_read}. This should result in the same
#' \code{data.frame} as initially written (barring things like precision and
#' unsupported types).
#' 
#' @export
csv_write <- function(x, filename = "", 
    schema = paste0(tools::file_path_sans_ext(filename), ".schema.json"), 
    delimiter = ",", decimalChar = ".",
    ...) {
  if (filename == "" && missing(schema)) schema = stdout()
  schema_df <- build_schema(x)
  if (decimalChar != ".") schema_df <- set_decimalchar(schema_df, decimalChar, FALSE)
  delimiter_ok <- sapply(schema_df$fields, function(x) x$type != "number" || 
    is.null(x$decimalChar) || x$decimalChar != delimiter)
  delimiter_ok <- all(delimiter_ok)
  if (delimiter == decimalChar || !delimiter_ok)
    stop("There are fields for which the decimalChar equals the field ", 
      "delimiter. This is not allowed.")
  # Keep track of the fields that were originally character field and should
  # be quoted in the output
  quote <- which(sapply(x, is.character))
  # Format the fields (if necessary)
  for (col in names(x)) {
    s <- build_schema(x[[col]], col)
    if (decimalChar != ".") s <- set_decimalchar(s, decimalChar, FALSE)
    attr(x[[col]], "schema") <- s # setattr for data.table
    x[[col]] <- csv_format(x[[col]], s)
  }
  # How to write missing values
  nastrings <- if (!is.null(schema_df$missingValues)) schema_df$missingValues else ""
  na <- if (length(nastrings) > 0) nastrings[1] else ""
  # Write
  utils::write.table(x, file = filename, na = na, row.names = FALSE, 
    fileEncoding = "UTF-8", quote = quote, sep = delimiter, dec = ".", qmethod = "double")
  write_schema(schema_df, schema, pretty = TRUE)
}

set_decimalchar <- function(schema, value, all = TRUE) {
  if (!is.null(schema$fields)) {
    # assume table schema
    for (i in seq_along(schema$fields)) {
      if (schema$fields[[i]]$type == "number") {
        if (all || is.null(schema$fields[[i]]$decimalChar)) {
          schema$fields[[i]]$decimalChar <- value
        }
      }
    }
  } else {
    # assume field schema
    if (!is.null(schema$type) && schema$type == "number") {
      if (all || is.null(schema$decimalChar)) {
        schema$decimalChar <- value
      }
    }
  }
  schema
}

