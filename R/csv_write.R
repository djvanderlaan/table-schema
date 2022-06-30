
#' Write data to CSV-file and the table schema to a JSON file
#' 
#' @param x the dataset to write to file. 
#' @param filename the name of the CSV-file. If empty string the contents are
#'   written to the console.
#' @param schema the name fo the JSON-file to which the schema should be
#'   written. If missing and \code{filename} is an empty string the output is 
#'   written to the console.
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
    ...) {
  if (filename == "" && missing(schema)) schema = stdout()
  schema_df <- build_schema(x)
  # Keep track of the fields that were originally character field and should
  # be quoted in the output
  quote <- which(sapply(x, is.character))
  # Format the fields (if necessary)
  for (col in names(x)) {
    s <- build_schema(x[[col]], col)
    attr(x[[col]], "schema") <- s # setattr for data.table
    x[[col]] <- csv_format(x[[col]], s)
  }
  # How to write missing values
  nastrings <- if (!is.null(schema_df$missingValues)) schema_df$missingValues else ""
  na <- if (length(nastrings) > 0) nastrings[1] else ""
  # Write
  utils::write.csv(x, file = filename, na = na, row.names = FALSE, 
    fileEncoding = "UTF-8", quote = quote)
  write_schema(schema_df, schema, pretty = TRUE)
}

