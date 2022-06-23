
#' @export
csv_write <- function(x, filename, 
    schema = paste0(tools::file_path_sans_ext(filename), ".schema.json"), 
    ...) {
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
  write_schema(schema_df, schema)
}

#' @export
csv_format <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  type <- schema$type
  format_fun <- paste0("csv_format_", type)
  format_fun <- get(format_fun)
  format_fun(x, schema)
}

