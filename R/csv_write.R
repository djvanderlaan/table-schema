
#' @export
csv_write <- function(x, filename, 
    schema = paste0(tools::file_path_sans_ext(filename), ".schema.json"), 
    ...) {
  for (col in names(x)) {
    s <- build_schema(x[[col]], col)
    attr(x[[col]], "schema") <- s # setattr for data.table
    x[[col]] <- csv_format(x[[col]], s)
  }
  s <- build_schema(x)
  utils::write.csv(x, file = filename, na = "", row.names = FALSE, 
    fileEncoding = "UTF-8")
  write_schema(s, schema)
}

#' @export
csv_format <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  type <- schema$type
  format_fun <- paste0("csv_format_", type)
  format_fun <- get(format_fun)
  format_fun(x, schema)
}

