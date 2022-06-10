
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
  write.csv(x, file = filename, na = "", row.names = FALSE, 
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

#' @export
csv_format_string <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  # For a character we don't have to do anything; we can write as is
  x
}

#' @export
csv_format_boolean <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  if (is.logical(x) && ("TRUE" %in% schema$trueValues) && 
      ("FALSE" %in% schema$falseValues)) {
    # We can write as is as R writes TRUE/FALSE by default
    x
  } else {
    trueval <- utils::head(schema$trueValues, 1)
    falseval <- utils::head(schema$trueValues, 1)
    # When x is not logical; we let ifelse handle that
    ifelse(x, trueval, falseval)
  }
}

#' @export
csv_format_number <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  has_groupchar <- !is.null(schema$groupChar) && schema$groupChar != ""
  has_decimalchar <- !is.null(schema$decimalChar) && schema$decimalChar != "."
  x <- as.numeric(x)
  if (has_groupchar || has_decimalchar) {
    groupchar <- if (has_groupchar) schema$groupChar else ""
    decimalchar <- if (has_decimalchar) schema$decimalChar else "."
    formatC(x, big.mark = groupchar, decimal.mark = decimalchar)
  } else x
}

#' @export
csv_format_integer <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  as.integer(x)
}

