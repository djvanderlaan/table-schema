#' @export 
complete_schema_date <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "date"
  schema
}

#' @export
to_date <- function(x, schema = list()) {
  UseMethod("to_date")
}

##' @export
to_date.integer <- function(x, schema = list()) {
  # When we get an integer or numeric; assume date was accidentally read as 
  # numeric, e.g. when date = 20200101 or 01012020-> convert to character and 
  # convert
  to_date(sprintf("%08d", x))
}

##' @export
to_date.numeric <- function(x, schema = list()) {
  # When we get an integer or numeric; assume date was accidentally read as 
  # numeric, e.g. when date = 20200101 or 01012020-> convert to character and 
  # convert
  to_date(sprintf("%08d", x))
}


#' @export
to_date.character <- function(x, schema = list()) {
  schema <- complete_schema_date(schema)
  # Consider "" as a NA
  x[x == ""] <- NA
  na <- is.na(x);
  if (is.null(schema$format) || schema$format == "default") {
    res <- as.Date(x, format = "%Y-%m-%d")
  } else if (schema$format == "any") {
    res <- as.Date(x)
  } else {
    res <- as.Date(x, format = schema$format)
  }
  invalid <- is.na(res) & !na
  if (any(invalid)) 
    stop("Invalid values found: '", x[utils::head(which(invalid), 1)], "'.")
  structure(res, schema = schema)
}

#' @export
csv_colclass_date <- function(schema = list()) {
  "character"
}

#' @export
csv_format_date <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  if (is.null(schema$format) || schema$format == "default" || 
      schema$format == "any") {
    format <- "%Y-%m-%d"
  } else {
    format <- schema$format
  }
  format(x, format = format)
}

