
#' Add required fields to the schema for an integer column
#'
#' @param schema should be a list.
#'
#' @return
#' Returns \code{schema} with the required fields added. 
#' 
#' @export
complete_schema_integer <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "integer"
  schema
}


#' Convert a vector to 'integer' using the specified schema
#' 
#' @param x the vector to convert.
#' @param schema the table-schema for the field.
#'
#' @details
#' When \code{schema} is missing a default schema is generated using
#' \code{\link{complete_schema_integer}}. 
#'
#' @return
#' Will return an \code{integer} vector with \code{schema} added as the 'schema'
#' attribute.
#' 
#' @export
to_integer <- function(x, schema = list()) {
  UseMethod("to_integer")
}

#' @export
to_integer.integer <- function(x, schema = list()) {
  schema <- complete_schema_integer(schema)
  structure(x, schema = schema)
}

#' @export
to_integer.numeric <- function(x, schema = list()) {
  schema <- complete_schema_integer(schema)
  # Need to check for rounding errors? Would round(x) be better? 
  structure(as.integer(round(x)), schema = schema)
}


#' @export
to_integer.character <- function(x, schema = list()) {
  schema <- complete_schema_integer(schema)
  # Consider "" as a NA
  na_values <- ""
  na <- x %in% na_values | is.na(x);
  res <- suppressWarnings(as.integer(x))
  invalid <- is.na(res) & !na
  if (any(invalid)) 
    stop("Invalid values found: '", x[utils::head(which(invalid), 1)], "'.")
  structure(res, schema = schema)
}


#' @rdname csv_colclass
#' @export
csv_colclass_integer <- function(schema = list()) {
  "integer"
}

#' @rdname csv_format
#' @export
csv_format_integer <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  as.integer(x)
}

