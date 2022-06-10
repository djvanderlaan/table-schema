
#' @export
complete_schema_string <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "string"
  schema
}

#' @export
to_string <- function(x, schema = list()) {
  UseMethod("to_string")
}

#' @export
to_string.character <- function(x, schema = list()) {
  schema <- complete_schema_string(schema)
  # Consider "" as a NA?
  structure(x, schema = schema)
}

#' @export
csv_colclass_string <- function(schema = list()) {
  "character"
}

#' @export
csv_format_string <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  # For a character we don't have to do anything; we can write as is
  x
}

