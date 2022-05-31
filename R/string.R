
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
colclass_string <- function(schema = list()) {
  "character"
}

