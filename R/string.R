
complete_schema_string <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "string"
  schema
}

to_string <- function(x, schema = list()) {
  UseMethod("to_string")
}

to_string.character <- function(x, schema = list()) {
  schema <- complete_schema_string(schema)
  # Consider "" as a NA?
  structure(x, schema = schema)
}

colclass_string <- function(schema = list()) {
  "character"
}

