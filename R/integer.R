
complete_schema_integer <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "integer"
  schema
}

to_integer <- function(x, schema = list()) {
  UseMethod("to_integer")
}

to_integer.integer <- function(x, schema = list()) {
  schema <- complete_schema_integer(schema)
  structure(x, schema = schema)
}

to_integer.numeric <- function(x, schema = list()) {
  schema <- complete_schema_integer(schema)
  # Need to check for rounding errors? Would round(x) be better? 
  structure(round(x), schema = schema)
}


to_integer.character <- function(x, schema = list()) {
  schema <- complete_schema_integer(schema)
  # Consider "" as a NA
  na_values <- ""
  na <- x %in% na_values | is.na(x);
  res <- suppressWarnings(as.integer(x))
  invalid <- is.na(res) & !na
  if (any(invalid)) 
    stop("Invalid values found: '", x[head(which(invalid), 1)], "'.")
  structure(res, schema = schema)
}

colclass_integer <- function(schema = list()) {
  "integer"
}

