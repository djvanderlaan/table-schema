
#' @export
complete_schema_number <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "number"
  if (!exists("decimalChar", schema)) schema[["decimalChar"]] <- "."
  schema
}

#' @export
to_number <- function(x, schema = list()) {
  UseMethod("to_number")
}

#' @export
to_number.numeric <- function(x, schema = list()) {
  schema <- complete_schema_number(schema)
  structure(x, schema = schema)
}

#' @export
to_number.character <- function(x, schema = list()) {
  schema <- complete_schema_number(schema)
  # Consider "" as a NA
  na_values <- ""
  na <- x %in% na_values | is.na(x);
  if (!is.null(schema$groupChar)) 
    x <- gsub(schema$groupChar, "", x, fixed = TRUE)
  if (schema$decimalChar != ".") 
    x <- gsub(schema$decimalChar, ".", x, fixed = TRUE)
  res <- suppressWarnings(as.numeric(x))
  invalid <- is.na(res) & !na & !is.nan(res)
  if (any(invalid)) 
    stop("Invalid values found: '", x[utils::head(which(invalid), 1)], "'.")
  structure(res, schema = schema)
}

#' @export
colclass_number <- function(schema = list()) {
  schema <- complete_schema_number(schema)
  if (!is.null(schema$groupChar) || schema$decimalChar != ".") {
    "character"
  } else {
    "numeric"
  }
}

