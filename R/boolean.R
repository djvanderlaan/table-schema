
#' @export
complete_schema_boolean <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "boolean"
  if (!exists("trueValues", schema))
    schema[["trueValues"]] <- c("true", "TRUE", "True", "1")
  if (!exists("falseValues", schema))
    schema[["falseValues"]] <- c("false", "FALSE", "False", "0")
  schema
}

#' @export
to_boolean <- function(x, schema = list()) {
  UseMethod("to_boolean")
}

#' @export
to_boolean.integer <- function(x, schema = list()) {
  schema <- complete_schema_boolean(schema)
  true_values <- suppressWarnings(as.integer(schema$trueValues))
  if (any(is.na(true_values))) 
    stop("Not all falseValues in schema are integer.")
  false_values <- suppressWarnings(as.integer(schema$falseValues))
  if (any(is.na(false_values))) 
    stop("Not all falseValues in schema are integer.")
  # Handle the easy fastest case of int to logical conversion
  # this is possible if false == 0
  if (length(false_values) == 1 && false_values == 0L) {
    res <- as.logical(x)
    if (!all(x %in% c(false_values, true_values, NA))) 
      warning("Invalid trueValues in x.")
    structure(res, schema = schema)
  } else {
    s1  <- x %in% true_values
    s0 <- x %in% false_values
    res <- ifelse(s1, TRUE, NA)
    res[s0] <- FALSE
    invalid <- !(s0 | s1 | is.na(x))
    if (any(invalid)) 
      stop("Invalid values found: '", x[utils::head(which(invalid), 1)], "'.")
    structure(res, schema = schema)
  }
}

#' @export
to_boolean.character <- function(x, schema = list()) {
  schema <- complete_schema_boolean(schema)
  # Unless "" is a true of false value we will consider it a missing value
  na_values <- setdiff("", c(schema$trueValues, schema$falseValues))
  if (length(na_values)) x[x %in% na_values] <- NA
  s1  <- x %in% schema$trueValues
  s0 <- x %in% schema$falseValues
  res <- ifelse(s1, TRUE, NA)
  res[s0] <- FALSE
  invalid <- !(s0 | s1 | is.na(x))
  if (any(invalid)) 
    stop("Invalid values found: '", x[utils::head(which(invalid), 1)], "'.")
  structure(res, schema = schema)
}

#' @export
to_boolean.logical <- function(x, schema = list()) {
  schema <- complete_schema_boolean(schema)
  structure(x, schema = schema)
}


#' @export
csv_colclass_boolean <- function(schema = list()) {
  schema <- complete_schema_boolean(schema)
  res <- "character"
  if (length(schema$trueValues) == 1 && length(schema$falseValues) == 1) {
    if (schema$trueValues == "TRUE" && schema$falseValues == "FALSE")
      res <- "logical"
    if (schema$trueValues == "True" && schema$falseValues == "False")
      res <- "logical"
    if (schema$trueValues == "true" && schema$falseValues == "false")
      res <- "logical"
    if (schema$trueValues == "1" && schema$falseValues == "0")
      res <- "integer"
  }
  res
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

