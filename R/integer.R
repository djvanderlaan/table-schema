
#' Generate field schema for an integer field
#'
#' @param name name of the field
#' @param description description of the field
#' @param ... additional custom fields to add to the field schema.
#'
#' @return 
#' A list with a least the fields "name" and "type".
#'
#' @examples
#' x <- 1:4
#' schema(x) <- schema_integer("field", "A logical field")
#'
#' @export
schema_integer <- function(name, description, ...) {
  res <- list(name = name, type = "integer")
  if (!missing(description) && !is.null(description)) 
    res$description <- description
  c(res, list(...))
}


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
#' @param to_factor convert to factor if the schema has a categories
#'   field. 
#' @param ... passed on to other methods.
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
to_integer <- function(x, schema = list(), to_factor = TRUE, ...) {
  UseMethod("to_integer")
}

#' @export
to_integer.integer <- function(x, schema = list(), to_factor = TRUE, ...) {
  schema <- complete_schema_integer(schema)
  # Handle categories
  if (to_factor && !is.null(schema$categories)) 
    x <- to_factor(x, schema)
  structure(x, schema = schema)
}

#' @export
to_integer.numeric <- function(x, schema = list(), to_factor = TRUE, ...) {
  schema <- complete_schema_integer(schema)
  # Need to check for rounding errors? Would round(x) be better? 
  x <- as.integer(round(x))
  # Handle categories
  if (to_factor && !is.null(schema$categories)) 
    x <- to_factor(x, schema)
  structure(x, schema = schema)
}

#' @export
to_integer.factor <- function(x, schema = list(), to_factor = TRUE, ...) {
  schema <- complete_schema_integer(schema)
  # Check if levels of x match those in the schema
  if (is.null(schema$categories)) 
    stop("schema does not define categories while x is of type factor.")
  labels <- sapply(schema$categories, function(x) x$label)
  if (!isTRUE(all.equal(labels, levels(x))))
    stop("Levels of x do not match the categories defined in the schema.")
  if (!to_factor) stop("to_factor = FALSE is not supported when x is a factor.")
  # We can return x
  structure(x, schema = schema)
}


#' @export
to_integer.character <- function(x, schema = list(), to_factor = TRUE, ...) {
  schema <- complete_schema_integer(schema)
  # Consider "" as a NA
  na_values <- if (!is.null(schema$missingValues)) schema$missingValues else ""
  na <- x %in% na_values | is.na(x);
  x[x %in% na_values] <- NA
  res <- suppressWarnings(as.integer(x))
  invalid <- is.na(res) & !na
  if (any(invalid)) 
    stop("Invalid values found: '", x[utils::head(which(invalid), 1)], "'.")
  # Handle categories
  if (to_factor && !is.null(schema$categories)) 
    res <- to_factor(res, schema)
  structure(res, schema = schema)
}


#' @rdname csv_colclass
#' @export
csv_colclass_integer <- function(schema = list(), ...) {
  # When there are specific strings that encode a missing values we have to
  # read the field as character; otherwise we can leave the conversion to
  # integer to the csv reader.
  if (!is.null(schema$missingValues)) "character" else "integer"
}

#' @rdname csv_format
#' @export
csv_format_integer <- function(x, schema = tableschema::schema(x)) {
  if (!is.null(schema$categories)) {
    # We are dealing with a categorical variable that is stored as 
    # an integer
    x <- csv_format_categorical(x, schema)
  }
  as.integer(x)
}

