
#' Convert field to factor based on schema
#' 
#' @param x vector with field to convert
#' @param schema the schema of the field
#'
#' @details
#' The information on levels is contained in the 'categories' field of the
#' field schema. This should be a vector with objects with the fields
#' 'name' and 'title'. 'name' is the value of the field in the vector and 
#' 'title' is name name of the level. 
#'
#' @examples
#' schema <- list(
#'   name = "field1",
#'   type = "string",
#'   categories = list(
#'     list(name = "M", title= "Male"),
#'     list(name = "F", title= "Female")
#'   )
#' )
#' to_factor(c("F", "M", "F"), schema)
#'
#' schema <- list(
#'   name = "field2",
#'   type = "integer",
#'   categories = list(
#'     list(name = 0, title= "North"),
#'     list(name = 1, title= "South"),
#'     list(name = 2, title= "East"),
#'     list(name = 3, title= "West")
#'   )
#' )
#' to_factor(1:3, schema)
#'
#' @export
to_factor <- function(x, schema) {
  if (is.null(schema$categories)) {
    warning("Schema does not have categories. Returning original vector.")
    return(x)
  }
  levels <- sapply(schema$categories, function(x) x$name)
  labels <- sapply(schema$categories, function(x) x$title)
  ok <- x %in% levels | is.na(x)
  if (!all(ok)) {
    wrong <- unique(x[!ok])
    wrong <- paste0("'", wrong, "'")
    if (length(wrong) > 5) 
      wrong <- c(utils::head(wrong, 5), "...")
    stop("Invalid values found in x: ", paste0(wrong, collapse = ","))
  }
  x <- factor(x, levels = levels, labels = labels)
  structure(x, schema = schema)
}


#' @export
csv_format_categorical <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  if (is.null(schema$categories)) stop("the categories element is missing ", 
    "from the field schema: x is not a categorical field.")
  # Convert the labels back to values
  values <- sapply(schema$categories, function(x) x$name)
  labels <- sapply(schema$categories, function(x) x$title)
  if (is.factor(x)) {
    m <- match(x, labels)
    ok <- is.na(x) | !is.na(m)
    x <- values[m]
  } else {
    # TODO: handle missing values?
    ok <- x %in% values | is.na(x)
  }
  if (!all(ok)) {
    wrong <- unique(x[!ok])
    wrong <- paste0("'", wrong, "'")
    if (length(wrong) > 5) 
      wrong <- c(utils::head(wrong, 5), "...")
    stop("Invalid values found in x: ", paste0(wrong, collapse = ","))
  }
  x
}
