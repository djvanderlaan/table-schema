# categorical is not a separate field type in table schema. Instead, for each
# regular type it is possible to have a categories field. This defines a fixes
# set of valid values for the type and assigns each value a label. 


# Convert field to factor based on schema
# 
# @param x vector with field to convert
# @param schema the schema of the field
#
# @details
# The information on levels is contained in the 'categories' field of the
# field schema. This should be a vector with objects with the fields
# 'value' and 'label'. 'value' is the value of the field in the vector and 
# 'label' is name of the level. 
#
# @examples
# schema <- list(
#   name = "field1",
#   type = "string",
#   categories = list(
#     list(value = "M", label = "Male"),
#     list(value = "F", label = "Female")
#   )
# )
# to_factor(c("F", "M", "F"), schema)
#
# schema <- list(
#   name = "field2",
#   type = "integer",
#   categories = list(
#     list(value = 0, label = "North"),
#     list(value = 1, label = "South"),
#     list(value = 2, label = "East"),
#     list(value = 3, label = "West")
#   )
# )
# to_factor(1:3, schema)
#
to_factor <- function(x, schema) {
  if (is.null(schema$categories)) {
    warning("Schema does not have categories. Returning original vector.")
    return(x)
  }
  levels <- sapply(schema$categories, function(x) x$value)
  labels <- sapply(schema$categories, function(x) x$label)
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


csv_format_categorical <- function(x, schema = attr(x, "schema")) {
  if (is.null(schema)) schema <- build_schema(x)
  if (is.null(schema$categories)) stop("the categories element is missing ", 
    "from the field schema: x is not a categorical field.")
  # Convert the labels back to values
  values <- sapply(schema$categories, function(x) x$value)
  labels <- sapply(schema$categories, function(x) x$label)
  # TODO: handle case when label or value fields are missing
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
