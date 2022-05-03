
complete_schema_boolean <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "boolean"
  schema
}

str_to_boolean <- function(x, schema = list()) {
  schema <- complete_schema_boolean(schema)
  true_values <- c("yes", "y", "true", "t", "1", "YES", "Y", "TRUE", "T")
  false_values <- c("no", "n", "false", "f", "0", "NO", "N", "FALSE", "F")
  res <- ifelse(x %in% true_values, TRUE, NA)
  res[x %in% false_values] <- FALSE
  structure(res, schema = schema)
}


if (exists("TEST") && TEST) {

  schema <- list(
    name = "boolean",
    title = "A boolean field",
    description = "A description",
    type = "boolean"
  )
  str_to_boolean(c("T", "F", ""), schema = schema)

  schema <- list(
    name = "boolean",
    title = "A boolean field",
    description = "A description",
    type = "boolean"
  )
  str_to_boolean(c("1", "0", ""), schema = schema)

}


