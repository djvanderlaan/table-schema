
complete_schema_date <- function(schema) {
  if (!exists("type", schema)) schema[["type"]] <- "date"
  if (!exists("format", schema)) schem[["format"]] <- "default"
  schema
}

str_to_date <- function(x, schema = list()) {
  schema <- complete_schema_date(schema)
  # extract format
  format <- if (exists("format", schema)) schema$format else "default"
  # empty fields are NA
  x[x == ""] <- NA
  # convert
  if (format == "default") {
    res <- as.Date(x)
  } else if (format == "any") {
    stop("Unsupported")
  } else {
    res <- as.Date(x, format = format)
  }
  structure(res, schema = schema)
}


if (exists("TEST") && TEST) {
  schema <- list(
    name = "date",
    title = "A date field",
    description = "A description",
    type = "date",
    format = "default"
  )
  str_to_date(c("2020-01-01", "2022-05-06", ""), schema = schema)

  schema <- list(
    name = "date",
    title = "A date field",
    description = "A description",
    type = "date",
    format = "%d-%m-%Y"
  )
  str_to_date(c("1-1-2020", "6-5-2022", ""), schema = schema)

}


