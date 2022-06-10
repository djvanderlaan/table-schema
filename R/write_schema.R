

#' @export
write_schema <- function(schema, fn) {

  # Some fields should be stored as a vector even when they contain
  # only a single values; make sure that they are indeed stored as
  # a vector
  nounbox_fields <- c("trueValues", "falseValues")
  schema$fields[] <- lapply(schema$fields, function(f, nounbox) {
    nounbox <- intersect(nounbox, names(f))
    for (n in nounbox) f[[n]] <- I(f[[n]])
    f
  }, nounbox = nounbox_fields)
  # Write
  jsonlite::write_json(schema, fn, auto_unbox = TRUE)
}

