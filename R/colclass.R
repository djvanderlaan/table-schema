colclass <- function(schema) {
  type <- schema$type
  fun <- paste0("colclass_", type)
  if (!exists(fun)) stop(fun, " does not exist.")
  do.call(fun, list(schema))
}
