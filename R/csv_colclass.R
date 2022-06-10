#' @export
csv_colclass <- function(schema) {
  type <- schema$type
  fun <- paste0("csv_colclass_", type)
  if (!exists(fun)) stop(fun, " does not exist.")
  do.call(fun, list(schema))
}
