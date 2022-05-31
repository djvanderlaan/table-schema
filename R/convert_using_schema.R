
#' @export
convert_using_schema <- function(dta, schema) {
  # Check columnnames
  colnames <- sapply(schema$fields, function(x) x$name)
  if (!all(names(dta) == colnames)) 
    stop("Column names of dta do not match those in the schema.")
  # Convert columns to correct type
  is_data_table <- methods::is(dta, "data.table")
  for (i in seq_along(dta)) {
    fun <- paste0("to_", schema$fields[[i]]$type)
    stopifnot(exists(fun))
    fun <- get(fun)
    col <- names(dta)[i]
    if (is_data_table) {
      data.table::set(dta, j = col, value = fun(dta[[col]], schema$fields[[i]]))
    } else {
      dta[[i]] <- fun(dta[[i]], schema$fields[[i]])
    }
  }
  if (is_data_table) {
    data.table::setattr(dta, "schema", schema)
  } else {
    attr(dta, "schema") <- schema
  }
  dta[]
}

