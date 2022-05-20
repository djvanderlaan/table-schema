
convert_using_schema <- function(dta, schema) {
  # Check columnnames
  colnames <- sapply(schema$fields, function(x) x$name)
  if (!all(names(dta) == colnames)) 
    stop("Column names of dta do not match those in the schema.")
  # Convert columns to correct type
  is_data_table <- is(dta, "data.table")
  for (i in seq_along(dta)) {
    fun <- paste0("to_", schema$fields[[i]]$type)
    stopifnot(exists(fun))
    col <- names(dta)[i]
    if (is_data_table) {
      dta[, (col) := do.call(fun, list(get(col), schema$fields[[i]]))]
    } else {
      dta[[i]] <- do.call(fun, list(dta[[i]], schema$fields[[i]]))
    }
  }
  if (is_data_table) {
    data.table::setattr(dta, "schema", schema)
  } else {
    attr(dta, "schema") <- schema
  }
  dta[]
}

