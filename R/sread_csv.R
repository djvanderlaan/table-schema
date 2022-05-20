sread_csv <- function(filename, schema,
    schema_filename = paste0(filename, ".schema.json"), 
    use_fread = FALSE, ...) {
  if (missing(schema)) schema <- jsonlite::read_json(meta_filename)
  # Determine how we need to read each of the columns
  colclasses <- sapply(schema$fields, colclass)
  colnames <- sapply(schema$fields, function(x) x$name)
  # Read
  if (use_fread) {
    if (!require(data.table)) stop("In order to use 'use_fread=TRUE'", 
      " the data.table package needs to be installed.")
    dta <- data.table::fread(filename, 
      colClasses = colclasses, stringsAsFactors = FALSE, ...)
  } else {
    dta <- read.csv(filename, colClasses = colclasses, 
      stringsAsFactors = FALSE, ...)
  }
  stopifnot(names(dta) == colnames)
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


