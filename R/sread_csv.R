# TODO: handle missing values using na.strings
# TODO: handle ; separator  


#' @export
sread_csv <- function(filename, 
    schema = paste0(tools::file_path_sans_ext(filename), ".schema.json"), 
    use_fread = FALSE, ...) {
  if (is.character(schema)) schema <- jsonlite::read_json(schema)
  # Determine how we need to read each of the columns
  colclasses <- sapply(schema$fields, colclass)
  # Read
  if (use_fread) {
    if (!requireNamespace("data.table")) stop("In order to use ", 
        "'use_fread=TRUE' the data.table package needs to be installed.")
    dta <- data.table::fread(filename, 
      colClasses = colclasses, stringsAsFactors = FALSE, ...)
  } else {
    dta <- utils::read.csv(filename, colClasses = colclasses, 
      stringsAsFactors = FALSE, ...)
  }
  convert_using_schema(dta, schema)
}

