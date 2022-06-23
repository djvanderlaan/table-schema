
#' Read table-scheme from JSON-file
#' 
#' @param filename the name of the file to read from
#'
#' @return
#' Return a \code{list} with the table-schema.
#' 
#' @export
read_schema <- function(filename) {
  schema <- jsonlite::read_json(filename, simplifyVector = TRUE, 
    simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  if (!is.null(schema$missingValues)) 
    schema$missingValues <- as.character(schema$missingValues)
  schema
}

