

#' @export
read_schema <- function(fn) {
  jsonlite::read_json(fn, simplifyVector = TRUE, simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE)
}

