#' Try guess the best variable meta data for a variable
#'
#' @param x the variable for which the meta data has to be generated.
#' @param name the name of the variable.
#' 
#' @details
#' When \code{x} has a 'schema' attribute, that is used. 
#' 
#' @export
build_schema <- function(x, name = NULL) {
  UseMethod("build_schema")
}

#' @export
build_schema.numeric <- function(x, name = NULL) {
  res <- attr(x, "schema")
  if (!is.null(res)) {
    if (!missing(name)) res[["name"]] <- name
  } else {
    res <- list(
      type = "number"
    )
    if (!missing(name) && !is.null(name)) res[["name"]] <- name
  }
  res
}

#' @export
build_schema.integer <- function(x, name = NULL) {
  res <- attr(x, "schema")
  if (!is.null(res)) {
    if (!missing(name)) res[["name"]] <- name
  } else {
    res <- list(
      type = "integer"
    )
    if (!missing(name) && !is.null(name)) res[["name"]] <- name
  }
  res
}

#' @export
build_schema.logical <- function(x, name = NULL) {
  res <- attr(x, "schema")
  if (!is.null(res)) {
    if (!missing(name)) res[["name"]] <- name
  } else {
    res <- list(
      type = "boolean",
      trueValues = "TRUE",
      falseValues = "FALSE"
    )
    if (!missing(name) && !is.null(name)) res[["name"]] <- name
  }
  res
}

#' @export
build_schema.character <- function(x, name = NULL) {
  res <- attr(x, "schema")
  if (!is.null(res)) {
    if (!missing(name)) res[["name"]] <- name
  } else {
    res <- list(
      type = "string"
    )
    if (!missing(name) && !is.null(name)) res[["name"]] <- name
  }
  res
}

#' @export
build_schema.Date <- function(x, name = NULL) {
  res <- attr(x, "schema")
  if (!is.null(res)) {
    if (!missing(name)) res[["name"]] <- name
  } else {
    res <- complete_schema_date(list())
    if (!missing(name) && !is.null(name)) res[["name"]] <- name
  }
  res
}

#' @export
build_schema.default <- function(x, name = NULL) {
  res <- attr(x, "schema")
  if (!is.null(res)) {
    if (!missing(name)) res[["name"]] <- name
  } else {
    res <- list(
      type = "string"
    )
    if (!missing(name) && !is.null(name)) res[["name"]] <- name
  }
  res
}

#' @export
build_schema.data.frame <- function(x, name = NULL) {
  schema <- attr(x, "schema")
  if (is.null(schema)) schema <- list(name = name)
  fields <- vector("list", ncol(x))
  for (i in seq_along(x)) {
    fields[[i]] <- build_schema(x[[i]], names(x)[i])
  }
  schema$fields <- fields
  if (is.null(schema$missingValues)) schema$missingValues <- ""
  schema
}

