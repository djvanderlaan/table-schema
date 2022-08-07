library(tableschema)
source("helpers.R")


schema <- list(
  name = "integer",
  title = "A integer field",
  description = "A description",
  type = "integer"
)
res <- tableschema:::to_integer.character(c("10", "-100", "", NA), schema = schema)
expect_equal(res, c(10L, -100L, NA_integer_, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)
res <- tableschema:::to_integer.integer(c(10, -100, NA), schema = schema)
expect_equal(res, c(10L, -100L, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)

# === Method call
schema <- list(
  name = "integer",
  title = "A integer field",
  description = "A description",
  type = "integer"
)
res <- to_integer(c("10", "-100", "", NA), schema = schema)
expect_equal(res, c(10L, -100L, NA_integer_, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)
res <- to_integer(c(10, -100, NA), schema = schema)
expect_equal(res, c(10L, -100L, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)
res <- to_integer(c(10L, -100L, NA_integer_), schema = schema)
expect_equal(res, c(10L, -100L, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)

# === No Schema
schema <- list(
  type = "integer"
)
res <- tableschema:::to_integer.character(c("10", "-100", "", NA))
expect_equal(res, c(10L, -100L, NA_integer_, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)
res <- tableschema:::to_integer.integer(c(10, -100, NA))
expect_equal(res, c(10L, -100L, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)

# === Empty input
res <- to_integer(character(0))
expect_equal(res, integer(0), attributes = FALSE)
res <- to_integer(character(0))
expect_equal(res, integer(0), attributes = FALSE)

# === Invalid characters
expect_error(res <- to_integer(c("foo", "10", "10", NA)))

# === NA
schema <- list(
  name = "integer",
  missingValues = c("--")
)
res <- to_integer(c("10","--", "11", NA), schema)
expect_equal(res, c(10, NA, 11, NA), attributes = FALSE)
expect_error(res <- to_integer(c("10","---", "11", NA), schema))

# === Factor
schema <- list(
  type = "integer",
  categories = list(
    list(value = 1, label = "foo"),
    list(value = 2, label = "bar")
  )
)
res <- to_integer(factor(c("bar", "foo", NA), levels = c("foo", "bar")), 
  schema)
expect_equal(res, c(2, 1, NA), attributes = FALSE)
expect_error(res <- to_integer(
   factor(c("bar", "foo", NA), levels = c("foo", "bar", "foobar")), schema)
)


# =============================================================================
# csv_colclass

res <- csv_colclass_integer(list()) 
expect_equal(res, "integer")

# === NA
schema <- list(
  name = "integer",
  missingValues = c("--")
)
res <- csv_colclass_integer(schema)
expect_equal(res, "character")
