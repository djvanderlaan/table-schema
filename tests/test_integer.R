source("R/integer.R")
source("tests/helpers.R")


schema <- list(
  name = "integer",
  title = "A integer field",
  description = "A description",
  type = "integer"
)
res <- to_integer.character(c("10", "-100", "", NA), schema = schema)
expect_equal(res, c(10L, -100L, NA_integer_, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)
res <- to_integer.integer(c(10, -100, NA), schema = schema)
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
res <- to_integer.character(c("10", "-100", "", NA))
expect_equal(res, c(10L, -100L, NA_integer_, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)
res <- to_integer.integer(c(10, -100, NA))
expect_equal(res, c(10L, -100L, NA_integer_), attributes = FALSE)
expect_attribute(res, "schema", schema)

# === Empty input
res <- to_integer(integer(0))
expect_equal(res, integer(0), attributes = FALSE)
res <- to_integer(character(0))
expect_equal(res, integer(0), attributes = FALSE)

# === Invalid characters
expect_error(res <- to_integer(c("foo", "10", "10", NA)))



# =============================================================================
# colclass

res <- colclass_integer(list()) 
expect_equal(res, "integer")

