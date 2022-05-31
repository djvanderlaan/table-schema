library(tableschema)
source("helpers.R")

schema <- list(
  name = "string",
  title = "A string field",
  description = "A description",
  type = "string"
)
res <- tableschema:::to_string.character(c("a", "b", "", NA), schema = schema)
expect_equal(res, c("a", "b", "", NA), attributes = FALSE)
expect_attribute(res, "schema", schema)

# === Method call
schema <- list(
  name = "string",
  title = "A string field",
  description = "A description",
  type = "string"
)
res <- to_string(c("a", "b", "", NA), schema = schema)
expect_equal(res, c("a", "b", "", NA), attributes = FALSE)
expect_attribute(res, "schema", schema)

# === No Schema
schema <- list(
  type = "string"
)
res <- to_string(c("a", "b", "", NA), schema = schema)
expect_equal(res, c("a", "b", "", NA), attributes = FALSE)
expect_attribute(res, "schema", schema)

# === Empty input
res <- to_string(character(0))
expect_equal(res, character(0), attributes = FALSE)

# =============================================================================
# colclass

res <- colclass_string(list()) 
expect_equal(res, "character")

