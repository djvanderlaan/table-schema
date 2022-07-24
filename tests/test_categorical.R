library(tableschema)
source("helpers.R")

# =============================================================================
# Base type integer to_integer
schema <- list(
  type = "integer",
  categories = list(
    list(value = 11, label = "foo"),
    list(value = 22, label = "bar")
  )
)

x <- c("11", "22", "")
res <- to_integer(x, schema = schema)
expect_equal(res, factor(c(1,2,NA), levels = 1:2, labels = c("foo", "bar")), 
  attributes = FALSE)
x <- c(11, 22, NA)
res <- to_integer(x, schema = schema)
expect_equal(res, factor(c(1,2,NA), levels = 1:2, labels = c("foo", "bar")), 
  attributes = FALSE)

# Invalid values
x <- c("11", "222", "")
expect_error(to_integer(x, schema = schema))
x <- c(1, 22, NA)
expect_error(to_integer(x, schema = schema))

# Zero length
x <- character(0)
res <- to_integer(x, schema = schema)
expect_equal(res, factor(integer(0), levels = 1:2, labels = c("foo", "bar")), 
  attributes = FALSE)
x <- integer(0)
res <- to_integer(x, schema = schema)
expect_equal(res, factor(integer(0), levels = 1:2, labels = c("foo", "bar")), 
  attributes = FALSE)

# =============================================================================
# Base type string to_string
schema <- list(
  type = "string",
  categories = list(
    list(value = "11", label = "foo"),
    list(value = "22", label = "bar")
  )
)

x <- c("11", "22", NA)
res <- to_string(x, schema = schema)
expect_equal(res, factor(c(1,2,NA), levels = 1:2, labels = c("foo", "bar")), 
  attributes = FALSE)

# Invalid values
x <- c("11", "222", NA)
expect_error(to_string(x, schema = schema))

# Zero length
x <- character(0)
res <- to_string(x, schema = schema)
expect_equal(res, factor(integer(0), levels = 1:2, labels = c("foo", "bar")), 
  attributes = FALSE)

# =============================================================================
schema <- list(
  type = "integer",
  categories = list(
    list(value = 11, label = "foo"),
    list(value = 22, label = "bar")
  )
)

x <- factor(c(1,2,NA), levels = 1:2, labels = c("foo", "bar"))
res <- csv_format(x, schema)
expect_equal(res, c(11, 22, NA))
x <- c(11, 22, NA)
res <- csv_format(x, schema)
expect_equal(res, c(11, 22, NA))

# Invalid values
x <- factor(c(1,2,NA), levels = 1:2, labels = c("foo", "foobar"))
expect_error(csv_format(x, schema))
x <- c(11, 222222, NA)
expect_error(csv_format(x, schema))

# Zero length
x <- factor(numeric(0), levels = 1:2, labels = c("foo", "bar"))
res <- csv_format(x, schema)
expect_equal(res, integer(0))
x <- integer(0)
res <- csv_format(x, schema)
expect_equal(res, integer(0))

# =============================================================================
schema <- list(
  type = "string",
  categories = list(
    list(value = "f", label = "foo"),
    list(value = "b", label = "bar")
  )
)

x <- factor(c(1,2,NA), levels = 1:2, labels = c("foo", "bar"))
res <- csv_format(x, schema)
expect_equal(res, c("f", "b", NA))
x <- c("f", "b", NA)
res <- csv_format(x, schema)
expect_equal(res, c("f", "b", NA))

# Invalid values
x <- factor(c(1,2,NA), levels = 1:2, labels = c("foo", "foobar"))
expect_error(csv_format(x, schema))
x <- c("f", "bb", NA)
expect_error(csv_format(x, schema))

# Zero length
x <- factor(numeric(0), levels = 1:2, labels = c("foo", "bar"))
res <- csv_format(x, schema)
expect_equal(res, character(0))
x <- character(0)
res <- csv_format(x, schema)
expect_equal(res, character(0))

