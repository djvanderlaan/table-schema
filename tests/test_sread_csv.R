source("tests/helpers.R")

library(jsonlite)
for (file in list.files("R", "*.R", full.names = TRUE)) source(file)

dta <- sread_csv("tests/test.csv")
expect_equal(dta$string1, c("a", "b", "c", "", "f", "g"), 
  attributes = FALSE)
expect_equal(dta$integer1, c(1, -100, NA, 100, 0, 0),
  attributes = FALSE)
expect_equal(dta$boolean1, c(TRUE, FALSE, TRUE, TRUE, NA, FALSE),
  attributes = FALSE)
expect_equal(dta$number1, c(1.2, -1E-4, Inf, 1E4, NaN, NA),
  attributes = FALSE)
expect_equal(dta$number2, c(1.2, -0.001, 1100, -11000.4, NA, 0),
  attributes = FALSE)
expect_equal(dta$boolean2, c(TRUE, FALSE, TRUE, NA, FALSE, TRUE),
  attributes = FALSE)

dta <- sread_csv("tests/test.csv", 
  schema = "tests/test.schema.json")
expect_equal(dta$string1, c("a", "b", "c", "", "f", "g"), 
  attributes = FALSE)
expect_equal(dta$integer1, c(1, -100, NA, 100, 0, 0),
  attributes = FALSE)
expect_equal(dta$boolean1, c(TRUE, FALSE, TRUE, TRUE, NA, FALSE),
  attributes = FALSE)
expect_equal(dta$number1, c(1.2, -1E-4, Inf, 1E4, NaN, NA),
  attributes = FALSE)
expect_equal(dta$number2, c(1.2, -0.001, 1100, -11000.4, NA, 0),
  attributes = FALSE)
expect_equal(dta$boolean2, c(TRUE, FALSE, TRUE, NA, FALSE, TRUE),
  attributes = FALSE)

dta <- sread_csv("tests/test.csv", use_fread = TRUE, data.table = TRUE)
stopifnot(is(dta, "data.table"))
expect_equal(dta$string1, c("a", "b", "c", "", "f", "g"), 
  attributes = FALSE)
expect_equal(dta$integer1, c(1, -100, NA, 100, 0, 0),
  attributes = FALSE)
expect_equal(dta$boolean1, c(TRUE, FALSE, TRUE, TRUE, NA, FALSE),
  attributes = FALSE)
expect_equal(dta$number1, c(1.2, -1E-4, Inf, 1E4, NaN, NA),
  attributes = FALSE)
expect_equal(dta$number2, c(1.2, -0.001, 1100, -11000.4, NA, 0),
  attributes = FALSE)
expect_equal(dta$boolean2, c(TRUE, FALSE, TRUE, NA, FALSE, TRUE),
  attributes = FALSE)

dta <- sread_csv("tests/test2.csv", 
  schema = "tests/test.schema.json")
expect_equal(dta$string1, character(0),
  attributes = FALSE)
expect_equal(dta$integer1, integer(0),
  attributes = FALSE)
expect_equal(dta$boolean1, logical(0),
  attributes = FALSE)
expect_equal(dta$number1, numeric(0),
  attributes = FALSE)
expect_equal(dta$number2, numeric(0),
  attributes = FALSE)
expect_equal(dta$boolean2, logical(0),
  attributes = FALSE)

