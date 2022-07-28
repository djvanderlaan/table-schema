library(tableschema)
source("helpers.R")

dta <- csv_read("test.csv")
expect_equal(dta$string1, c("a", "b", "c", NA, "f", "g"), 
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
expect_equal(dta$date1, as.Date(c("2020-01-01", "2022-01-12", NA, 
    "1950-10-10", "1920-12-10", "2002-02-20")),
  attributes = FALSE)
expect_equal(dta$factor1, factor(c(1,2,1,3,NA,3), levels = 1:4,
  labels = c("Female", "Male", "Other",  "Not given")), attributes = FALSE)
expect_equal(levels(dta$factor1), c("Female", "Male", "Other",  "Not given"))

dta <- csv_read("test.csv", 
  schema = "test.schema.json", to_factor = FALSE)
expect_equal(dta$string1, c("a", "b", "c", NA, "f", "g"), 
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
expect_equal(dta$date1, as.Date(c("2020-01-01", "2022-01-12", NA, 
    "1950-10-10", "1920-12-10", "2002-02-20")),
  attributes = FALSE)
expect_equal(dta$factor1, c(1,2,1,3,NA,3), attributes = FALSE)

dta <- csv_read("test.csv", use_fread = TRUE, data.table = TRUE)
stopifnot(is(dta, "data.table"))
expect_equal(dta$string1, c("a", "b", "c", NA, "f", "g"), 
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
expect_equal(dta$date1, as.Date(c("2020-01-01", "2022-01-12", NA, 
    "1950-10-10", "1920-12-10", "2002-02-20")),
  attributes = FALSE)
expect_equal(dta$factor1, factor(c(1,2,1,3,NA,3), levels = 1:4,
  labels = c("Female", "Male", "Other",  "Not given")), attributes = FALSE)
expect_equal(levels(dta$factor1), c("Female", "Male", "Other",  "Not given"))

dta <- csv_read("test2.csv", 
  schema = "test.schema.json")
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
expect_equal(dta$date1, as.Date(character(0)),
  attributes = FALSE)
expect_equal(dta$factor1, factor(numeric(0), levels = 1:4,
  labels = c("Female", "Male", "Other",  "Not given")), attributes = FALSE)
expect_equal(levels(dta$factor1), c("Female", "Male", "Other",  "Not given"))

# === ; separator
txt <- "col1;col2\n102.2;20"
schema <- list(fields=list(
    list(name="col1",type="number"), 
    list(name="col2",type="integer")
  ))
dta <- csv_read(textConnection(txt), delimiter = ";", schema = schema)
expect_equal(dta$col1, 102.2, attributes = FALSE)
expect_equal(dta$col2, 20L, attributes = FALSE)
dta <- csv_read(txt, delimiter = ";", schema = schema, use_fread = TRUE)
expect_equal(dta$col1, 102.2, attributes = FALSE)
expect_equal(dta$col2, 20L, attributes = FALSE)

# TODO: test decimalChar
txt <- "col1;col2;col3\n102,2;20;-10.1"
schema <- list(fields=list(
    list(name="col1", type="number", groupChar = "."), 
    list(name="col2", type="integer"),
    list(name="col3", type="number", decimalChar=".")
  ))
dta <- csv_read(textConnection(txt), delimiter = ";", schema = schema, 
  decimalChar = ",")
expect_equal(dta$col1, 102.2, attributes = FALSE)
expect_equal(dta$col2, 20L, attributes = FALSE)
expect_equal(dta$col3, -10.1, attributes = FALSE)

dta <- csv_read(txt, delimiter = ";", schema = schema, use_fread = TRUE, 
  decimalChar = ",")

expect_equal(dta$col1, 102.2, attributes = FALSE)
expect_equal(dta$col2, 20L, attributes = FALSE)
expect_equal(dta$col3, -10.1, attributes = FALSE)
