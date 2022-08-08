library(tableschema)
source("helpers.R")

dta <- data.frame(
    number = c(0.1, -100.21223212, 100.1020, 1E-8, NA),
    boolean = c(TRUE, FALSE, TRUE, NA, TRUE), 
    integer = c(NA, -100, 1150000, 1, 0),
    string = c("jan", "pier", NA, "joris", "corneel"),
    date = as.Date(c("2022-01-01", "1950-10-10", NA, "1920-12-31", "2001-10-20"))
  )

fn <- tempfile()
fn_schema <- tempfile()

# === 
# Default missing values are ""
csv_write(dta, fn, fn_schema)
d <- read.csv(fn, colClasses = "character")
expect_equal(d$number[5], "")
expect_equal(d$boolean[4], "")
expect_equal(d$integer[1], "")
expect_equal(d$string[3], "")

s <- read_schema(fn_schema)
expect_equal(s$missingValues, "")

d <- csv_read(fn, fn_schema)
expect_equal(d, dta, attributes = FALSE)

# ===
# Non-default value; should take first one
schema <- generate_schema(dta)
schema$missingValues <- c("FOO", "BAR")
attr(dta, "schema") <- schema
csv_write(dta, fn, fn_schema)
d <- read.csv(fn, colClasses = "character")
expect_equal(d$number[5], "FOO")
expect_equal(d$boolean[4], "FOO")
expect_equal(d$integer[1], "FOO")
expect_equal(d$string[3], "FOO")

s <- read_schema(fn_schema)
expect_equal(s$missingValues, schema$missingValues)

d <- csv_read(fn, fn_schema)
expect_equal(d, dta, attributes = FALSE)

# ===
# Non-default value; should take first one
schema <- generate_schema(dta)
schema$missingValues <- character(0)
attr(dta, "schema") <- schema
csv_write(dta, fn, fn_schema)
d <- read.csv(fn, colClasses = "character")
expect_equal(d$number[5], "")
expect_equal(d$boolean[4], "")
expect_equal(d$integer[1], "")
expect_equal(d$string[3], "")

s <- read_schema(fn_schema)
expect_equal(s$missingValues, schema$missingValues)

# The to_number functions etc will convert an empty value to NA, but
# to_string should not. 
d <- csv_read(fn, fn_schema)
expect_equal(d$string[3], "")

# ===
# Cleanup
file.remove(fn)
file.remove(fn_schema)

