library(tableschema)
source("helpers.R")

dta <- data.frame(
    number = c(0.1, -100.21223212, 100.1020, 1E-8, NA),
    boolean = c(TRUE, FALSE, TRUE, NA, TRUE), 
    integer = c(NA, -100, 1150000, 1, 0),
    string = c("jan", "pier", NA, "joris", "corneel"),
    date = as.Date(c("2022-01-01", "1950-10-10", NA, "1920-12-31", "2001-10-20")),
    factor = factor(c(2,1,1,NA,2), levels = 1:2, labels = c("B", "A"))
  )

fn <- tempfile()
fn_schema <- tempfile()

# ===
# Write file and read back; should remain the same
csv_write(dta, fn, filename_schema = fn_schema)
dta2 <- csv_read(fn, schema = fn_schema)
expect_equal(dta, dta2, attributes = FALSE)

# ===
# Write some columns differenty than the default; still writing and 
# then reading should result in the same data.table
schema <- generate_schema(dta$date)
schema$format <- "%Y%m%d"
attr(dta$date, "schema") <- schema

schema <- generate_schema(dta$boolean)
schema$trueValues <- c("1")
schema$falseValues <- c("0")
attr(dta$boolean, "schema") <- schema

schema <- generate_schema(dta$number)
schema$decimalChar <- "|"
attr(dta$number, "schema") <- schema

csv_write(dta, fn, filename_schema = fn_schema)
dta2 <- csv_read(fn, schema = fn_schema)
expect_equal(dta, dta2, attributes = FALSE)

# ===
# data.table
csv_write(dta, fn, filename_schema = fn_schema)
dta2 <- csv_read(fn, schema = fn_schema, use_fread = TRUE, data.table = TRUE)
expect_equal(data.table::as.data.table(dta), dta2, attributes = FALSE)

# ===
# Empty dataset
csv_write(dta[FALSE, ], fn, filename_schema = fn_schema)
dta2 <- csv_read(fn, schema = fn_schema)
expect_equal(dta[FALSE,], dta2, attributes = FALSE)

# ===
# decimalChar should not equal the field delimiter
data(iris)
expect_error(csv_write(iris, decimalChar = ","))
expect_error(csv_write(iris, decimalChar = ".", delimiter = "."))
schema <- generate_schema(iris)
schema$fields[[1]]$decimalChar <- ","
expect_error(csv_write(iris, schema = schema))
expect_error(csv_write(iris, schema = schema, delimiter = "."))


# Cleanup
file.remove(fn)
file.remove(fn_schema)


