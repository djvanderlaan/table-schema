library(jsonlite)
source("read_date.R")
source("read_boolean")

schema <- read_json("example.json")

types <- sapply(schema$fields, function(x) x$type)

schema_types <- c("integer", "number", "string", "boolean", "date")
schema_toread <- c("integer", "numeric", "character", "character", "character")

toread <- schema_toread[match(types, schema_types)]

dta <- read.csv("example.csv", colClasses = toread, 
  stringsAsFactors = FALSE, na.strings = "")

for (i in seq_along(schema$fields)) {
  f <- schema$fields[[i]]
  if (f$type == "boolean") {
    if (is.character(dta[[i]])) 
      dta[[i]] <- str_to_boolean(dta[[i]], f)
  } else if (f$type == "date") {
    if (is.character(dta[[i]])) 
      dta[[i]] <- str_to_date(dta[[i]], f)
  }
}



for (file in list.files("R", "*.R", full.names = TRUE)) source(file)

schema <- jsonlite::read_json("tests/test.csv.schema.json")

colclasses <- sapply(schema$fields, colclass)
colnames <- sapply(schema$fields, function(x) x$name)

dta <- read.csv("tests/test.csv", colClasses = colclasses, stringsAsFactors = FALSE)

stopifnot(names(dta) == colnames)

for (i in seq_along(dta)) {
  fun <- paste0("to_", schema$fields[[i]]$type)
  stopifnot(exists(fun))
  dta[[i]] <- do.call(fun, list(dta[[i]], schema$fields[[i]]))
}


