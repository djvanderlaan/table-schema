

.PHONY: build check document install

build: document
	mkdir -p work && cd work && R CMD build ../

check: build
	cd work && R CMD check --as-cran `ls tableschema_* | sort | tail -n 1`

document:
	R -e "roxygen2::roxygenise()"

install: build
	R CMD INSTALL `ls work/tableschema_* | sort | tail -n 1` 

