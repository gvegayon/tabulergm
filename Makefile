PKG_NAME := $(shell grep '^Package:' DESCRIPTION | sed 's/^Package: //')
PKG_VERSION := $(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
PKG_TAR := $(PKG_NAME)_$(PKG_VERSION).tar.gz

.DEFAULT_GOAL := help

## help        : Show this help message
.PHONY: help
help:
	@echo "Available targets:"
	@grep '^## ' Makefile | sed 's/^## /  /'

## document    : Generate documentation using roxygen2
.PHONY: document
document:
	Rscript -e "devtools::document()"

## build       : Build the R package tarball
.PHONY: build
build: document
	R CMD build .

## install     : Install the package
.PHONY: install
install: build
	R CMD INSTALL $(PKG_TAR)

## check       : Run R CMD check on the package
.PHONY: check
check: build
	R CMD check --as-cran $(PKG_TAR)

## test        : Run the test suite
.PHONY: test
test:
	Rscript -e "tinytest::test_package('$(PKG_NAME)', at_home = TRUE)"

## readme      : Build README.md from README.Rmd (if it exists)
.PHONY: readme
readme:
	Rscript -e "devtools::build_readme()"

## clean       : Remove build artifacts
.PHONY: clean
clean:
	rm -f $(PKG_TAR)
	rm -rf $(PKG_NAME).Rcheck
