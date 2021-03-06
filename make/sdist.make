# -*- mode: Makefile; -*-
################################################################################

PROJECT_NAME := etc
PROJECT_VERSION := 0.4.1.1

STACK := stack --resolver nightly

SDIST_DIR_NAME     := $(PROJECT_NAME)-$(PROJECT_VERSION)
INTERNAL_SDIST_TAR := etc/$$($(STACK) path --dist-dir)/$(SDIST_DIR_NAME).tar.gz
PROJECT_SDIST_TAR  := out/$(SDIST_DIR_NAME).tar.gz

################################################################################

$(PROJECT_SDIST_TAR):
	$(STACK) sdist etc --pvp-bounds lower
	cp $(INTERNAL_SDIST_TAR) out

sdist: $(PROJECT_SDIST_TAR) ## Build a tar release in ./out folder
.PHONY: sdist

untar-sdist: sdist ## uncompress tar release in ./tmp folder
	@mkdir -p tmp
	@rm -rf tmp/$(SDIST_DIR_NAME) || true
	cp $(INTERNAL_SDIST_TAR) out
	tar xzf $(INTERNAL_SDIST_TAR)
	mv $(SDIST_DIR_NAME) tmp
.PHONY: untar-sdist

test-sdist: untar-sdist ## test release candidate
	cd tmp/$(SDIST_DIR_NAME) && $(STACK) init --solver --force && $(STACK) build --test --bench --haddock --no-run-benchmarks
.PHONY: test-sdist

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
