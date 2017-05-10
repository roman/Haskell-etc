src/Config.hs: resources/spec.yaml
	@echo "== Building configuration"
	./bin/build_config.sh

prepare_config: src/Config.hs ## Embed configuration from resources/spec.yaml in library
.PHONY: prepare_config

build: src/Config.hs ## Build application
	stack build
.PHONY: build

test: src/Config.hs ## Run tests
	stack test
.PHONY: test

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help
