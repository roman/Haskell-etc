src/Config.hs: resources/spec.yaml
	@echo "== Building configuration"
	./bin/build_config.sh

prepare_config: src/Config.hs
.PHONY: prepare_config

build: src/Config.hs
	stack build
.PHONY: build

test: src/Config.hs
	stack test
.PHONY: test
