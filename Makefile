help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help

TEST:=stack build --test --haddock --no-haddock-deps --pedantic --flag etc:yaml --flag etc:cli --flag etc:printer
test: ## Execute test suite with all compiler flags
	 $(TEST) etc:etc-testsuite
.PHONY: test

sdist: ## Build a release
	stack sdist
.PHONY: sdist

DIST_DIR:=$$(stack path --dist-dir)
SDIST_TAR:=$$(find $(DIST_DIR) -name "*.tar.gz" | tail -1)
untar_sdist: sdist
	tar xzf $(SDIST_TAR)
.PHONY: untar_sdist

SDIST_FOLDER:=$$(basename $(SDIST_TAR) .tar.gz)
INIT:=$$(stack init --force)
test_sdist: untar_sdist
	cd $(SDIST_FOLDER) && $(INIT) && $(TEST)
.PHONY: test_sdist

stylish_haskell_install:
	stack install stylish-haskell
.PHONY: stylish_haskell_install

STYLISH=stylish-haskell -i {} \;
stylish_haskell: stylish_haskell_install ## Normalize style of source files
	find src/ test/ -name "*.hs" -exec $(STYLISH) && git diff --exit-code
.PHONY: stylish_haskell

hlint_install:
	stack install hlint
.PHONY: hlint_install

hlint: hlint_install ## Execute linter
	hlint src/ test/
.PHONY: hlint

hlint_apply_refact: hlint_install ## Apply linter recomendations
	stack install apply-refact
.PHONY: hlint_apply_refact

HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint_apply_refact
	find src/ test/ -name "*.hs" -exec $(HLINT)
.PHONY: hlint_refactor
