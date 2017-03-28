TEST:=stack build --test --haddock --no-haddock-deps --pedantic
test:
	 $(TEST) etc-spec:etc-spec-testsuite
.PHONY: test

sdist:
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
stylish_haskell: stylish_haskell_install
	find src/ test/ -name "*.hs" -exec $(STYLISH) && git diff --exit-code
.PHONY: stylish_haskell

hlint_install:
	stack install hlint
.PHONY: hlint_install

hlint: hlint_install
	hlint src/ test/
.PHONY: hlint

hlint_apply_refact: hlint_install
	stack install apply-refact
.PHONY: hlint_apply_refact

HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint_apply_refact
	find src/ test/ -name "*.hs" -exec $(HLINT)
.PHONY: hlint_refactor
