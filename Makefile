.PHONY: install
install:
	stack --no-terminal --skip-ghc-check --copy-bins --test build

.PHONY: test
test:
	fregot test tests/rego                # Rego tests
	fregot test examples/                 # Examples
	fspec -j2 --pretty-diff tests/golden  # Golden tests
