.PHONY: install
install:
	stack --no-terminal --skip-ghc-check --copy-bins --test build

.PHONY: test
test:
	fregot test tests/rego                # Rego tests
	fregot test examples/ami_id           # AMI ID example
	fregot test examples/break_example    # Breakpoint example
	fspec -j2 --pretty-diff tests/golden  # Golden tests
