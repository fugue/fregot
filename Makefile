FREGOT_VERSION=v$(shell sed -n '/^Version:/{s/.*: *//p}' fregot.cabal)

.PHONY: install
install:
	stack --no-terminal --skip-ghc-check install goldplate
	stack --no-terminal --skip-ghc-check --copy-bins --test build

.PHONY: test
test:
	fregot test tests/rego                        # Rego tests
	fregot test examples/ami_id                   # AMI ID example
	fregot test examples/break_example            # Breakpoint example
	goldplate -j2 --pretty-diff tests/golden      # Golden tests
	fregot capabilities | jq --sort-keys '.' | \
	  diff - extra/capabilities-master.json       # Capabilities doc

extra/capabilities-master.json: fregot.cabal
	fregot capabilities | jq --sort-keys '.' >$@
