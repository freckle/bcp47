all: setup build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) bcp47 --fast --pedantic --test
	stack build $(STACK_ARGUMENTS) bcp47-orphans --fast --pedantic --test

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint bcp47/library bcp47/tests bcp47-orphans/library bcp47-orphans/tests
	# stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: clean
clean:
	stack clean

.PHONY: check-nightly
check-nightly: STACK_ARGUMENTS=--stack-yaml stack-nightly.yaml --resolver nightly
check-nightly: setup build test
