all: init

.PHONY: all init test continuous_integration

init:
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests
	cabal build

test:
	dist/build/tests/tests

continuous_integration: init
	cabal test
