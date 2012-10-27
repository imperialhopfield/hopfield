all: init

.PHONY: all init continuous_integration

init:
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests
	cabal build

continuous_integration: init
	cabal test
