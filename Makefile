all: init

.PHONY: all init

init:
	cabal install --only-dependencies
	cabal configure --enable-tests
	cabal install --only-dependencies
	cabal build
