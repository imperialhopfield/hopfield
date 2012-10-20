all: init

.PHONY: all init

init:
	cabal configure --enable-tests
	cabal install --only-dependencies
	cabal build
