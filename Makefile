all: init

.PHONY: all init

init:
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests
	cabal build
