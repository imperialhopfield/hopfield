all: init

.PHONY: all init test continuous_integration

init:
# Required packages: libgsl0-dev, liblapack-dev
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests $(CABAL_CONFIGURE_FLAGS)
	cabal build

test:
	dist/build/tests/tests

continuous_integration: init
	time cabal test
