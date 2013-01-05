all: init

.PHONY: all init test continuous_integration

init:
# Required packages: libgsl0-dev, liblapack-dev
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests $(CABAL_CONFIGURE_FLAGS)
	cabal build

test:
	dist/build/tests/tests

profile:
	cabal configure --enable-tests --enable-library-profiling --enable-executable-profiling
	cabal build

continuous_integration: init
	time cabal test
