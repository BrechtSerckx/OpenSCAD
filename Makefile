SHELL := /usr/bin/env bash

ifdef PACKAGE_SET
	NIX_ARGS += --argstr packageSet $(PACKAGE_SET)
endif

.PHONY: info
info:
	echo "NIX_ARGS: '$(NIX_ARGS)'"

.PHONY: clean
clean:
	rm -rf dist-newstyle result result-* bench/out/*

.PHONY: build
build:
	cabal build OpenSCAD

.PHONY: test
test:
	cabal test OpenSCAD

.PHONY: bench
bench:
	cabal bench

.PHONY: nix-build
nix-build:
	nix-build $(NIX_ARGS) --no-out-link default.nix -A OpenSCAD.components

.PHONY: format
format:
	find src test -type f -name '*.hs' -exec ormolu --mode inplace {} \+
	find -type f -name '*.nix' -and -not -path './nix/sources.nix' -exec nixfmt {} \+
	cabal-fmt --inplace OpenSCAD.cabal

.PHONY: format-check
format-check:
	find src test -type f -name '*.hs' -exec ormolu --mode check {} \+
	find -type f -name '*.nix' -and -not -path './nix/sources.nix' -exec nixfmt --check {} \+
	cabal-fmt --check OpenSCAD.cabal

.PHONY: hlint
hlint:
	hlint src test

.PHONY: ci-cd
ci-cd: 
	act $(ACT_ARGS)

.PHONY: check
check: format-check hlint
	cabal check

.PHONY: haddock
haddock: 
	cabal haddock

.PHONY: doctest
doctest: 
	doctest
