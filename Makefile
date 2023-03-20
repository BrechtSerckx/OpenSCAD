SHELL := /usr/bin/env bash
PROJECT := OpenSCAD

.PHONY: clean
clean:
	rm -rf dist-newstyle result result-*

.PHONY: build
build:
	cabal build $(PROJECT)

.PHONY: test
test:
	cabal test $(PROJECT)

.PHONY: nix-build
nix-build:
	nix-build --no-out-link default.nix -A OpenSCAD.components

.PHONY: format
format:
	find src test -type f -name '*.hs' -exec ormolu --mode inplace {} \+
	find -type f -name '*.nix' -and -not -path './nix/sources.nix' -exec nixfmt {} \+
	cabal-fmt --inplace $(PROJECT).cabal

.PHONY: format-check
format-check:
	find src test -type f -name '*.hs' -exec ormolu --mode check {} \+
	find -type f -name '*.nix' -and -not -path './nix/sources.nix' -exec nixfmt --check {} \+
	cabal-fmt --check $(PROJECT).cabal

.PHONY: hlint
hlint:
	hlint src test

.PHONY: ci-cd
ci-cd: 
	act $(ACT_ARGS)

.PHONY: make check
check: format-check hlint
	cabal check
