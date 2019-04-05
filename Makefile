#------------------------------------------------#
# Makefile Settings -----------------------------#
#------------------------------------------------#

SHELL=bash

.EXPORT_ALL_VARIABLES:

#------------------------------------------------#
# Makefile Variables ----------------------------#
#------------------------------------------------#

Cabal ?=cabal

Open ?=xdg-open

#------------------------------------------------#

CabalOptions ?=

HaddockDirectory ?=./share/doc

#------------------------------------------------#
# Makefile Targets ------------------------------#
#------------------------------------------------#

#================================================#
# Standard Targets:

build:

	@printf "\n%s\n" "========================================"

	$(Cabal) new-build $(CabalOptions) all

	@printf "\n%s\n" "========================================"

.PHONY: build

#------------------------------------------------#

check:

	@printf "\n%s\n" "========================================"

	$(Cabal) new-test --enable-tests $(CabalOptions) all

	@printf "\n%s\n" "========================================"

.PHONY: check

#================================================#
# Packages and Components:
#------------------------------------------------#

enumerate:

	@printf "\n%s\n" "========================================"

	$(Cabal) new-build $(CabalOptions) "lib:enumerate"

	@printf "\n%s\n" "========================================"

.PHONY: enumerate

#------------------------------------------------#

enumerate-function:

	@printf "\n%s\n" "========================================"

	$(Cabal) new-build $(CabalOptions) "lib:enumerate-function"

	@printf "\n%s\n" "========================================"

.PHONY: enumerate-function

#------------------------------------------------#

doctest:

	@printf "\n%s\n" "========================================"

	$(Cabal) new-test --enable-tests $(CabalOptions) "enumerate:test:doc"

	@printf "\n%s\n" "========================================"

	$(Cabal) new-test --enable-tests $(CabalOptions) "enumerate-function:test:doc"

	@printf "\n%s\n" "========================================"

.PHONY: doctest

#================================================#
# Development:
#------------------------------------------------#

clean:

	$(Cabal) new-clean

	rm -rf "./dist" "./dist-newstyle" ./dist-* ".stack-work"
	rm -rf ./*/dist/ ./*/dist-*/

	rm -f *.project.local .ghc*.environment.*
	rm -rf TAGS ./*/TAGS

.PHONY: clean

#------------------------------------------------#

tags:

	hasktags --etags  --follow-symlinks library/  --output TAGS  --tags-absolute

.PHONY: tags

#------------------------------------------------#

documentation: build

	$(Cabal) new-haddock "all" --enable-documentation

.PHONY: documentation

#------------------------------------------------#

# TODO...
index.html: documentation

	mkdir -p  $(HaddockDirectory)

	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/$(Package)/doc/html/$(PackageName)/src/* $(HaddockDirectory)

#------------------------------------------------#

print-platform:
	@$(Ghci) -e ":unset +t" -e "putStrLn System.Info.arch" -e "putStrLn System.Info.os" 2>/dev/null

# e.g.
#
# $ make print-platform
#
#   x86_64
#   linux

.PHONY: print-platform

#================================================#
# Release...

sdist:

	$(Cabal) new-sdist all

.PHONY: sdist

#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#