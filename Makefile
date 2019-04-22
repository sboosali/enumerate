#------------------------------------------------#
# README ----------------------------------------#
#------------------------------------------------#

# Targets:
#
# • Standard targets — « make {build,check,dist} ».
#

#------------------------------------------------#
# Makefile Settings -----------------------------#
#------------------------------------------------#

SHELL=bash

.EXPORT_ALL_VARIABLES:

#------------------------------------------------#
# Makefile Variables: ---------------------------#
#------------------------------------------------#

Version=0.3.2

#------------------------------------------------#

Project ?=enumerate
Package ?=enumerate

#------------------------------------------------#
# Makefile Variables: Haskell -------------------#
#------------------------------------------------#

CabalTargets ?=all
CabalTarget  ?=lib:$(Package)
CabalProgram ?=exe:$(Program)

#------------------------------------------------#

CabalProject ?=./cabal.project

#------------------------------------------------#
# Makefile Variables: Programs ------------------#
#------------------------------------------------#

Cabal ?=cabal
Ghc   ?=ghc

#------------------------------------------------#

Open ?=xdg-open
Nix  ?=nix

#------------------------------------------------#
# Makefile Variables: Paths ---------------------#
#------------------------------------------------#

BuildDirectory   ?=./dist-newstyle
DataDirectory    ?=./data

#------------------------------------------------#

ShareDirectory      ?=./gitignored/share
HaddockDirectory    ?=$(ShareDirectory)/doc

#------------------------------------------------#
# Makefile Variables: Options -------------------#
#------------------------------------------------#

CabalOptions=--project-file $(CabalProject) --builddir $(BuildDirectory)

#------------------------------------------------#

HackageUser     =sboo
HackagePassword =pass hackage.haskell.org/user/$(HackageUser)

#------------------------------------------------#

GitHubOwner      =sboosali
GitHubRepository =$(Project)

#------------------------------------------------#
# Makefile Variables: Subcommands ---------------#
#------------------------------------------------#


#------------------------------------------------#
# Makefile Variables: Subcommands ---------------#
#------------------------------------------------#

CabalBuild   ?=$(Cabal) new-build $(CabalOptions)
CabalRun     ?=$(Cabal) new-run $(CabalOptions)

CabalTest    ?=$(Cabal) new-test --enable-tests $(CabalOptions)
CabalBench   ?=$(Cabal) new-bench --enable-benchmarks $(CabalOptions)
CabalDocs    ?=$(Cabal) new-haddock --enable-documentation $(CabalOptions)

CabalInstall ?=$(Cabal) new-install -v --overwrite-policy=always
CabalDist    ?=$(Cabal) new-sdist -v

#------------------------------------------------#
# Makefile Variables: Environment Variables -----#
#------------------------------------------------#

LC_ALL=C.UTF-8

#------------------------------------------------#
# Makefile Targets: Standard --------------------#
#------------------------------------------------#

build:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Building..."

	$(CabalBuild) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: build

#------------------------------------------------#

check:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Testing..."

	$(CabalTest) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: check

#------------------------------------------------#

install:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Installing..."

	$(CabalInstall) $(CabalOptions) $(CabalProgram)

	@printf "\n%s\n" "========================================"

.PHONY: install

#------------------------------------------------#

dist:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Archiving..."

	$(CabalDist) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: dist

#------------------------------------------------#

all: enumerate enumerate-function check dist js 7.10 8.6

.PHONY: all

#------------------------------------------------#
# Makefile Targets: Components ------------------#
#------------------------------------------------#

enumerate:

	@printf "\n%s\n" "========================================"

	$(CabalBuild) "lib:enumerate"

	@printf "\n%s\n" "========================================"

.PHONY: enumerate

#------------------------------------------------#

enumerate-function:

	@printf "\n%s\n" "========================================"

	$(CabalBuild) "lib:enumerate-function"

	@printf "\n%s\n" "========================================"

.PHONY: enumerate-function


#------------------------------------------------#

docs:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Building Documentation..."

	$(CabalDocs) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: docs

#------------------------------------------------#

bench:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Benchmarking..."

	$(CabalBench) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: bench

#------------------------------------------------#

checkdocs:

	@printf "\n%s\n" "========================================"
	@printf "%s\n\n" "Checking Documentation..."

	$(CabalTest) "enumerate:test:doc"

	@printf "\n%s\n" "========================================"

	$(CabalTest) "enumerate-function:test:doc"

	@printf "\n%s\n" "========================================"

	$(CabalDocs) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: checkdocs

#------------------------------------------------#

check-enumerate:

	@printf "\n%s\n" "========================================"

	$(CabalTest) "enumerate:test:doc"

	@printf "\n%s\n" "========================================"

.PHONY: check-enumerate

#------------------------------------------------#

check-enumerate-function:

	@printf "\n%s\n" "========================================"

	$(CabalTest) "enumerate-function:test:doc"

	@printf "\n%s\n" "========================================"

.PHONY: check-enumerate-function

#	$(CabalTest) "enumerate-function:test:doc" "enumerate-function:test:unit" "enumerate-function:test:golden"

#------------------------------------------------#
# Haskell Compilers -----------------------------#
#------------------------------------------------#

js:

	$(CabalBuild)  $(CabalTargets)  --project-file "./ghcjs.project"

.PHONY: js

#------------------------------------------------#

7.10:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-7.10.3

.PHONY: 7.10

#------------------------------------------------#

8.0:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.0.2

.PHONY: 8.0

#------------------------------------------------#

8.2:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.2.2

.PHONY: 8.2

#------------------------------------------------#

8.4:

	$(CabalBuild)  $(CabalTargets)  -with-compiler ghc-8.4.4

.PHONY: 8.4

#------------------------------------------------#

8.6:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.6.4

.PHONY: 8.6

#------------------------------------------------#

8.8:

	$(CabalBuild)  $(CabalTargets)  --with-compiler ghc-8.8.0

.PHONY: 8.8

#------------------------------------------------#
# Git -------------------------------------------#
#------------------------------------------------#

git-tag:

	git tag -a "$(Version)" -m "$(Version)"
	git push --tags origin master

.PHONY: git-tag

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

update:

	$(Cabal) new-update "--project-file=./cabal.project"
	$(Cabal) new-update "--project-file=./ghcjs.project"
#	$(Cabal) new-update "--project-file=./static.project"

.PHONY: update

#------------------------------------------------#

# TODO...
index.html: documentation

	mkdir -p  $(HaddockDirectory)

	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/$(Package)/doc/html/$(PackageName)/src/* $(HaddockDirectory)

#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#