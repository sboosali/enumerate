#!/bin/bash

COMMAND='cabal repl enumerate'
nix-shell --run "$COMMAND"

# COMMAND='cabal new-repl enumerate enumerate-function'
# nix-shell --run "$COMMAND"

