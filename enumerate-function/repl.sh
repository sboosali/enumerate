#!/bin/bash

COMMAND='cabal repl enumerate-function'
nix-shell --run "$COMMAND"

