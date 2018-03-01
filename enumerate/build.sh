#!/bin/sh

# e.g.
#
# ./build.sh
#
# ./build.sh --argstr compiler 802
#
#
nix-shell --run "cabal build" "$@"
