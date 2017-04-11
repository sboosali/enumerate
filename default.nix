{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let

h = nixpkgs.pkgs.haskell.packages.${compiler};

ps = {
 spiros = h.callPackage ~/spiros {}; # absolute
};

in

h.callPackage ./package.nix ps