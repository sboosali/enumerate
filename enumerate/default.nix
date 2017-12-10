{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: # , localPackages ? {} }:
let

h = nixpkgs.pkgs.haskell.packages.${compiler};

in

h.callPackage ./package.nix {} # localPackages

# nix-build --arg '{spiros = ??? ;}'
