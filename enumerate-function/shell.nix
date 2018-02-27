{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, criterion, deepseq, doctest
      , enumerate, exceptions, hspec, MemoTrie, QuickCheck, semigroups
      , stdenv
      }:
      mkDerivation {
        pname = "enumerate-function";
        version = "0.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers deepseq enumerate exceptions MemoTrie semigroups
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base doctest hspec QuickCheck ];
        benchmarkHaskellDepends = [ base criterion deepseq ];
        homepage = "http://github.com/sboosali/enumerate-function#readme";
        description = "simple package for inverting functions and testing totality, via brute enumeration of the domain";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
